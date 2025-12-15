source("R_helpers/generic.R")
library(tidyquant)
library(future.apply)
plan(multisession)

# param
plot_show <- TRUE # show example plots
dir.create(file.path(out_dir, "covariates", "final"), showWarnings = FALSE, recursive = TRUE)
timeWindow_cumulative <- c(6, 12, 24) # in weeks ex. c(4, 12, 24)
timeWindow_standardisation <- c(6, 12, 24) # in weeks ex. c(4, 12, 24)

# Load data --------------------------------------------------------------
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
cov_list <- list.files(file.path(out_dir, "covariates", "interim"), pattern = "^ua", full.names = T)

cov_df <- cov_list %>%
  lapply(read_csv) %>%
  bind_rows() |>
  rename(raw = value)

# Support functions -----------------------------------------------------

compute_rollMetric <- function(df, fun, window, var = raw, col_name = "") {
  fun_name <- as.character(substitute(fun))

  # if (fun_name == "mean") {
  #   fun <- function(x) mean(x[-window], na.rm = T) # remove current value
  # }
  print(paste0("computing ", fun_name, " over ", window, " weeks... "))
  df <- df |>
    group_by(covariate, i) |>
    arrange(covariate, i, t) |>
    tq_mutate(
      select = var,
      mutate_fun = rollapply,
      width = window,
      FUN = fun,
      align = "right", # means lagging
      fill = "extend",
      col_rename = fun_name
    ) |>
    mutate(window = paste0(window, "week"))
  return(df)
}

# Run computation ---------------------------------------------------------
cov_df <- cov_df |>
  group_by(covariate, i) |>
  arrange(covariate, i, t)

# compute duration processing -------------------------------------------
# a bit long. ~5sec
cov_df_sum <- bind_rows(
  future_lapply(timeWindow_cumulative, function(w) compute_rollMetric(cov_df, sum, w))
)


cov_df_sum <- cov_df_sum |>
  mutate(sum_stat = paste("sum", window, sep = "_")) |>
  rename(value = sum) |>
  select(-raw, -window)


cov_df_sum <- cov_df_sum |>
  group_by(covariate, sum_stat, t) |>
  mutate(
    space_std = "country",
    center_std = mean(value, na.rm = T),
    scale_std = sd(value, na.rm = T),
  )

# compute temporal outliers -------------------------------------------


cov_df_mean <- bind_rows(
  future_lapply(timeWindow_standardisation, function(w) compute_rollMetric(cov_df, mean, w))
) |>
  rename(center_std = mean, time_std = window)

cov_df_sd <- bind_rows(
  future_lapply(timeWindow_standardisation, function(w) compute_rollMetric(cov_df, sd, w))
) |>
  rename(scale_std = sd, time_std = window)

cov_df_time <- full_join(cov_df_mean, cov_df_sd) |>
  rename(value = raw) |>
  mutate(sum_stat = "raw")


# compute spatial outliers -----------------------------------------------

cov_df_space <- cov_df |>
  rename(value = raw) |>
  group_by(covariate, t) |>
  mutate(
    space_std = "country",
    sum_stat = "raw",
    center_std = mean(value, na.rm = T),
    scale_std = sd(value, na.rm = T)
  )


# extreme outliers -------------------------------------------------------

cov_df_extreme <- cov_df |>
  rename(value = raw) |>
  group_by(covariate) |>
  mutate(
    space_std = "country",
    sum_stat = "raw",
    time_std = "all",
    center_std = mean(value, na.rm = T),
    scale_std = sd(value, na.rm = T)
  )


# bind scaling together -------------------------------------------


cov_df_scaled <- bind_rows(cov_df_sum, cov_df_time, cov_df_space, cov_df_extreme) |>
  group_by(covariate, sum_stat, time_std, space_std) |>
  mutate(
    scale_std = ifelse(scale_std == 0, NA, scale_std),
    scale_std = ifelse(is.na(scale_std), min(scale_std, na.rm = T), scale_std)
  ) |>
  ungroup() |>
  mutate(
    value_std = (value - center_std) / scale_std
  ) |>
  select(
    i, t, starts_with("i"), ADM1_PCODE, starts_with("t_"),
    covariate, sum_stat, time_std, space_std, center_std, scale_std,
    value, value_std
  )

cov_df_scaled |>
  group_by(covariate, sum_stat, time_std, space_std) |>
  summarise(mean(value_std), sd(value_std), sd(value / scale_std)) |>
  View()

write_csv(cov_df_scaled, file.path(out_dir, "covariates", "final", paste0(tolower(country), "_covariates_oblast", output_label, ".csv")))

# write description
cov_df_scaled_description <- cov_df_scaled |>
  distinct(covariate, sum_stat, time_std, space_std)

write_csv(cov_df_scaled_description, file.path(out_dir, "covariates", "final", paste0(tolower(country), "_covariates_oblast_description", output_label, ".csv")))

# plot
if (plot_show == TRUE) {
  for (cov in unique(cov_df$covariate)) {
    # cov = "acled_all"
    print(cov)
    data <- cov_df_scaled |>
      filter(covariate == cov & i %in% c(5, 10, 25)) |>
      mutate(scaling = paste0(sum_stat, "_", time_std, "_", space_std))
    g <- ggplot(data, aes(x = t, y = value_std, colour = scaling)) +
      geom_point() +
      geom_path() +
      facet_grid(i_name ~ ., scales = "free_y") +
      theme_bw() +
      labs(y = cov)
    print(g)
  }
}
