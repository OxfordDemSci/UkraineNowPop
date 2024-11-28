source("R_helpers/generic.R")
library(tidyquant)
library(future.apply)
plan(multisession)

# param
plot_show <- TRUE # show example plots
dir.create(file.path(out_dir, "covariates", "final"), showWarnings = FALSE, recursive = TRUE)

# Load data --------------------------------------------------------------
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
cov_list <- list.files(file.path(out_dir, "covariates", "interim"), pattern = "^ua", full.names = T)

cov_df <- cov_list %>%
  lapply(read_csv) %>%
  bind_rows() |>
  rename(raw = value)

# Support functions -----------------------------------------------------

compute_rollMetric <- function(df, fun, window, col_name = "") {
  fun_name <- as.character(substitute(fun))
  if (col_name == "") {
    col_name <- paste0(fun_name, "_", window, "week")
  }
  # if (fun_name == "mean") {
  #   fun <- function(x) mean(x[-window], na.rm = T) # remove current value
  # }
  print(col_name)
  var <- df |>
    group_by(covariate, i) |>
    arrange(covariate, i, t) |>
    tq_mutate(
      select = raw,
      mutate_fun = rollapply,
      width = window,
      FUN = fun,
      align = "right", # means lagging
      fill = "extend",
      col_rename = col_name
    ) |>
    ungroup() |>
    select(last_col())
  return(var)
}

# Run computation ---------------------------------------------------------
cov_df <- cov_df |>
  group_by(covariate, i) |>
  arrange(covariate, i, t)

# compute cumulative covariate -------------------------------------------
# a bit long. ~5sec
cov_df_sum <- bind_cols(
  cov_df,
  future_lapply(c(4, 12, 24), function(w) compute_rollMetric(cov_df, sum, w))
)

cov_df_sum <- cov_df_sum |>
  pivot_longer(c(contains("week"), "raw"), names_to = "sum_stat", values_to = "value")

# compute time scaling  -------------------------------------------
# a bit long. ~5sec
cov_df_mean <- bind_cols(
  cov_df,
  future_lapply(c(4, 12, 24), function(w) compute_rollMetric(cov_df, mean, w))
)

cov_df_mean <- cov_df_mean |>
  pivot_longer(contains("week"), names_to = "method", values_to = "center_std") |>
  mutate(value = raw) |>
  separate(method, into = c("drop", "time_std"), sep = "_", fill = "right") |>
  select(-raw, -drop) |>
  mutate(sum_stat = "raw")

# compute spatial scaling  -------------------------------------------
# TODO

# bind scaling together -------------------------------------------
cov_df_scaled <- bind_rows(cov_df_sum, cov_df_mean)

cov_df_scaled <- cov_df_scaled |>
  mutate(
    space_std = NA
  ) |>
  group_by(covariate, sum_stat, time_std, space_std) |>
  mutate(
    center_std = ifelse(is.na(center_std), mean(value), center_std),
    scale_std = sd(value, na.rm = T),
    value_std = (value - center_std) / scale_std
  )

cov_df_scaled <- cov_df_scaled |>
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
  distinct(covariate, sum_stat, time_std, space_std, center_std, scale_std)

write_csv(cov_df_scaled_description, file.path(out_dir, "covariates", "final", paste0(tolower(country), "_covariates_oblast_description", output_label, ".csv")))

# plot
if (plot_show == TRUE) {
  for (cov in unique(cov_df$covariate)) {
    # cov = "acled_all"
    print(cov)
    data <- cov_df_scaled |>
      filter(covariate == cov & i %in% c(5, 10, 25)) |>
      mutate(scaling = paste0(sum_stat, "_", time_scale, "_", spatial_scale))
    g <- ggplot(data, aes(x = t, y = value_std, colour = scaling)) +
      geom_point() +
      geom_path() +
      facet_grid(i_name ~ ., scales = "free_y") +
      theme_bw() +
      labs(y = cov)
    print(g)
  }
}
