source("R_helpers/generic.R")
library(tidyquant)

dir.create(file.path(out_dir, "covariates", "final"), showWarnings = FALSE, recursive = TRUE)

master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
cov_list <- list.files(file.path(out_dir, "covariates", "interim"), pattern = "^ua", full.names = T)

cov_df <- cov_list %>%
  lapply(read_csv) %>%
  bind_rows()

# Standardise ------------------------------------------------------------

cov_df <- cov_df |>
  group_by(covariate, i) |>
  arrange(t) |>
  # compute value cumulative with two windows: sum over 7 and 1 months
  tq_mutate(
    select = "value",
    mutate_fun = rollsum,
    k = 4,
    align = "right", # means lagging
    fill = "extend",
    col_rename = "value_sum1month"
  ) |>
  tq_mutate(
    select = "value",
    mutate_fun = rollsum,
    k = 12,
    align = "right",
    fill = "extend",
    col_rename = "value_sum3month"
  ) |>
  tq_mutate(
    select = "value",
    mutate_fun = rollsum,
    k = 24,
    align = "right",
    fill = "extend",
    col_rename = "value_sum6month"
  ) |>
  ungroup() |>
  group_by(covariate) |>
  # z-score on value and cumulative values
  mutate(
    value_std = (value - mean(value, na.rm = T)) / sd(value, na.rm = T),
    value_sum1month_std = (value_sum1month - mean(value_sum1month, na.rm = T)) / sd(value_sum1month, na.rm = T),
    value_sum3month_std = (value_sum3month - mean(value_sum3month, na.rm = T)) / sd(value_sum3month, na.rm = T),
    value_sum6month_std = (value_sum6month - mean(value_sum6month, na.rm = T)) / sd(value_sum6month, na.rm = T)
  ) |>
  ungroup() |>
  group_by(covariate, i) |>
  arrange(t) |>
  # compute value mean with two windows: mean over 1 and 3 and 6 months
  tq_mutate(
    select = "value",
    mutate_fun = rollmean,
    k = 4,
    align = "right",
    fill = "extend",
    col_rename = "value_mean1month"
  ) |>
  tq_mutate(
    select = "value",
    mutate_fun = rollmean,
    k = 12,
    align = "right",
    fill = "extend",
    col_rename = "value_mean3month"
  ) |>
  tq_mutate(
    select = "value",
    mutate_fun = rollmean,
    k = 24,
    align = "right",
    fill = "extend",
    col_rename = "value_mean6month"
  ) |>
  # compute value sd with two windows: sd over 1 and 3 and 6 months
  tq_mutate(
    select = "value",
    mutate_fun = rollapply,
    width = 4,
    FUN = sd,
    align = "right",
    fill = "extend",
    col_rename = "value_sd1month"
  ) |>
  tq_mutate(
    select = "value",
    mutate_fun = rollapply,
    width = 12,
    FUN = sd,
    align = "right",
    fill = "extend",
    col_rename = "value_sd3month"
  ) |>
  tq_mutate(
    select = "value",
    mutate_fun = rollapply,
    width = 24,
    FUN = sd,
    align = "right",
    fill = "extend",
    col_rename = "value_sd6month"
  ) |>
  ungroup() |>
  # standardise value with mean and sd in time windows
  mutate(
    value_sd1month = ifelse(!is.finite(value_sd1month), 0.001, value_sd1month),
    value_sd3month = ifelse(!is.finite(value_sd3month), 0.001, value_sd3month),
    value_sd6month = ifelse(!is.finite(value_sd6month), 0.001, value_sd6month),
    value_mean1month_ = (value - value_mean1month) / value_sd1month,
    value_mean3month_ = (value - value_mean3month) / value_sd3month,
    value_mean6month_ = (value - value_mean6month) / value_sd6month
  ) |>
  group_by(covariate) |>
  # z-score on temporal scaling
  mutate(
    value_mean1month_std = (value_mean1month_ - mean(value_mean1month_, na.rm = T)) / sd(value_mean1month_, na.rm = T),
    value_mean3month_std = (value_mean3month_ - mean(value_mean3month_, na.rm = T)) / sd(value_mean3month_, na.rm = T),
    value_mean6month_std = (value_mean6month_ - mean(value_mean6month_, na.rm = T)) / sd(value_mean6month_, na.rm = T)
  ) |>
  filter(t >= min(master_index$t) & t <= max(master_index$t))


cov_df <- cov_df |>
  ungroup() |>
  select(i, t, i_key, i_name, t_key, t_name, covariate, ADM1_PCODE, ends_with("std"))

write_csv(cov_df, file.path(out_dir, "covariates", "final", paste0(tolower(country), "_covariates_oblast", output_label, ".csv")))


# this doesnt work for me in positron, any idea?
for (cov in unique(cov_df$covariate)) {
  # cov = "acled_all"
  data <- cov_df |>
    filter(covariate == cov & i %in% c(5, 10, 25)) |>
    pivot_longer(ends_with("std"), names_to = "scaling", values_to = "value")
  g <- ggplot(data, aes(x = t, y = value, colour = scaling)) +
    geom_point() +
    geom_path() +
    facet_grid(i_name ~ ., scales = "free_y") +
    theme_bw() +
    labs(y = cov)
  print(g)
}

cov_df |>
  filter(covariate == cov) |>
  pivot_longer(ends_with("std"), names_to = "scaling", values_to = "value") |>
  group_by(scaling) |>
  summarise(mean(value, na.rm = T), sd(value, na.rm = T)) |>
  View()
