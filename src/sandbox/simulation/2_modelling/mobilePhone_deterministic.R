library(tidyverse)
library(ggplot2)
library(plotly)

source(file.path(here::here(), "R_helpers/generic.R"))
data_dir <- file.path(out_dir, "simulation", "mobilePhone")


stocksRaw <- read_csv(file.path(data_dir, paste0("mobilePhone_stocks_observed.csv")))
true_pop <- read_csv(file.path(data_dir, paste0("mobilePhone_stocks.csv")))
flowsRaw <- read_csv(file.path(data_dir, paste0("mobilePhone_flows_observed.csv")))
baselineFlowsRaw <- read_csv(file.path(data_dir, paste0("mobilePhone_baselineFlows_observed.csv")))

# for visualisation

stocksRaw <- stocksRaw |>
  mutate(
    macroregion = "Centre"
  )

flowsRaw <- flowsRaw |>
  mutate(
    macroregion = "Centre"
  )

dir.create(file.path(data_dir, "pic", "penetrationRate"), showWarnings = F, recursive = T)
dir.create(file.path(data_dir, "pic", "stocksRaw"), showWarnings = F, recursive = T)
dir.create(file.path(data_dir, "pic", "stocksEst"), showWarnings = F, recursive = T)
dir.create(file.path(data_dir, "pic", "flowsRaw"), showWarnings = F, recursive = T)
dir.create(file.path(data_dir, "pic", "flowsEst"), showWarnings = F, recursive = T)


# 0. Visualise raw users count --------------------------------------------
stocksRaw_oblast <- stocksRaw |>
  group_by(time, macroregion, destination) |>
  summarise(users = sum(users))

# Total by oblast
for (macroregion in unique(stocksRaw$macroregion)) {
  print(macroregion)
  ggsave_(
    ggplot(stocksRaw_oblast |> filter(macroregion == macroregion), aes(y = users, x = time)) +
      geom_line() +
      theme_minimal() +
      facet_grid(destination ~ .) +
      labs(title = paste("Raw user count for", macroregion)),
    filename = file.path(data_dir, "pic", "stocksRaw", paste0("stocksRaw_", macroregion, ".png"))
  )
}

# Totals by oblast, age and sex
for (macroregion in unique(stocksRaw$macroregion)) {
  print(macroregion)
  ggsave_(
    ggplot(stocksRaw |> filter(macroregion == macroregion), aes(y = users, x = time, color = age)) +
      geom_line() +
      theme_minimal() +
      facet_grid(destination ~ sex) +
      labs(title = paste("Raw user count for", macroregion)),
    filename = file.path(data_dir, "pic", "stocksRaw", paste0("stocksRaw_agesex_", macroregion, ".png"))
  )
}

# 0.2 Visualise raw flows count --------------------------------------------
flowsRaw_oblast <- flowsRaw |>
  group_by(time, macroregion, origin, destination) |>
  summarise(users = sum(users))

gg_flowsRaw_oblast <- ggplot(flowsRaw_oblast, aes(x = time, y = users)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(origin ~ destination, scales = "free", labeller = labeller(.multi_line = FALSE)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "DESTINATION", breaks = NULL, labels = NULL)) +
  scale_x_date(sec.axis = sec_axis(~., name = "ORIGIN", breaks = NULL, labels = NULL))
gg_flowsRaw_oblast
ggsave(gg_flowsRaw_oblast, filename = file.path(data_dir, "pic", "flowsRaw", "flowsRaw.png"), width = 10, height = 10)

gg_flowsRaw_agesex <- ggplot(flowsRaw, aes(x = time, y = users, color = age)) +
  geom_line(aes(linetype = sex)) +
  theme_minimal() +
  facet_wrap(origin ~ destination, scales = "free", labeller = labeller(.multi_line = FALSE)) +
  labs(title = "Raw flow count") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "DESTINATION", breaks = NULL, labels = NULL)) +
  scale_x_date(sec.axis = sec_axis(~., name = "ORIGIN", breaks = NULL, labels = NULL))
gg_flowsRaw_agesex
ggsave(gg_flowsRaw_agesex, filename = file.path(data_dir, "pic", "flowsRaw", "flowsRaw_agesex.png"), width = 10, height = 10)


# Map origin flows by oblast
for (destination_i in unique(flowsRaw$destination)) {
  gg_flowsRaw_agesex_i <- ggplot(flowsRaw |> filter(destination == destination_i), aes(x = time, y = users, color = age)) +
    geom_line(aes(linetype = sex)) +
    theme_minimal() +
    facet_wrap(origin ~ ., labeller = labeller(.multi_line = FALSE)) +
    labs(title = paste("Raw flow count to", destination_i, "from:"))
  ggsave(gg_flowsRaw_agesex_i, filename = file.path(data_dir, "pic", "flowsRaw", paste0("flowsRaw_agesex_to", destination_i, ".png")), width = 10, height = 10)
}

# Map destination flows by oblast
for (origin_i in unique(flowsRaw$origin)) {
  gg_flowsRaw_agesex_i <- ggplot(flowsRaw |> filter(origin == origin_i), aes(x = time, y = users, color = age)) +
    geom_line(aes(linetype = sex)) +
    theme_minimal() +
    facet_wrap(destination ~ ., labeller = labeller(.multi_line = FALSE)) +
    labs(title = paste("Raw flow count from", origin_i, "to:"))
  ggsave(gg_flowsRaw_agesex_i, filename = file.path(data_dir, "pic", "flowsRaw", paste0("flowsRaw_agesex_from", origin_i, ".png")), width = 10, height = 10)
}

# 1. Model ------------------------------------------------------------------



# 1.1 compute penetration rate -----------------------------------------------

day1_pop <- true_pop |>
  filter(time == min(time) & destination != "Abroad") |>
  left_join(stocksRaw) |>
  mutate(
    p0 = pop / users,
    macroregion = "Centre"
  )

# 1.2 apply penetration rate -------------------------------------------------

stocks <- stocksRaw |>
  filter(destination != "Abroad") |>
  left_join(day1_pop |> select(-time, -users)) |>
  mutate(stock_hat = users * p0)



# 1.3 [optional] recalibrate to national totals --------------------------

stocks <- stocks |>
  group_by(time, age, sex) |>
  mutate(
    stock_pred = stock_hat / sum(stock_hat) * sum(pop)
  )



# 1.4 calibrate flows ----------------------------------------------------
# We apply calibration on the in-flows. Therefore the flows having Abroad as destination can not be estimated

flows <- flowsRaw |>
  left_join(stocks |> select(time, age, sex, macroregion, destination, stock_hat, stock_pred) |>
    rename(stockDestination_hat = stock_hat, stockDestination_pred = stock_pred)) |>
  left_join(stocks |> select(time, age, sex, macroregion, destination, stock_hat, stock_pred) |>
    mutate(time = lag(time)) |>
    rename(stockOrigin_hat = stock_hat, stockOrigin_pred = stock_pred, origin = destination)) |>
  group_by(time, age, sex, destination) |>
  mutate(
    flowsIn_hat = users / sum(users) * stockDestination_hat,
    flowsIn_pred = users / sum(users) * stockDestination_pred,
    flowsOut_hat = users / sum(users) * stockOrigin_hat,
    flowsOut_pred = users / sum(users) * stockOrigin_pred
  )



# 1.5 calibrate baseline flows -------------------------------------------

baselineFlows <- baselineFlowsRaw |>
  left_join(stocks |> select(time, age, sex, macroregion, destination, stock_hat, stock_pred)) |>
  group_by(time, age, sex, destination) |>
  mutate(
    baselineFlows_hat = users / sum(users) * stock_hat,
    baselineFlows_pred = users / sum(users) * stock_pred
  )

# Assessment -------------------------------------------------------------


# 2.1 Evaluate penetration rate -------------------------------------------

# National by age and sex
day1_pop_age_sex <- day1_pop |>
  group_by(age, sex) |>
  summarise(across(c(pop, users), sum)) |>
  mutate(p0 = pop / users)

gg_penRate_age_sex <- ggplot(day1_pop_age_sex, aes(y = age, x = p0)) +
  geom_col(fill = "grey40") +
  facet_grid(sex ~ .) +
  lims(x = c(0, 2)) +
  geom_vline(xintercept = 1, color = "orange", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Penetration rate by age and sex", x = "Pop/Users at t0")

ggsave_(gg_penRate_age_sex, filename = file.path(data_dir, "pic/penetrationRate/penetrationRate_agesex.png"))

# Per oblast
day1_pop_oblast <- day1_pop |>
  group_by(destination) |>
  summarise(across(c(pop, users), sum)) |>
  mutate(p0 = pop / users)

for (macroregion in unique(stocks$macroregion)) {
  print(macroregion)
  ggsave_(
    ggplot(day1_pop_oblast |> filter(macroregion == macroregion), aes(y = destination, x = p0)) +
      geom_col(fill = "grey40") +
      lims(x = c(0, 5)) +
      geom_vline(xintercept = 1, color = "orange", linetype = "dashed") +
      theme_minimal() +
      labs(title = paste("Penetration rate for", macroregion), x = "Pop/Users at t0"),
    filename = file.path(data_dir, "pic", "penetrationRate", paste0("penetrationRate_", macroregion, ".png"))
  )
}

# Per oblast by age and sex
for (macroregion in unique(stocks$macroregion)) {
  print(macroregion)
  ggsave_(
    ggplot(day1_pop |> filter(macroregion == macroregion), aes(y = age, x = p0)) +
      geom_col(fill = "grey40") +
      facet_grid(sex ~ destination) +
      geom_vline(xintercept = 1, color = "orange", linetype = "dashed") +
      theme_minimal() +
      labs(title = paste("Penetration rate for", macroregion), x = "Pop/Users at t0"),
    filename = file.path(data_dir, "pic", "penetrationRate", paste0("penetrationRate_agesex_", macroregion, ".png"))
  )
}

# 2.2 Evaluate stocks estimation ---------------------------------------------

# First assessment: how different is the total estimates when not calibrated?
# Note: See the importance of abroad on the difference
stocks_national <- stocks |>
  group_by(time) |>
  summarise(
    users = sum(users),
    stock_hat = sum(stock_hat),
    stock_hat_perc = (sum(stock_hat) - sum(day1_pop$pop)) / sum(day1_pop$pop) * 100
  )

type_label <- c(
  `stock_hat` = "Total",
  `stock_hat_perc` = "Percentage of change compared to baseline (%)"
)

gg_stocks_national <- ggplot(stocks_national |> pivot_longer(c(stock_hat, stock_hat_perc), names_to = "type"), aes(x = time, y = value)) +
  geom_line() +
  theme_minimal() +
  geom_hline(data = tibble(type = c("stock_hat", "stock_hat_perc"), value = c(sum(day1_pop$pop), 0)), aes(yintercept = value), color = "red", linetype = "dashed") +
  labs(title = "Total population stocks: National level", ) +
  facet_wrap(~type, scales = "free_y", labeller = labeller(type = type_label))
gg_stocks_national

ggsave_(gg_stocks_national, filename = file.path(data_dir, "pic/stocksEst/stocks_national.png"))


# Second assessment: how different is the total estimates by age and sex when not calibrated?
stocks_age_sex <- stocks |>
  group_by(time, age, sex) |>
  summarise(stock_hat = sum(stock_hat)) |>
  ungroup() |>
  left_join(day1_pop |> group_by(age, sex) |> summarise(pop = sum(pop))) |>
  mutate(
    age_sex = paste(age, sex),
    diff = stock_hat - pop
  )

gg_stocks_agesex <- ggplot(stocks_age_sex, aes(x = time, y = diff, color = age)) +
  geom_line() +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "grey20", linetype = "dashed") +
  facet_wrap(~sex) +
  labs(title = "Total population stocks: Age and sex differences", y = "stock_hat - pop")
gg_stocks_agesex
ggsave_(gg_stocks_agesex, filename = file.path(data_dir, "pic/stocksEst/stocks_agesex.png"))


# Visualisation of oblast-level stocks estimation

# Totals per oblast
gg_stocks_oblast <- stocks |>
  group_by(time, destination) |>
  summarise(stock_hat = sum(stock_hat)) |>
  ggplot(aes(x = time, y = stock_hat)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~destination) +
  labs(title = "Total population stocks: Oblast level")
gg_stocks_oblast

for (macroregion in unique(stocks$macroregion)) {
  print(macroregion)
  gg_stocks_oblast <- stocks |>
    filter(macroregion == macroregion) |>
    group_by(time, destination) |>
    summarise(stock_hat = sum(stock_hat)) |>
    ggplot(aes(x = time, y = stock_hat)) +
    geom_line() +
    theme_minimal() +
    facet_wrap(~destination) +
    labs(title = paste("Total population stocks: Oblast level for", macroregion))
  ggsave_(gg_stocks_oblast, filename = file.path(data_dir, "pic/stocksEst", paste0("stocks_", macroregion, ".png")))
}

# Total by age and sex
for (macroregion in unique(stocks$macroregion)) {
  print(macroregion)
  gg_stocks_agesex <- stocks |>
    filter(macroregion == macroregion) |>
    ggplot(aes(x = time, y = stock_hat, color = age)) +
    geom_line() +
    theme_minimal() +
    facet_grid(destination ~ sex) +
    labs(title = paste("Total population stocks: Oblast level for", macroregion))
  print(gg_stocks_agesex)
  ggsave_(gg_stocks_agesex, filename = file.path(data_dir, "pic/stocksEst", paste0("stocks_agesex_", macroregion, ".png")))
}

# Visualisation: how different are estimated flows from raw flows?

flows_comp <- flows |>
  select(time, destination, origin, users, age, sex, flows_hat) |>
  pivot_longer(c(users, flows_hat), names_to = "source")


flows_comp |>
  group_by(time, destination, origin, source) |>
  summarise(value = sum(value)) |>
  ggplot(aes(x = time, y = value, color = source)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(origin ~ destination, scales = "free", labeller = labeller(.multi_line = FALSE)) +
  labs(title = "Total flow stocks: Comparison of estimated and raw flows")
