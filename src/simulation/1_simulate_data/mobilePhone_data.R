# Five files are created in 'out_dir' for output data, specifically:
# 1. "mobilePhone_pop_observed.csv"   - The observed population data.
# 2. "mobilePhone_flows_observed.csv" - The observed flows data.
# 3. "mobilePhone_baselineFlows_observed.csv"   - The observed baseline flows data.
# 4. "mobilePhone_flows.csv"          - The true flow data.
# 3. "mobilePhone_baselineFlows.csv"  - The true baseline flows data.
# 5. "mobilePhone_pop.csv"            - The simulated population data.
# 6. "mobilePhone_transitions.csv"    - The transition data for the simulation.

# Steps to simulate the true population:
# 1. Population by age and sex at time t0 is calculated with a Poisson(population_total * location_share * age_share * sex_share)
# 2. Transition probabilities are applied based on the population at time t0.
# 3. The flow between time t and t+1 is calculated based on the population and transitions and stored as flows at time t
# 4. The population at time t+1 is calculated as the sum of the destination flows.
# 5. The baseline flows is calculated as the matrix multiplication of all transtition probabilities between t0 and t and the population at time t0

# Steps to simulate mobilePhone data:
# 1. Simulate the population detection probability by location, age and sex using a uniform distribution
# 2. The observed population is estimated with a binomial distribution.
# 3. Simulate the flow detection probability by origin, destination, age and sex using a uniform distribution
# 4. The observed flows is estimated with a binomial distribution.
# 5. Reiterate steps 3-4 for the baseline flows.


library(tidyverse)
library(ggplot2)

set.seed(1789)

locations <- sort(c("Kyiv", "Lviv", "Odesa", "Kharkiv", "Abroad")) # 5 locations
ages <- c("20-29", "30-39", "40-49", "50-59", "60+") # 7 age groups
sexes <- c("Female", "Male") # 2 categories
times <- seq(as.Date("2023-01-01"), by = "month", length.out = 12) |> as.character() # 12 time points
total_population_size <- 100000

check <- F # print check on the console

# output directory
source(file.path(here::here(), "R_helpers/generic.R"))
out_dir <- file.path(out_dir, "simulation", "mobilePhone")
dir.create(out_dir, showWarnings = F, recursive = T)



#  True process model ----------------------------------------------------------

## Baseline population ----------------------------------------------------

baseline_pop <- array(0,
  dim = c(length(ages), length(sexes), length(locations)),
  dimnames = list(age = ages, sex = sexes, origin = locations)
)

location_shares <- c(0.05, 0.20, 0.35, 0.2, 0.2)
sex_shares <- c(0.5, 0.5)
age_shares <- c(0.16, 0.19, 0.19, 0.21, 0.25)


for (i in 1:length(locations)) {
  for (s in 1:length(sexes)) {
    for (a in 1:length(ages)) {
      baseline_pop[a, s, i] <- rpois(1, total_population_size * location_shares[i] * sex_shares[s] * age_shares[a])
    }
  }
}

print(paste("Total population size:", sum(baseline_pop)))

if (check) {
  apply(baseline_pop, 3, sum)
  apply(baseline_pop, 1, sum)
  apply(baseline_pop, 2, sum)
  apply(baseline_pop, c(1, 2), sum)
}

ggplot(
  data = as_tibble(as.table(baseline_pop)) %>%
    rename(pop = n),
  mapping = aes(
    x = ifelse(sex == "Male", yes = -pop, no = pop),
    y = age, fill = sex
  )
) +
  geom_col() +
  labs(x = "Population") +
  facet_wrap(. ~ origin)

## Transition probabilities -----------------------------------------------

migration_rate <- array(NA,
  dim = c(length(ages), length(sexes), length(locations), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, origin = locations, time = times)
)


# Draw migration rates from a normal distribution, but adjust for:
# - more stayers
# - more out-migration from Kharkiv
# - even more out-migration from Kharkiv for females
# - less out-migration to abroad
# - no out-migration to abroad for males
# - rate constant through time

for (age_group in ages) {
  for (sex_group in sexes) {
    for (destination in locations) {
      for (origin in locations) {
        # If origin equals destination, we increase the migration rate (ie stayers)
        if (origin == destination) {
          migration_rate[age_group, sex_group, destination, origin, ] <- rnorm(1, mean = 0.5, sd = 0.05)
        } else if (destination == "Abroad") {
          migration_rate[age_group, sex_group, destination, origin, ] <- abs(rnorm(1, mean = 0.05, sd = 0.01))
        } else if (destination == "Abroad" & sex_group == "Male") {
          migration_rate[age_group, sex_group, destination, origin, ] <- 0
        } else if (origin == "Kharkiv") {
          # If the origin is Kharkiv, we want to increase the out-migration rate
          migration_rate[age_group, sex_group, destination, origin, ] <- rnorm(1, mean = 0.4, sd = 0.05)
        } else if (origin == "Kharkiv" & sex_group == "Female") {
          # If the origin is Kharkiv, we want to increase the out-migration rate
          migration_rate[age_group, sex_group, destination, origin, ] <- rnorm(1, mean = 0.3, sd = 0.05)
        } else {
          # For all other origin-destination pairs, use a normal distribution with a lower mean
          migration_rate[age_group, sex_group, destination, origin, ] <- rnorm(1, mean = 0.15, sd = 0.05)
        }
      }
    }
  }
}


# Normalize migration rates to ensure they sum to 1 for each origin
for (t in times) {
  for (origin in locations) {
    for (age_group in ages) {
      for (sex_group in sexes) {
        # Normalize migration rates for each origin-age-sex group at time t
        rates <- migration_rate[age_group, sex_group, , origin, t]
        migration_rate[age_group, sex_group, , origin, t] <-
          rates / sum(rates) # Normalize so that they sum to 1
      }
    }
  }
}


## Generate stocks and flows through time ---------------------------------------

population_stocks <- array(0,
  dim = c(length(ages), length(sexes), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, time = times)
)

population_flows <- array(0,
  dim = c(length(ages), length(sexes), length(locations), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, origin = locations, time = times)
)

population_flows_baseline <- array(0,
  dim = c(length(ages), length(sexes), length(locations), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, origin = locations, time = times)
)

population_stocks[, , , 1] <- baseline_pop

for (t in 1:(length(times) - 1)) {
  for (age_group in ages) {
    for (sex_group in sexes) {
      # Population flows from t to t+1 are assigned to t
      population_flows[age_group, sex_group, , , times[t]] <- t(t(migration_rate[age_group, sex_group, , , times[t]]) * population_stocks[age_group, sex_group, , times[t]])
      # Population stocks at t+1 are the sum of population flows between t and t+1
      population_stocks[age_group, sex_group, , times[t + 1]] <- apply(population_flows[age_group, sex_group, , , times[t]], 1, sum)

      migration_rate_temp <- array(NA,
        dim = c(length(locations), length(locations)),
        dimnames = list(destination = locations, origin = locations)
      )

      migration_rate_temp <- migration_rate[age_group, sex_group, , , 1]

      if (t > 1) {
        for (t_idx in 2:t) {
          migration_rate_temp <- migration_rate[age_group, sex_group, , , t_idx] %*% migration_rate_temp
        }
      }

      population_flows_baseline[age_group, sex_group, , , times[t]] <- t(t(migration_rate_temp) * population_stocks[age_group, sex_group, , 1])
    }
  }
}

# Convert to long format
migration_rate_df <- as_tibble(as.table(migration_rate)) |>
  rename(prob = n)

population_stocks_df <- as_tibble(as.table(population_stocks)) |>
  rename(pop = n) |>
  mutate(time = as.Date(time))

population_flows_df <- as_tibble(as.table(population_flows)) |>
  filter(time != max(times)) |>
  rename(pop = n) |>
  mutate(time = as.Date(time))

population_flowsBaseline_df <- as_tibble(as.table(population_flows_baseline)) |>
  filter(time != max(times)) |>
  rename(pop = n) |>
  mutate(time = as.Date(time))

# True population plots
ggplot(population_stocks_df, aes(x = time, y = pop, color = age, linetype = sex)) +
  geom_line() +
  theme_minimal() +
  facet_grid(destination ~ .) +
  labs(title = "True population stocks")

ggplot(population_flows_df, aes(x = time, y = pop, color = age, linetype = sex)) +
  geom_line() +
  theme_minimal() +
  facet_grid(origin ~ destination) +
  labs(title = "True population flows")

ggplot(population_flowsBaseline_df, aes(x = time, y = pop, color = age, linetype = sex)) +
  geom_line() +
  theme_minimal() +
  facet_grid(origin ~ destination) +
  labs(title = "True population baseline flows")


if (check) {
  apply(population_stocks, 4, sum)
  apply(population_stocks, c(1, 4), sum)
  apply(population_stocks, c(3, 4), sum)

  apply(population_flows, c(1, 5), sum)

  apply(population_flows_baseline, c(1, 5), sum)

  apply(population_flows_baseline[age_group, sex_group, , , 9], 1, sum)
  population_stocks[age_group, sex_group, , 10]

  apply(population_flows_baseline[age_group, sex_group, , , 9], 2, sum)
  population_stocks[age_group, sex_group, , 1]
}

# Observation model ------------------------------------------------------

# Population detection probability
# varying by age and sex
# lower in the East
# constant through time


population_detection <- array(NA,
  dim = c(length(ages), length(sexes), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, time = times)
)

population_observed <- array(NA,
  dim = c(length(ages), length(sexes), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, time = times)
)

for (age_group in ages) {
  for (sex_group in sexes) {
    for (destination in locations) {
      # If origin is in East lower detection probability
      if (destination == "Abroad") {
        population_detection[age_group, sex_group, destination, ] <- rep(runif(1, 0.4, 0.5), length(times))
      } else if (destination == "Kharkiv") {
        population_detection[age_group, sex_group, destination, ] <- rep(runif(1, 0.2, 0.3), length(times))
      } else {
        # For all other origin-destination pairs, use a normal distribution with a lower mean
        population_detection[age_group, sex_group, destination, ] <- rep(runif(1, 0.7, 0.8), length(times))
      }
    }
  }
}

# Observed population

for (t in times) {
  for (age_group in ages) {
    for (sex_group in sexes) {
      for (destination in locations) {
        # Add noise to population stocks
        population_observed[age_group, sex_group, destination, t] <- rbinom(1, as.integer(population_stocks[age_group, sex_group, destination, t]), population_detection[age_group, sex_group, destination, t])
      }
    }
  }
}

population_observed_df <- bind_rows(
  as_tibble(as.table(population_observed)) |>
    rename(pop = n) |>
    mutate(
      time = as.Date(time),
      source = "observed population"
    ),
  population_stocks_df |>
    mutate(source = "true population")
)

ggplot(population_observed_df, aes(x = time, y = pop, color = age, linetype = source)) +
  geom_line() +
  theme_minimal() +
  facet_grid(destination ~ sex)


# Flows detection probability
# varying by age and sex
# lower in the East
# constant through time


flows_detection <- array(NA,
  dim = c(length(ages), length(sexes), length(locations), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, origin = locations, time = times)
)

flows_observed <- array(NA,
  dim = c(length(ages), length(sexes), length(locations), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, origin = locations, time = times)
)

for (age_group in ages) {
  for (sex_group in sexes) {
    for (destination in locations) {
      for (origin in locations) {
        # If flows are from/to East or from/to Abroad lowest detection probability
        if (destination == "Abroad" | origin == "Abroad") {
          flows_detection[age_group, sex_group, destination, origin, ] <- rep(runif(1, 0.3, 0.4), length(times))
        } else if (destination == "Kharkiv" | origin == "Kharkiv") {
          flows_detection[age_group, sex_group, destination, origin, ] <- rep(runif(1, 0.1, 0.2), length(times))
        } else if (destination == origin) {
          # If origin and destination are the same, higher detection probability
          flows_detection[age_group, sex_group, destination, origin, ] <- rep(runif(1, 0.6, 0.7), length(times))
        } else {
          # For all other origin-destination pairs, lower detection probability
          flows_detection[age_group, sex_group, destination, origin, ] <- rep(runif(1, 0.5, 0.6), length(times))
        }
      }
    }
  }
}

# Observed flows

for (t in times[-length(times)]) {
  for (age_group in ages) {
    for (sex_group in sexes) {
      for (destination in locations) {
        for (origin in locations) {
          # Add noise to population flows
          flows_observed[age_group, sex_group, destination, origin, t] <- rbinom(
            1,
            as.integer(population_flows[age_group, sex_group, destination, origin, t]),
            flows_detection[age_group, sex_group, destination, origin, t]
          )
        }
      }
    }
  }
}

flows_observed_df <- as_tibble(as.table(flows_observed)) |>
  filter(time != max(times)) |>
  rename(pop = n) |>
  mutate(
    time = as.Date(time),
    source = "observed flows"
  )

flows_observed_df_plot <- flows_observed_df |>
  bind_rows(
    population_flows_df |>
      mutate(source = "true flows")
  )

ggplot(
  flows_observed_df_plot |> filter(sex == "Female"),
  aes(x = time, y = pop, color = age, linetype = source)
) +
  geom_line() +
  theme_minimal() +
  facet_grid(origin ~ destination) +
  labs(title = "Observed flows. Sex=Female")


# Baseline flows detection probability
# varying by age and sex
# lower than flows except for East
# constant through time

baselineFlows_detection <- array(NA,
  dim = c(length(ages), length(sexes), length(locations), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, origin = locations, time = times)
)

baselineFlows_observed <- array(NA,
  dim = c(length(ages), length(sexes), length(locations), length(locations), length(times)),
  dimnames = list(age = ages, sex = sexes, destination = locations, origin = locations, time = times)
)

for (age_group in ages) {
  for (sex_group in sexes) {
    for (destination in locations) {
      for (origin in locations) {
        # If flows are from/to East or from/to Abroad lowest detection probability
        if (destination == "Abroad" | origin == "Abroad") {
          baselineFlows_detection[age_group, sex_group, destination, origin, ] <- rep(runif(1, 0.3, 0.4), length(times))
        } else if (destination == "Kharkiv" | origin == "Kharkiv") {
          baselineFlows_detection[age_group, sex_group, destination, origin, ] <- rep(runif(1, 0.3, 0.4), length(times))
        } else if (destination == origin) {
          # If origin and destination are the same, higher detection probability
          baselineFlows_detection[age_group, sex_group, destination, origin, ] <- rep(runif(1, 0.5, 0.6), length(times))
        } else {
          # For all other origin-destination pairs, lower detection probability
          baselineFlows_detection[age_group, sex_group, destination, origin, ] <- rep(runif(1, 0.4, 0.5), length(times))
        }
      }
    }
  }
}

# Observed baseline flows

for (t in times[-length(times)]) {
  for (age_group in ages) {
    for (sex_group in sexes) {
      for (destination in locations) {
        for (origin in locations) {
          # Add noise to population flows
          baselineFlows_observed[age_group, sex_group, destination, origin, t] <- rbinom(
            1,
            as.integer(population_flows_baseline[age_group, sex_group, destination, origin, t]),
            baselineFlows_detection[age_group, sex_group, destination, origin, t]
          )
        }
      }
    }
  }
}

# plot
baselineFlows_observed_df <- as_tibble(as.table(baselineFlows_observed)) |>
  filter(time != max(times)) |>
  rename(pop = n) |>
  mutate(
    time = as.Date(time),
    source = "observed flows"
  )

baselineFlows_observed_df_plot <- baselineFlows_observed_df |>
  bind_rows(
    population_flowsBaseline_df |>
      mutate(source = "true flows")
  )

ggplot(
  baselineFlows_observed_df_plot |> filter(sex == "Female"),
  aes(x = time, y = pop, color = age, linetype = source)
) +
  geom_line() +
  theme_minimal() +
  facet_grid(origin ~ destination) +
  labs(title = "Observed baseline flows. Sex=Female")


# write output -----------------------------------------------------------

# Observed population
write_csv(population_observed_df, file.path(out_dir, paste0("mobilePhone_pop_observed.csv")))
write_csv(flows_observed_df, file.path(out_dir, paste0("mobilePhone_flows_observed.csv")))
write_csv(baselineFlows_observed_df, file.path(out_dir, paste0("mobilePhone_baselineFlows_observed.csv")))

# True population
write_csv(population_flows_df, file.path(out_dir, paste0("mobilePhone_flows.csv")))
write_csv(population_flowsBaseline_df, file.path(out_dir, paste0("mobilePhone_baselineFlows.csv")))
write_csv(population_stocks_df, file.path(out_dir, paste0("mobilePhone_pop.csv")))
write_csv(migration_rate_df, file.path(out_dir, paste0("mobilePhone_transitions.csv")))
