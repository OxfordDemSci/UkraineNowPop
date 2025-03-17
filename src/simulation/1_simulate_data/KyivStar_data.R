library(tidyverse)
library(ggplot2)

set.seed(123)

locations <- sort(c("Kyiv", "Lviv", "Odesa", "Kharkiv")) # 4 locations
age_groups <- c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+") # 7 age groups
sex <- c("Female", "Male") # 2 categories
times <- seq(as.Date("2023-01-01"), by = "month", length.out = 12) # 12 time points


# Baseline population ----------------------------------------------------


baseline_pop <- expand.grid(
  time = as.Date("2023-01-01"),
  origin = locations, # 4 origins
  age = age_groups, # 7 age groups
  sex = sex # 2 sexes
) |>
  arrange(time, age, sex, origin)


baseline_pop$pop <- round(
  rnorm(nrow(baseline_pop), mean = 300, sd = 50)
  *
    ifelse(baseline_pop$origin == "Kyiv", 1.2,
      ifelse(baseline_pop$origin == "Lviv", 0.9,
        ifelse(baseline_pop$origin == "Odesa", 0.7, 1)
      )
    )
    *
    ifelse(baseline_pop$age %in% c("0-17", "45-54", "55-64"), 1.3, 1)
)

ggplot(
  data = baseline_pop,
  mapping = aes(
    x = ifelse(baseline_pop$sex == "Male", yes = -pop, no = pop),
    y = age, fill = sex
  )
) +
  geom_col() +
  labs(x = "Population") +
  facet_wrap(. ~ origin)



# Transition probabilities -----------------------------------------------

create_transition <- function(dim = length(locations)) {
  matrix_prob <- round(matrix(runif(dim * dim), nrow = dim, ncol = dim), 3)
  for (i in 1:dim) {
    matrix_prob[i, i] <- matrix_prob[i, i] * 10
  }
  col_sums <- colSums(matrix_prob)

  for (i in 1:dim) {
    matrix_prob[, i] <- matrix_prob[, i] / col_sums[i]
  }
  return(matrix_prob)
}


transition <- bind_rows(replicate(length(age_groups) * length(sex), create_transition() |> as_tibble(), simplify = FALSE))
colnames(transition) <- sort(locations)

transition_data <- baseline_pop |>
  rename(destination = origin) |>
  arrange(time, age, sex, destination) |>
  bind_cols(transition)

matrix_power <- function(mat, exp) {
  result <- mat
  for (i in 2:exp) {
    result <- result %*% mat
  }
  return(result)
}


pop_data <- transition_data |>
  group_by(age, sex) |>
  group_modify(~ {
    location_matrix <- as.matrix(.x[, locations, drop = FALSE])
    pop_vector <- .x$pop
    for (i in 1:length(times[-1])) { # Assuming we want pop1 to pop4; change 4 to the desired upper bound
      matrix_exp <- matrix_power(location_matrix, i)
      .x[[paste0("pop_", times[-1][i])]] <- matrix_exp %*% pop_vector |> as.vector()
    }
    return(.x)
  })

pop_data <- pop_data |>
  select(-all_of(locations)) |>
  pivot_longer(cols = starts_with("pop"), values_to = "pop", names_to = "date") |>
  rowwise() |>
  mutate(
    time = ifelse(date == "pop", time, str_remove(date, "pop_")) |> as.Date()
  ) |>
  select(-date)

totals <- pop_data |>
  group_by(age, sex, time) |>
  summarise(pop = sum(pop))

ggplot(pop_data, aes(x = time, y = pop)) +
  geom_line(aes(color = destination)) +
  facet_grid(age ~ sex) +
  geom_line(data = totals, aes(color = "total")) +
  theme_minimal() +
  abline()
