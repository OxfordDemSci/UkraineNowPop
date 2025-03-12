library(tidyverse)
library(ggplot2)

set.seed(123) 

locations <- c("Kyiv", "Lviv", "Odesa", "Kharkiv") # 4 locations
age_groups <- c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")  # 7 age groups
sex <- c("Female", "Male")   # 2 categories
time <- seq(as.Date("2023-01-01"), by = "month", length.out = 12)   #12 time points

data <- expand.grid(
  time = time,
  location = locations,
  age_group = age_groups,
  sex = sex
)

##################################
# POPULATION STOCKS
##################################
##################################
# "Ideal" dataset
##################################
# Assign a base number of subscribers, with variation by age and location
data$subscribers <- round(
  rnorm(nrow(data), mean = 500, sd = 150) *
  ifelse(data$location == "Kyiv", 1.2, ifelse(data$location == "Lviv", 0.9, ifelse(data$location == "Odesa", 0.7, 1))) *
  ifelse(data$age_group %in% c("18-24", "25-34"), 1.3, 1)
)

data$subscribers <- pmax(data$subscribers, 10)

all_age_group <- data %>%
  group_by(time, location, sex) %>%
  summarise(subscribers = sum(subscribers), .groups = 'drop') %>%
  mutate(age_group = "0-65+")

data <- bind_rows(data, all_age_group)

all_sex <- data %>%
  group_by(time, location, age_group) %>%
  summarise(subscribers = sum(subscribers), .groups = 'drop') %>%
  mutate(sex = "Both") 

data <- bind_rows(data, all_sex) %>%
  mutate(age_group = factor(age_group, 
                            levels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+", "0-65+")))

ggplot(data, aes(x = time, y = subscribers, color = age_group)) +
  geom_line() +
  facet_grid(location ~ sex) +
  theme_minimal() +
  labs(title = "Monthly active subscribers by location, age group, and sex",
       x = "Month", y = "Number of Subscribers")

ggplot(data, aes(x = time, y = subscribers, color = sex)) +
  geom_line() +
  facet_grid(location ~ age_group) +
  theme_minimal() +
  labs(title = "Monthly active subscribers by location, age group, and sex",
       x = "Month", y = "Number of Subscribers")


data <- mutate(data, time1 = format(time, "%b.%y"))
head(data)

#####################################
# Reproducing table A - Only gender
#####################################
data_sex <- data %>%
  group_by(time, location, sex) %>%
  summarise(subscribers = sum(subscribers), .groups = 'drop')

both_sex <- data_sex %>%
  group_by(time, location) %>%
  summarise(subscribers = sum(subscribers), .groups = 'drop') %>%
  mutate(sex = "Both")

table_a <- bind_rows(data_sex, both_sex) %>%
  mutate(time1 = format(time, "%b.%y"),
         age_group = "0-65+") %>%
  select(time1, location, sex, Subscribers_A = subscribers)
head(table_a)


########################################
# Reproducing table B - only age groups
########################################
data_age <- data %>%
  group_by(time, location, age_group) %>%
  summarise(subscribers = sum(subscribers), .groups = 'drop')

all_age_groups <- data_age %>%
  group_by(time, location) %>%
  summarise(subscribers = sum(subscribers), .groups = 'drop') %>%
  mutate(age_group = "0-65+")

all_age_groups$age_group <- factor(all_age_groups$age_group, 
  levels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+", "0-65+"))

table_b <- bind_rows(data_age, all_age_groups) %>%
  mutate(time1 = format(time, "%b.%y"),
         sex = "Both") %>%
  select(time1, location, age_group, Subscribers_B = subscribers, sex)
head(table_b)



##################################
# MIGRATION FLOWS
##################################
##################################
# Ideal table
##################################
flow_data <- expand.grid(
  time = time,              # 12 time steps
  origin = locations,       # 4 origins
  destination = locations,  # 4 destinations
  age_group = age_groups,   # 7 age groups
  sex = sex                 # 2 sexes
)

flow_data <- flow_data %>%
  mutate(stayer_mover = ifelse(origin == destination, "Stayer", "Mover") )

flow_data$flows <- round(
  # In non-conflict contexts, "Stayers" can be ~ 95% of the local population on average, "Movers" are the other 5%
  ifelse(flow_data$stayer_mover == "Stayer",
         rnorm(nrow(flow_data), mean = 300, sd = 50),
         rnorm(nrow(flow_data), mean = 30, sd = 10))
  *
    ifelse(flow_data$origin == "Kyiv", 1.2,
      ifelse(flow_data$origin == "Lviv", 0.9,
      ifelse(flow_data$origin == "Odesa", 0.7, 1)))
  *
    ifelse(flow_data$age_group %in% c("18-24","25-34"), 1.3, 1)
)

flow_data$flows <- pmax(flow_data$flows, 0) 


all_age_group_flows <- flow_data %>%
  group_by(time, origin, destination, sex, stayer_mover) %>%
  summarise(flows = sum(flows), .groups = 'drop') %>%
  mutate(age_group = "0-65+")

flow_data <- bind_rows(flow_data, all_age_group_flows)

all_sex_flows <- flow_data %>%
  group_by(time, origin, destination, age_group, stayer_mover) %>%
  summarise(flows = sum(flows), .groups = 'drop') %>%
  mutate(sex = "Both")

flow_data <- bind_rows(flow_data, all_sex_flows)
flow_data$age_group <- factor(flow_data$age_group,
  levels = c("0-17","18-24","25-34","35-44","45-54","55-64","65+","0-65+"))

head(flow_data)

#############################################
# Table A for migration flows - Only gender
#############################################
flow_data_sex <- flow_data %>%
  group_by(time, origin, destination, sex) %>%
  summarise(flows = sum(flows), .groups = 'drop')

both_flow_sex <- flow_data_sex %>%
  group_by(time, origin, destination) %>%
  summarise(flows = sum(flows), .groups = 'drop') %>%
  mutate(sex = "Both")
head(both_flow_sex)

table_a_flows <- bind_rows(flow_data_sex, both_flow_sex) 
head(table_a_flows)


################################################
# Table B for migration flows - Only age groups
################################################
flow_data_age <- flow_data %>%
  group_by(time, origin, destination, age_group) %>%
  summarise(flows = sum(flows), .groups = 'drop')

all_flow_age <- flow_data_age %>%
  group_by(time, origin, destination) %>%
  summarise(flows = sum(flows), .groups = 'drop') %>%
  mutate(age_group = "0-65+")

table_b_flows <- bind_rows(flow_data_age, all_flow_age) 
head(table_b_flows)


