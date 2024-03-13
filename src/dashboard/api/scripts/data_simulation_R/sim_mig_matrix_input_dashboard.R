#install.packages("tidyverse")
library("tidyverse")


dummy_mig_matrix <- expand.grid(
  country = "UKR",
                                
  admin_level = "1",
      
  origin = c("UA01", "UA05", "UA07", "UA12", "UA14",
                   "UA18", "UA21", "UA23", "UA26", "UA32",
                   "UA35", "UA44", "UA46", "UA48", "UA51",
                   "UA53", "UA56", "UA59", "UA61", "UA63",
                   "UA65", "UA68", "UA71", "UA73", "UA74",
                   "UA80", "UA85"), 
  
  destination = c("UA01", "UA05", "UA07", "UA12", "UA14",
                       "UA18", "UA21", "UA23", "UA26", "UA32",
                       "UA35", "UA44", "UA46", "UA48", "UA51",
                       "UA53", "UA56", "UA59", "UA61", "UA63",
                       "UA65", "UA68", "UA71", "UA73", "UA74",
                       "UA80", "UA85"), 
  
  sex = c("male", "female"),
  
  age_group = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                 "25-29", "30-34", "35-39", "40-44", "45-49", 
                 "50-54", "55-59", "60-64", "65-69", "70-74", 
                 "75-79", "80-999")) %>%
  filter(origin!=destination)


gen_rn <- function(total_sum, num_elements) {
  random_numbers <- runif(num_elements - 1, min = 0, max = total_sum)
  random_numbers <- random_numbers / sum(random_numbers) * total_sum
  final_number <- total_sum - sum(random_numbers)
  c(random_numbers, final_number)
}

start_date <- as.Date("2022/2/24")
end_date <- Sys.Date()

total_sum <- 1

num_elements <- 702*2*17  # number of corridors (orig!=dest) x sex x age_groups

all_days <- list()

for (day in seq(start_date, end_date, by="day")) {
  set.seed(day) # 
  all_days[[day]] <- gen_rn(total_sum, num_elements)
}


wide_data <- as.data.frame(do.call(cbind, all_days)) 
colnames(wide_data) <- seq(start_date, end_date, by="day")

dummy_mig_matrix <- cbind(dummy_mig_matrix, wide_data)

long_data <- pivot_longer(dummy_mig_matrix, 
                          cols = 7:747, 
                          names_to = "day", 
                          values_to = "probability") %>%
  pivot_wider(names_from = "age_group",
              values_from = "probability") %>%
  mutate(`0-999`=sum(7:23)) %>%
  pivot_longer(cols = 7:24,
               names_to = "age_group",
               values_to = "probability") %>%
  separate(age_group, c("age_min", "age_max")) %>%
  mutate(age_min = as.numeric(age_min),
         age_max = as.numeric(age_max)) %>%
  pivot_wider(names_from = "sex",
              values_from = "probability") %>%
  mutate(all = female + male ) %>%
  pivot_longer(cols = c("all", "female", "male"),
               names_to = "sex",
               values_to = "probability") %>%
  mutate(sex = recode(sex, "all"=0, "male"=1, "female"=2),
         count = 43790000*probability) %>%
  select(country, admin_level, origin, destination, day,
         age_min, age_max, sex, probability, count)
      

#write.csv(long_data, "dummy_mig_matrix.csv", row.names=FALSE)

