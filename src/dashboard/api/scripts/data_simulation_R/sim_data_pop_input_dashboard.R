#install.packages("tidyverse")
library("tidyverse")


dummy_dataset <- expand.grid(
  country = "UKR",
  
  admin_level = "1",
  
  pdcode = c("UA01", "UA05", "UA07", "UA12", "UA14",
             "UA18", "UA21", "UA23", "UA26", "UA32",
             "UA35", "UA44", "UA46", "UA48", "UA51",
             "UA53", "UA56", "UA59", "UA61", "UA63",
             "UA65", "UA68", "UA71", "UA73", "UA74",
             "UA80", "UA85"),  #27 groups
  
  sex = c("male", "female"),   #2 groups
  
  age_group = c("0-4", "5-9", "10-14", "15-19", "20-24", 
              "25-29", "30-34", "35-39", "40-44", "45-49", 
              "50-54", "55-59", "60-64", "65-69", "70-74", 
              "75-79", "80-999")) %>%   #17 groups
  mutate(sex = recode(sex, "male"=1, "female"=2)) 
  


gen_rn <- function(total_sum, num_elements) {
  random_numbers <- runif(num_elements - 1, min = 0, max = total_sum)
  random_numbers <- random_numbers / sum(random_numbers) * total_sum
  final_number <- total_sum - sum(random_numbers)
  c(random_numbers, final_number)
}

start_date <- as.Date("2022/2/24")
end_date <- Sys.Date()

total_sum <- 43790000    #baseline population

num_elements <- 27*2*17  # number of admon1_code x sex x age_groups

all_days <- list()

for (day in seq(start_date, end_date, by="day")) {
  set.seed(day) 
  all_days[[day]] <- gen_rn(total_sum, num_elements)
}


wide_data <- as.data.frame(do.call(cbind, all_days)) 
colnames(wide_data) <- seq(start_date, end_date, by="day")

dummy_dataset <- cbind(dummy_dataset, wide_data)
str(dummy_dataset)

long_data <- pivot_longer(dummy_dataset, 
                          cols = 7:746, 
                          names_to = "day", 
                          values_to = "pop") %>%
  select("country", "admin_level", "pdcode", "day",
         "age_group", "sex", "pop") %>%
  pivot_wider(names_from = "age_group",
              values_from = "pop") %>%
  mutate(`0-999`=sum(6:22)) %>%
  pivot_longer(cols = 6:23,
               names_to = "age_group",
               values_to = "pop") %>%
  separate(age_group, c("age_min", "age_max")) %>%
  mutate(age_min = as.numeric(age_min),
         age_max = as.numeric(age_max)) %>%
  pivot_wider(names_from = "sex",
              values_from = "pop") %>%
  mutate(`0` = `1`+`2`) %>%
  pivot_longer(cols = 7:9,
               names_to = "sex",
               values_to = "pop") %>%
  group_by(day) %>%
  mutate(pop = abs(pop),
         pop_upper = abs(abs(pop)+qnorm(0.975)),
         pop_lower = ifelse(abs(pop)<=1, 0, abs(abs(pop)+qnorm(0.025)))) %>% ungroup()

long_data1 <- long_data %>%
  rowwise() %>%
  mutate(pop_posterior = paste("[", paste(rpois(1000, pop), collapse = ", "), "]")) %>%
  ungroup()


#write.csv(long_data1, "dummy_data_pop.csv")
