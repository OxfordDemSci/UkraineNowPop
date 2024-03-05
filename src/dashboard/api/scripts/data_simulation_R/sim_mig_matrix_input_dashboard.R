#install.packages("tidyverse")
library("tidyverse")


dummy_mig_matrix <- expand.grid(
orig_admin1_code=c("UA01", "UA05", "UA07", "UA12", "UA14",
                   "UA18", "UA21", "UA23", "UA26", "UA32",
                   "UA35", "UA44", "UA46", "UA48", "UA51",
                   "UA53", "UA56", "UA59", "UA61", "UA63",
                   "UA65", "UA68", "UA71", "UA73", "UA74",
                   "UA80", "UA85"), 
  
  dest_admin1_code = c("UA01", "UA05", "UA07", "UA12", "UA14",
                       "UA18", "UA21", "UA23", "UA26", "UA32",
                       "UA35", "UA44", "UA46", "UA48", "UA51",
                       "UA53", "UA56", "UA59", "UA61", "UA63",
                       "UA65", "UA68", "UA71", "UA73", "UA74",
                       "UA80", "UA85"), 
  
  sex = c("male", "female"),
  
  age_groups = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                 "25-29", "30-34", "35-39", "40-44", "45-49", 
                 "50-54", "55-59", "60-64", "65-69", "70-74", 
                 "75-79", "80+")) %>%
  filter(orig_admin1_code!=dest_admin1_code)


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
                          cols = 5:length(dummy_mig_matrix), 
                          names_to = "Day", 
                          values_to = "prob_mig") 

write.csv(long_data, "dummy_mig_matrix.csv")

