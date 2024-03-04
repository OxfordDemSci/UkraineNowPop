#install.packages("tidyverse")
library("tidyverse")


dummy_dataset <- expand.grid(
  admin1_code = c("UA05", "UA07", "UA12", "UA14", "UA18", 
                  "UA21", "UA23", "UA26", "UA32", "UA35", 
                  "UA44", "UA46", "UA48", "UA51", "UA53", 
                  "UA56", "UA59", "UA61", "UA63", "UA65", 
                  "UA68", "UA71", "UA73", "UA74", "UA80",
                  "RW"),   #RW = Rest of the world
  
  sex = c("male", "female"),
  
  age_groups = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                 "25-29", "30-34", "35-39", "40-44", "45-49", 
                 "50-54", "55-59", "60-64", "65-69", "70-74", 
                 "75-79", "80+")) 
  
  
gen_rn <- function(total_sum, num_elements) {
    random_numbers <- runif(num_elements - 1, min = 0, max = total_sum)
    random_numbers <- random_numbers / sum(random_numbers) * total_sum
    final_number <- total_sum - sum(random_numbers)
    c(random_numbers, final_number)
  }
  
start_date <- as.Date("2022/2/24")
end_date <- Sys.Date()

total_sum <- 43790000

num_elements <- 26*2*17  # number of geo x sex x age_groups

all_days <- list()
  
for (day in seq(start_date, end_date, by="day")) {
    set.seed(day) # 
    all_days[[day]] <- gen_rn(total_sum, num_elements)
  }
  

wide_data <- as.data.frame(do.call(cbind, all_days)) 
colnames(wide_data) <- seq(start_date, end_date, by="day")
  
dummy_dataset <- cbind(dummy_dataset, wide_data)

long_data <- pivot_longer(dummy_dataset, 
                            cols = 4:length(dummy_dataset), 
                            names_to = "Day", 
                            values_to = "mean_pop") %>%
             group_by(Day) %>%
             mutate(median_pop = mean_pop,
                    q2.5_pop = mean_pop-qnorm(0.975)*sd(mean_pop)/sqrt(dim(dummy_dataset)[1]),
                    q97.5_pop = mean_pop+qnorm(0.975)*sd(mean_pop)/sqrt(dim(dummy_dataset)[1]),
                    q20_pop = mean_pop-qnorm(0.80)*sd(mean_pop)/sqrt(dim(dummy_dataset)[1]), 
                    q80_pop = mean_pop+qnorm(0.80)*sd(mean_pop)/sqrt(dim(dummy_dataset)[1])) %>% ungroup()
  
 write.csv(long_data, "long_dummy_data.csv")
