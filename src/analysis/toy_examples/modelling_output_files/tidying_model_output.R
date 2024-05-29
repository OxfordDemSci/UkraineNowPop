packages <- c("tidyverse", "coda")
if (!require("packages")) install.packages("packages", dependencies = TRUE)
library("tidyverse")
library("coda")


# Tidying up subnational population sizes [N_j,a,s,t]
mod_compl = as.data.frame(as.mcmc(do.call(rbind, coda.complN)))
pop_posteriors <- as.data.frame(t(mod_compl)) %>% 
  rownames_to_column(var = "parameter") %>%
  rowwise() %>%
  mutate(pop_posteriors = paste0("[", paste(c_across(2:1000), collapse = ","), "]")) %>%
  select(parameter, pop_posteriors)



indata_dashboard_pop <- as.data.frame(as.mcmc(do.call(rbind, coda.complN))) %>%
    summarise(across(everything(), list(pop = mean, 
                                        pop_lower = ~quantile(., probs = 0.025),
                                        pop_upper = ~quantile(., probs = 0.975)), 
                     .names = "{.col}-{.fn}")) %>% 
    t() %>% as.data.frame() %>%
    rename("value"="V1") %>%            
    rownames_to_column(var = "parameter") %>%
    separate(parameter, into = c("parameter", "statistic"), sep = "-") %>%
    mutate(value = round(value, 0)) %>%
    spread(key = statistic, value = value) %>%
    left_join(pop_posteriors) %>%
    mutate(parameter = str_sub(parameter, 3, -2)) %>%
    separate(col = parameter, into = c("pcode", "age_group", "sex", "day0"), sep = ",") %>%
    mutate(country = rep("UKR", length(pcode)),
           admin_level = rep(1, length(pcode)),
           pcode = case_when(
                     pcode==1 ~ "UA01", pcode==2 ~ "UA05", pcode==3 ~ "UA07", 
                     pcode==4 ~ "UA12", pcode==5 ~ "UA14", pcode==6 ~ "UA18", 
                     pcode==7 ~ "UA21", pcode==8 ~ "UA23", pcode==9 ~ "UA26", 
                     pcode==10 ~ "UA32", pcode==11 ~ "UA35", pcode==12 ~ "UA44", 
                     pcode==13 ~ "UA46", pcode==14 ~ "UA48", pcode==15 ~ "UA51",
                     pcode==16 ~ "UA53", pcode==17 ~ "UA56", pcode==18 ~ "UA59", 
                     pcode==19 ~ "UA61", pcode==20 ~ "UA63", pcode==21 ~ "UA65", 
                     pcode==22 ~ "UA68", pcode==23 ~ "UA71", pcode==24 ~ "UA73", 
                     pcode==25 ~ "UA74", pcode==26 ~ "UA80", pcode==27 ~ "UA85"),  #27 oblasts
           start_date = as.Date("2022-02-25"),
           day0 = as.numeric(day0),
           day = start_date + day0,
           
           age_group = case_when(
             age_group==1 ~ "0-4", age_group==2 ~ "5-9", 
             age_group==3 ~ "10-14", age_group==4 ~ "15-19", 
             age_group==5 ~ "20-24", age_group==6 ~ "25-29", 
             age_group==7 ~ "30-34", age_group==8 ~ "35-39", 
             age_group==9 ~ "40-44", age_group==10 ~ "45-49", 
             age_group==11 ~ "50-54", age_group==12 ~ "55-59", 
             age_group==13 ~ "60-64", age_group==14 ~ "65-69",
             age_group==15 ~ "70-74", age_group==16 ~ "75-79", 
             age_group==17 ~ "80-999")   #17 groups
           ) %>%
    separate(age_group, into = c("age_min", "age_max"), sep = "-") %>%
    select(country, admin_level,
           pcode, day, 
           age_min, age_max, 
           sex, pop, pop_lower, pop_upper, pop_posteriors)



# Tidying up mobility patterns [N_i,j,a,s,t]
indata_dashboard_mob <- as.data.frame(as.mcmc(do.call(rbind, coda.complNijast))) %>%
  summarise(across(everything(), list(count_mean = mean, 
                                      count_median = median), 
                   .names = "{.col}-{.fn}")) %>% 
  t() %>% as.data.frame() %>%
  rename("value"="V1") %>%            
  rownames_to_column(var = "parameter") %>%
  separate(parameter, into = c("parameter", "statistic"), sep = "-") %>%
  mutate(value = round(value, 0)) %>%
  spread(key = statistic, value = value) %>%
  mutate(parameter = str_sub(parameter, 9, -2)) %>%
  separate(col = parameter, into = c("orig", "dest", "age_group", "sex", "day0"), sep = ",") %>%
  mutate(country = rep("UKR", length(dest)),
         admin_level = rep(1, length(dest)),
         orig = case_when(
           orig==1 ~ "UA01", orig==2 ~ "UA05", orig==3 ~ "UA07", 
           orig==4 ~ "UA12", orig==5 ~ "UA14", orig==6 ~ "UA18", 
           orig==7 ~ "UA21", orig==8 ~ "UA23", orig==9 ~ "UA26", 
           orig==10 ~ "UA32", orig==11 ~ "UA35", orig==12 ~ "UA44", 
           orig==13 ~ "UA46", orig==14 ~ "UA48", orig==15 ~ "UA51",
           orig==16 ~ "UA53", orig==17 ~ "UA56", orig==18 ~ "UA59", 
           orig==19 ~ "UA61", orig==20 ~ "UA63", orig==21 ~ "UA65", 
           orig==22 ~ "UA68", orig==23 ~ "UA71", orig==24 ~ "UA73", 
           orig==25 ~ "UA74", orig==26 ~ "UA80", orig==27 ~ "UA85"),  #27 oblasts
         
         dest = case_when(
           dest==1 ~ "UA01", dest==2 ~ "UA05", dest==3 ~ "UA07", 
           dest==4 ~ "UA12", dest==5 ~ "UA14", dest==6 ~ "UA18", 
           dest==7 ~ "UA21", dest==8 ~ "UA23", dest==9 ~ "UA26", 
           dest==10 ~ "UA32", dest==11 ~ "UA35", dest==12 ~ "UA44", 
           dest==13 ~ "UA46", dest==14 ~ "UA48", dest==15 ~ "UA51",
           dest==16 ~ "UA53", dest==17 ~ "UA56", dest==18 ~ "UA59", 
           dest==19 ~ "UA61", dest==20 ~ "UA63", dest==21 ~ "UA65", 
           dest==22 ~ "UA68", dest==23 ~ "UA71", dest==24 ~ "UA73", 
           dest==25 ~ "UA74", dest==26 ~ "UA80", dest==27 ~ "UA85"),  
         
         start_date = as.Date("2022-02-25"),
         day0 = as.numeric(day0),
         day = start_date + day0,
         
         age_group = case_when(
           age_group==1 ~ "0-4", age_group==2 ~ "5-9", 
           age_group==3 ~ "10-14", age_group==4 ~ "15-19", 
           age_group==5 ~ "20-24", age_group==6 ~ "25-29", 
           age_group==7 ~ "30-34", age_group==8 ~ "35-39", 
           age_group==9 ~ "40-44", age_group==10 ~ "45-49", 
           age_group==11 ~ "50-54", age_group==12 ~ "55-59", 
           age_group==13 ~ "60-64", age_group==14 ~ "65-69",
           age_group==15 ~ "70-74", age_group==16 ~ "75-79", 
           age_group==17 ~ "80-999")   #17 groups
  ) %>%
  separate(age_group, into = c("age_min", "age_max"), sep = "-") %>%
  select(country, admin_level,
         orig, dest, day, 
         age_min, age_max, sex, 
         count_mean, count_median)#,
         #prob_mean, prob_median)
