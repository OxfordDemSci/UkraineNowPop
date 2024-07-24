packages <- c("tidyverse", "MASS", "truncnorm", "rjags", "runjags", 
              "R2jags", "coda", "dclone", "parallel", "viridis", "matrixStats")

if(!require(packages))install.packages(packages)

lapply(packages, library, character.only = TRUE)

#Loading file to clean
load("./output/model/coda.complN_20240702.RData")


# Tidying up subnational population sizes [N_j,a,s,t]
est_log_N <- as.data.frame(as.mcmc(do.call(rbind, coda.complN))) %>% t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "parameter") %>%
  rowwise() %>%
  mutate(across(starts_with("V"), ~ as.integer(round(.))), 
         pop_posterior = paste0("[", paste(sort(c_across(starts_with("V"))), collapse = ","), "]")) %>% ungroup() %>% 
  mutate(parameter = str_sub(parameter, 6, -2)) %>%
  separate(col = parameter, into = c("pcode", "age_group", "sex", "day0"), sep = ",") %>%
  mutate(country = rep("UKR", length(pcode)),
         admin_level = rep(1, length(pcode) %>% as.integer(),
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
         day = format(day, format="%d/%m/%Y"),
         
         sex =  as.integer(sex),
         
         age_group = case_when(
           age_group==1 ~ "0-4", age_group==2 ~ "5-9", 
           age_group==3 ~ "10-14", age_group==4 ~ "15-19", 
           age_group==5 ~ "20-24", age_group==6 ~ "25-29", 
           age_group==7 ~ "30-34", age_group==8 ~ "35-39", 
           age_group==9 ~ "40-44", age_group==10 ~ "45-49", 
           age_group==11 ~ "50-54", age_group==12 ~ "55-59", 
           age_group==13 ~ "60-64", age_group==14 ~ "65-69",
           age_group==15 ~ "70-74", age_group==16 ~ "75-79", 
           age_group==17 ~ "80-999"),   #17 groups
         
         pop = round(rowMeans(as.matrix(est_log_N %>% dplyr::select(starts_with("V"))), na.rm = TRUE), 0) %>% as.integer(),
         pop_lower = round(rowQuantiles(as.matrix(est_log_N %>% dplyr::select(starts_with("V"))), probs=0.025), 0) %>% as.integer(),
         pop_upper = round(rowQuantiles(as.matrix(est_log_N %>% dplyr::select(starts_with("V"))), probs=0.975), 0) %>% as.integer())) %>%
  separate(age_group, into = c("age_min", "age_max"), sep = "-") %>% 
  mutate(age_min = as.integer(age_min),
         age_max = as.integer(age_max)) %>%
  dplyr::select(country, admin_level,
                pcode, day, 
                age_min, age_max,  
                sex, pop, pop_lower, pop_upper, pop_posterior)

write.csv(est_log_N, "./output/model/mod_output_pop.csv")
