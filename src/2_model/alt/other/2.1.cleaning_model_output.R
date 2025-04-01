library(cmdstanr)  
library(rstan)     
library(tidyverse)
library(posterior)
library(matrixStats)


model_name <- "211_age_sex_model"

fit <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("fit_", model_name, ".rds")))
md <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("md_", model_name, ".rds")))


pop_stocks <- fit$draws(variables = "N")
pop_stocks1 <- pop_stocks %>%
     as_draws_df() %>%
     t() %>%
     as.data.frame()
  
pop_stocks2 <- pop_stocks1 %>%  
  rownames_to_column(var = "parameter") %>%
  filter(str_starts(parameter, "N")) %>%
  mutate(parameter = str_sub(parameter, 3, -2) %>% as.integer()) %>%
  left_join(master_index, by = "parameter") %>%  
  separate(col = a_name, into = c("age_min0", "age_max0"), sep = "_", convert = TRUE) %>%
  mutate(
        country = rep("UKR", length(ADM1_PCODE)),
         admin_level = rep(1, length(ADM1_PCODE)) %>% as.integer(),
        age_min = ifelse(str_detect(age_min0, "Plus"), str_sub(age_min0, 1, 2), round(as.numeric(age_min0), 0)),
        age_max = ifelse(str_detect(age_min0, "Plus"), 999, round(as.numeric(age_max0), 0)),
      
        pop = round(rowMeans(select(., starts_with("V")), na.rm = TRUE), 0) %>% as.integer(),
        pop_lower = round(rowQuantiles(as.matrix(select(., starts_with("V"))), probs = 0.025), 0) %>% as.integer(),
        pop_upper = round(rowQuantiles(as.matrix(select(., starts_with("V"))), probs = 0.975), 0) %>% as.integer()) %>%
  rename("pcode" = "ADM1_PCODE",
         "day" = "t_name",
         "sex" = "s")
  

         library(dplyr)


pop_stocks3 <- pop_stocks2 %>%         
  rowwise() %>%
  mutate(
  across(starts_with("V"), ~ as.integer(round(.))),  # Corrected across() usage
             pop_posterior = paste0("[", paste(sort(c_across(starts_with("V"))), collapse = ","), "]")  # Fixed c_across()
           ) %>%
  ungroup() %>% 
           dplyr::select(
             country, admin_level, pcode, 
             day, age_min, age_max,
             sex, pop, pop_lower, pop_upper,
             pop_posterior
           )
         
  
write.csv(pop_stocks3,
          file = file.path(out_dir, model_name, "eval", "mod_output_pop.csv"),
          row.names = FALSE)
                
