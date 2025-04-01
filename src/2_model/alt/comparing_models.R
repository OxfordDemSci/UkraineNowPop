library(cmdstanr)  
library(rstan)     
library(tidyverse)
library(posterior)
library(matrixStats)
library(ggplot2)



fit_2 <- readRDS("/data/home/andrea/git/OxfordDemSci/UkraineNowPop/wd/out/modelling/2_props_model/mcmc/fit_2_props_model.rds")
fit_2$draws(variables = "N")
fit2_df <- fit_2 %>%
  as_draws_df() %>%
  t() %>%
  as.data.frame()%>%
    rownames_to_column(var = "parameter") %>%
    filter(str_starts(parameter, "N")) %>%
    mutate(parameter = str_sub(parameter, 3, -2) %>% as.integer()) %>%
    left_join(master_index, by = "parameter") %>%  
    #separate(col = a_name, into = c("age_min0", "age_max0"), sep = "_", convert = TRUE) %>%
    mutate(
      pop2 = round(rowMeans(select(., starts_with("V")), na.rm = TRUE), 0) %>% as.integer(),
      pop_lower2 = round(rowQuantiles(as.matrix(select(., starts_with("V"))), probs = 0.025), 0) %>% as.integer(),
      pop_upper2 = round(rowQuantiles(as.matrix(select(., starts_with("V"))), probs = 0.975), 0) %>% as.integer()) %>%
      select(-starts_with("V")) 

fit_3 <- readRDS("/data/home/andrea/git/OxfordDemSci/UkraineNowPop/wd/out/modelling/3_covs_model/mcmc/fit_3_covs_model.rds")
fit_3$draws(variables = "N")
fit3_df <- fit_3 %>%
  as_draws_df() %>%
  t() %>%
  as.data.frame() %>%
rownames_to_column(var = "parameter") %>%
filter(str_starts(parameter, "N")) %>%
mutate(parameter = str_sub(parameter, 3, -2) %>% as.integer()) %>%
left_join(master_index, by = "parameter") %>%  
#separate(col = a_name, into = c("age_min0", "age_max0"), sep = "_", convert = TRUE) %>%
mutate(
  pop3 = round(rowMeans(select(., starts_with("V")), na.rm = TRUE), 0) %>% as.integer(),
  pop_lower3 = round(rowQuantiles(as.matrix(select(., starts_with("V"))), probs = 0.025), 0) %>% as.integer(),
  pop_upper3 = round(rowQuantiles(as.matrix(select(., starts_with("V"))), probs = 0.975), 0) %>% as.integer()) %>%
  select(-starts_with("V")) 


fit_combined <- left_join(fit2_df, fit3_df)

View(fit_combined)


ggplot(fit_combined, aes(x = pop2, y = pop3)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_errorbar(aes(ymin = pop_lower3, ymax = pop_upper3), width = 0.1, alpha = 0.5) +
  geom_errorbarh(aes(xmin = pop_lower2, xmax = pop_upper2), height = 0.1, color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Comparison of Pop2 and Pop3 Estimates",
    x = "Population Estimate from Model 2 (pop2)",
    y = "Population Estimate from Model 3 (pop3)"
  ) +
  theme_minimal()
