# Load required libraries
library(bayesplot)
library(ggplot2)
library(cmdstanr)
library(posterior)
library(dplyr)
library(here)
library(patchwork)
library(purrr) 

env <- new.env()
source(here::here(".env"), local = env)

ncores <- 10

# Working directory
dir.create(file.path(here::here(), "wd"), showWarnings = FALSE, recursive = TRUE)
setwd(file.path(here::here(), "wd"))

# Directories
repo_dir <- env$repo_dir
src_dir <- file.path(repo_dir, "src", "2_model")
in_dir <- env$in_dir
out_dir <- file.path(env$out_dir, "modelling")

# Source functions
source(file.path(src_dir, "2_eval_fun.R"))

country <- "UA"
model_name <- "356_covs_model"


fit <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("fit_", model_name,".rds")))
#recompiled <- cmdstanr::cmdstan_model(write_stan_file(fit$code())); recompiled
md <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("md_", model_name, ".rds")))

dir.create(file.path(out_dir, model_name, "eval"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, model_name, "eval", "time_series_plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, model_name, "eval", "trace_plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, model_name, "eval", "population_pyramids"), showWarnings = FALSE, recursive = TRUE)

out_dir_idx <- file.path("/data/home/andrea/git/OxfordDemSci/UkraineNowPop/wd/out")
idx = read.csv(file.path(out_dir_idx, paste0(tolower(country), "_master_index", ".csv"))) 

##############################
# Checking total population
##############################
names_N <- paste0("N_full[", 1:(md$T * md$C_full), "]")

draws_df <- fit$draws(variables = "N_full", format = "df")
head(names(draws_df))

N_draws <- as.matrix(draws_df[, grep("^N_full\\[", names(draws_df))])
N_means <- colMeans(N_draws)

# Summing into time‐blocks of length C
N_t_mean <- sapply(seq_len(md$T), function(t) {
  idxs <- ((t - 1) * md$C_full + 1):(t * md$C_full)
  sum(N_means[idxs])
})

df_tot <- data.frame(observed  = md$y_N_tot, predicted = N_t_mean)
p <- ggplot(df_tot, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Total Population Check",
    x     = "Observed y_N_tot",
    y     = "Posterior mean total N"
  ) + theme_minimal()
ggsave(file.path(out_dir, model_name, "eval", "total_population_check.jpg"), 
       plot = p, width = 10, height = 8)
#####################################################
# Posterior predictive check
#####################################################
plot_postpred_fit(
  dat = "y_F", 
  hat = "F_hat",
  outfile = file.path(out_dir, model_name, "eval", paste0("postpredict_insamp_facebook", ".jpg")))

dat <- "y_F"
hat <- "F_hat"

draws <- fit$draws(hat, format = "df") |> select(!starts_with("."))
prob_lower <- 0.025
prob_upper <- 0.975

hat_mean  <- apply(draws, 2, mean)
hat_lower <- apply(draws, 2, quantile, probs = prob_lower)
hat_upper <- apply(draws, 2, quantile, probs = prob_upper)

df_idx_F <-  as.data.frame(md$idx_F) %>%
  mutate(parameter = row_number())

plot_df <- data.frame(
  observed  = md[[dat]],
  predicted = hat_mean,
  lower     = hat_lower,
  upper     = hat_upper) %>%
tibble::rownames_to_column(var = "parameter") %>%
mutate(parameter = as.integer(gsub("[^0-9]", "", parameter))) %>%
left_join(df_idx_F, by = c("parameter")) %>%
left_join(idx %>% dplyr::select(t, t_name, i, i_name, macroregion, s, s_name, a, a_name)) %>%
  mutate(year = case_when(
    t_name>="2022-01-01" & t_name<= "2022-12-31" ~ 2022,
    t_name>="2023-01-01" & t_name<= "2023-12-31" ~ 2023,
    t_name>="2024-01-01" & t_name<= "2024-12-31" ~ 2024
  ) %>% as.integer())


p <- ggplot(plot_df, aes(x = observed, y = predicted, colour = s_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  facet_wrap(year~macroregion, scales = "free") +
  labs(
    title = "Posterior Predictive Check by Macroregion",
    x = paste0("Observed (", dat, ")"),
    y = paste0("Predicted (", hat, ")"),
    colour = "s_name"
  ) +
  theme_minimal()
p
ggsave(file.path(out_dir, model_name, "eval", "postpredict_insamp_facebook1.jpg"), plot = p, width = 10, height = 8)


p <- ggplot(plot_df, aes(x = observed, y = predicted, colour = s_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  facet_wrap(a_name~macroregion, scales = "free") +
  labs(
    title = "Posterior Predictive Check by Macroregion",
    x = paste0("Observed (", dat, ")"),
    y = paste0("Predicted (", hat, ")"),
    colour = "a_name"
  ) +
  theme_minimal()
p
ggsave(file.path(out_dir, model_name, "eval", "postpredict_insamp_facebook2.jpg"), plot = p, width = 10, height = 8)


  
plot_postpred_fit(
      dat = "y_G", 
      hat = "G_hat",
      outfile = file.path(out_dir, model_name, "eval", paste0("postpredict_insamp_instagram", ".jpg")))


dat <- "y_G"
hat <- "G_hat"

draws <- fit$draws(hat, format = "df") |> select(!starts_with("."))
prob_lower <- 0.025
prob_upper <- 0.975

hat_mean  <- apply(draws, 2, mean)
hat_lower <- apply(draws, 2, quantile, probs = prob_lower)
hat_upper <- apply(draws, 2, quantile, probs = prob_upper)

df_idx_G <-  as.data.frame(md$idx_G) %>%
  mutate(parameter = row_number())

plot_df <- data.frame(
  observed  = md[[dat]],
  predicted = hat_mean,
  lower     = hat_lower,
  upper     = hat_upper) %>%
tibble::rownames_to_column(var = "parameter") %>%
mutate(parameter = as.integer(gsub("[^0-9]", "", parameter))) %>%
left_join(df_idx_G, by = c("parameter")) %>%
left_join(idx %>% dplyr::select(t, t_name, i, i_name, macroregion, s, s_name, a, a_name)) %>%
  mutate(year = case_when(
    t_name>="2022-01-01" & t_name<= "2022-12-31" ~ 2022,
    t_name>="2023-01-01" & t_name<= "2023-12-31" ~ 2023,
    t_name>="2024-01-01" & t_name<= "2024-12-31" ~ 2024
  ) %>% as.integer())


p <- ggplot(plot_df, aes(x = observed, y = predicted, colour = s_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  facet_wrap(year~macroregion, scales = "free") +
  labs(
    title = "Posterior Predictive Check by Macroregion",
    x = paste0("Observed (", dat, ")"),
    y = paste0("Predicted (", hat, ")"),
    colour = "s_name"
  ) +
  theme_minimal()
p
ggsave(file.path(out_dir, model_name, "eval", "postpredict_insamp_instagram1.jpg"), plot = p, width = 10, height = 8)


p <- ggplot(plot_df, aes(x = observed, y = predicted, colour = s_name)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  facet_wrap(a_name~macroregion, scales = "free") +
  labs(
    title = "Posterior Predictive Check by Macroregion",
    x = paste0("Observed (", dat, ")"),
    y = paste0("Predicted (", hat, ")"),
    colour = "a_name"
  ) +
  theme_minimal()
p
ggsave(file.path(out_dir, model_name, "eval", "postpredict_insamp_instagram2.jpg"), plot = p, width = 10, height = 8)

#####################################################
# Time series plots
#####################################################
options(scipen = 999)

params <- c("N_adult", "N_young", "r", "p_F", "p_G")

param_labels <- c(
  "N" = "Children population (N_young)",
  "r"   = "Growth rate (r)",
  "p_F" = "Facebook penetration rate (p_F)",
  "p_G" = "Instagram penetration rate (p_G)"
)

draws <- fit$draws(params, format = "df") |> select(!starts_with("."))
prob_lower <- 0.025
prob_upper <- 0.975

params_mean  <- apply(draws, 2, mean)
params_lower <- apply(draws, 2, quantile, probs = prob_lower)
params_upper <- apply(draws, 2, quantile, probs = prob_upper)


selected_locations <- seq(1, 27, by = 1
                        ) %>% as.integer()

idx_master = read.csv(file.path(out_dir, paste0(tolower(country), "_master_index", ".csv")))

i_idx <- idx_master %>%
  dplyr::select(i_key) %>%
  distinct() %>%
  filter(i_key %in% selected_locations_key) %>%
  arrange(i_key) %>%
  mutate(i = row_number())

idx <- expand.grid(t = 1:T, i = 1:I, a = 1:A, s = 1:S) %>% 
  left_join(i_idx) %>%
  arrange(t, i, a, s) %>%                       
  mutate(tias = row_number()) %>%               
  mutate(tias_adult = NA_integer_,
    tias_young = NA_integer_)                   

idx$tias_adult[idx$a != 1L] <- seq_len(sum(idx$a != 1L))
idx$tias_young[idx$a == 1L] <- seq_len(sum(idx$a == 1L))


plot_df_adult <- tibble(
  parameter = names(params_mean),
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) |>
  mutate(param    = sub("\\[.*", "", parameter),
    tias_adult = sub(".*\\[([^]]+)]$", "\\1", parameter) |> as.integer()) %>%
filter(param %in% c("N_adult", "r", "p_F", "p_G")) %>%
left_join(idx %>% dplyr::select(tias_adult, tias, t, i_key, a, s)) %>%
dplyr::select(-tias_adult) %>%
mutate(a = a-1) %>%
left_join(idx_master %>% dplyr::select(-parameter, t,i,a,s,t_name, i_key, i_name, a_name,s_name)) %>%
dplyr::select(parameter, predicted, lower, upper, param, tias, t, i, a, s, t_name, i_name, a_name, s_name)


idx_y_temp <- idx_master %>% dplyr::select(-a, -a_name, -agesex, -a_key, -parameter) %>% distinct(t, i, s, .keep_all = TRUE)

plot_df_young <- tibble(
  parameter = names(params_mean),
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) |>
  mutate(param    = sub("\\[.*", "", parameter),
    tias_young = sub(".*\\[([^]]+)]$", "\\1", parameter) |> as.integer()) %>%
filter(param %in% c("N_young")) %>%
left_join(idx %>% dplyr::select(tias_young, tias, t, i_key, a, s)) %>%
  dplyr::select(-tias_young) %>%
left_join(idx_y_temp) %>%
mutate(a = 6,
      a_name="0-19") %>%
dplyr::select(parameter, predicted, lower, upper, param, tias, t, i, a, s, t_name, i_name, a_name, s_name)
  


plot_df <- plot_df_adult %>% rbind(plot_df_young) %>%
  mutate(param = ifelse(param=="N_young"|param=="N_adult", "N", param)) %>%
  arrange(t,i,a,s)
  

param_vec    <- c("N", "r", "p_F", "p_G")

param_labels <- c(
  N   = "Total population (N)",
  r   = "Growth rate (r)",
  p_F = "Facebook penetration (p_F)",
  p_G = "Instagram penetration (p_G)"
)

plot_df <- plot_df %>%
  mutate(t_name = factor(t_name, levels = sort(unique(t_name))))

out_dir_plots <- file.path(out_dir, model_name, "eval", "time_series_plots")
dir.create(out_dir_plots, recursive = TRUE, showWarnings = FALSE)

for (name in unique(plot_df$i_name)) {

  df_i  <- filter(plot_df, i_name == name)
  plots <- vector("list", length(param_vec))

  for (k in seq_along(param_vec)) {

    p_code <- param_vec[k]
    df_p   <- filter(df_i, param == p_code)

    labs <- unique(df_p$t_name)
    breaks_vec <- if (length(labs) >= 5) labs[seq(1, length(labs), 5)] else labs

    p <- ggplot(df_p, aes(t_name, predicted, group = a_name)) +
         geom_ribbon(aes(ymin = lower, ymax = upper, fill = a_name),
                     alpha = .20, colour = NA) +
         geom_line  (aes(colour = a_name)) +
         geom_point (aes(colour = a_name, shape  = a_name), size = 1.8) +
         scale_x_discrete(breaks = breaks_vec) +
         labs(x = "Date",
              y = param_labels[[p_code]] %||% p_code,
              colour = "Age group", fill = "Age group", shape = "Age group") +
         facet_wrap(~s_name, ncol = 2) +
         theme_minimal(base_size = 11) +
         theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
               strip.text  = element_text(size = 12))

    if (p_code == "N") p <- p + ggtitle(name)

    plots[[k]] <- p
  }

  combined_plot <- wrap_plots(plots, ncol = 1)

  ggsave(
    filename = file.path(out_dir_plots,
                         paste0(gsub("[^A-Za-z0-9_]", "_", name), ".jpg")),
    plot   = combined_plot,
    width  = 14,
    height = 12
  )
}
#####################################################
# Evolution of population pyramids
#####################################################
n_steps      <- 30
times_to_plot <- unique(round(seq(1, md$T, length.out = n_steps)))

draws_N <- fit$draws(variables = c("N_adult", "N_young"), format = "df") |> select(!starts_with("."))
prob_lower <- 0.025
prob_upper <- 0.975

params_mean  <- apply(draws_N, 2, mean)
params_lower <- apply(draws_N, 2, quantile, probs = prob_lower)
params_upper <- apply(draws_N, 2, quantile, probs = prob_upper)


selected_locations <- seq(1, 27, by = 1) %>% as.integer()

idx_master = read.csv(file.path(out_dir, paste0(tolower(country), "_master_index", ".csv")))

i_idx <- idx_master %>%
  dplyr::select(i_key) %>%
  distinct() %>%
  filter(i_key %in% selected_locations_key) %>%
  arrange(i_key) %>%
  mutate(i = row_number())

idx <- expand.grid(t = 1:T, i = 1:I, a = 1:A, s = 1:S) %>% 
  left_join(i_idx) %>%
  arrange(t, i, a, s) %>%                       
  mutate(tias = row_number()) %>%               
  mutate(tias_adult = NA_integer_,
    tias_young = NA_integer_)                   

idx$tias_adult[idx$a != 1L] <- seq_len(sum(idx$a != 1L))
idx$tias_young[idx$a == 1L] <- seq_len(sum(idx$a == 1L))


plot_df_adult <- tibble(
  parameter = names(params_mean),
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) |>
  mutate(param    = sub("\\[.*", "", parameter),
    tias_adult = sub(".*\\[([^]]+)]$", "\\1", parameter) |> as.integer()) %>%
filter(param %in% c("N_adult", "r", "p_F", "p_G")) %>%
left_join(idx %>% dplyr::select(tias_adult, tias, t, i_key, a, s)) %>%
dplyr::select(-tias_adult) %>%
mutate(a = a-1) %>%
left_join(idx_master %>% dplyr::select(-parameter, t,i,a,s,t_name, i_key, i_name, a_name,s_name)) %>%
dplyr::select(parameter, predicted, lower, upper, param, tias, t, i, a, s, t_name, i_name, a_name, s_name)


idx_y_temp <- idx_master %>% dplyr::select(-a, -a_name, -agesex, -a_key, -parameter) %>% distinct(t, i, s, .keep_all = TRUE)

plot_df_young <- tibble(
  parameter = names(params_mean),
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) |>
  mutate(param    = sub("\\[.*", "", parameter),
    tias_young = sub(".*\\[([^]]+)]$", "\\1", parameter) |> as.integer()) %>%
filter(param %in% c("N_young")) %>%
left_join(idx %>% dplyr::select(tias_young, tias, t, i_key, a, s)) %>%
  dplyr::select(-tias_young) %>%
left_join(idx_y_temp) %>%
mutate(a = 6,
      a_name="0-19") %>%
dplyr::select(parameter, predicted, lower, upper, param, tias, t, i, a, s, t_name, i_name, a_name, s_name)
  


plot_df <- plot_df_adult %>% rbind(plot_df_young) %>%
  mutate(param = ifelse(param=="N_young"|param=="N_adult", "N", param), 
         pop = ifelse(s==1, -predicted, predicted),
         lower1 = ifelse(s==1, -lower, lower),
         upper1 = ifelse(s==1, -upper, upper)) %>%
  filter(t %in% times_to_plot)


oblasts <- unique(plot_df$i_name)

for (ob in oblasts) {
  df_sub <- plot_df %>% filter(i_name == ob)
  
  p <- ggplot(df_sub, aes(x = a_name)) +
    geom_col(aes(y = pop/1000,   fill   = s_name), width = 0.8) +
    geom_errorbar(
      aes(ymin  = lower1/1000, ymax = upper1/1000, colour = s_name),
      width    = 0.3,
      position = position_identity() 
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = abs,
      name   = "Population (in thousands)"
    ) +
    facet_wrap(~ t_name) +
    labs(
      x      = "Age group",
      fill   = "Sex",
      colour = "Sex",
      title  = paste0("Population pyramids – ", ob)
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.title.y       = element_blank(),
      strip.text.x       = element_text(size = 8),
      panel.background   = element_rect(fill = "white", colour = NA),
      plot.background    = element_rect(fill = "white", colour = NA)
    )
  
  safe_name <- gsub("[^A-Za-z0-9_]", "_", ob)
  ggsave(
    filename = paste0(safe_name, ".png"),
    plot     = p,
    path     = file.path(out_dir, model_name, "eval", "population_pyramids"),
    width    = 12,
    height   = 4
  )
}


####################################################
# Trace plots for location-time-specific parameters
#####################################################
pars_model <- c("N_full", "r", "p_F", "p_G")

t_values_to_plot <- seq(1, md$T, by = 30)

grid_info_all <- md$idx %>% 
  distinct(t, i, a, s, t_name, i_name) %>% 
  filter(t %in% t_values_to_plot) %>% 
  arrange(t, i, a, s)
for (b in seq_len(nrow(grid_info_all))) {
  grid_info  <- slice(grid_info_all, b)
  loc_folder <- gsub(" ", "_", grid_info$i_name)
  folder_path <- file.path(out_dir, model_name, "eval", "trace_plots", loc_folder)
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

  plot_label <- paste0(gsub(" ", "_", grid_info$t_name), "_", loc_folder)

  pars_batch <- vapply(
    pars_model,
    function(p) paste0(p, "[", b, "]"),
    character(1)
  )

  draws <- fit$draws(variables = pars_batch, inc_warmup = FALSE)

  trace_plot <- mcmc_trace(
    draws,
    n_warmup   = 300,
    facet_args = list(
      ncol      = 4,           
      labeller  = label_parsed))

  outfile <- file.path(folder_path, paste0("traceplot_", plot_label, ".jpg"))
  ggsave(
    filename = outfile,
    plot     = trace_plot,
    width    = 12,   
    height   = length(pars_batch) * 1.5
  )}


####################################################
# Other trace plots for location–time–specific parameters
####################################################
plot_traces <- function(fit, params, prefix,
  out_dir, model_name,
  ncol       = 5,
  n_warmup   = 300,
  chunk_size = 10,
  width      = 12,
  height     = 8) {

base_path <- file.path(out_dir, model_name, "eval", "trace_plots", prefix)
dir.create(base_path, recursive = TRUE, showWarnings = FALSE)

chunks <- split(params, ceiling(seq_along(params) / chunk_size))

for (i in seq_along(chunks)) {
pars_chunk <- chunks[[i]]
draws <- fit$draws(variables = pars_chunk, inc_warmup = FALSE)

plt <- bayesplot::mcmc_trace(
draws,
pars       = pars_chunk,
n_warmup   = n_warmup,
facet_args = list(ncol = ncol,
  labeller = ggplot2::label_parsed)
) +
ggplot2::ggtitle(sprintf("%s: chunk %d / %d",
       prefix, i, length(chunks)))

ggplot2::ggsave(
filename = sprintf("%s_chunk%02d.jpg", prefix, i),
plot     = plt,
path     = base_path,
width    = width,
height   = height
)}}

I        <- md$I
A        <- md$A
A_adult  <- A - 1
S        <- md$S
K_r      <- md$K_r
K_p      <- md$K_p
T        <- md$T
C_adult  <- T * I * A_adult * S

pars_global <- c(
sprintf("beta_p[%d]", seq_len(K_p)),
sprintf("beta_r[%d]", seq_len(K_r)),
"phi_p",
"sigma_F", "sigma_G",
"sigma_phi_p_i", "sigma_phi_p_a", "sigma_phi_p_s"
)
plot_traces(fit, pars_global,
prefix     = "01_global",
chunk_size = length(pars_global),
out_dir    = out_dir,
model_name = model_name)

pars_gamma_r <- c(
sprintf("gamma_r_i[%d]", seq_len(I)),
sprintf("gamma_r_a[%d]", seq_len(A_adult)),
sprintf("gamma_r_s[%d]", seq_len(S))
)

plot_traces(fit, pars_gamma_r,
prefix     = "02_gamma_r",
out_dir    = out_dir,
model_name = model_name)

pars_gamma_p <- c(
sprintf("gamma_p_i[%d]", seq_len(I)),
sprintf("gamma_p_a[%d]", seq_len(A_adult)),
sprintf("gamma_p_s[%d]", seq_len(S))
)

plot_traces(fit, pars_gamma_p,
prefix     = "03_gamma_p",
out_dir    = out_dir,
model_name = model_name)

pars_phi_p <- c(
sprintf("phi_p_i_raw[%d]", seq_len(I)),
sprintf("phi_p_a_raw[%d]", seq_len(A_adult)),
sprintf("phi_p_s_raw[%d]", seq_len(S))
)

plot_traces(fit, pars_phi_p,
prefix     = "04_phi_p_raw",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name)

every_t_steps <- seq(1, T, by = 30)

plot_traces(fit, sprintf("delta_p[%d]", every_t_steps),
prefix     = "05_delta_p_sample",
ncol       = 3,
chunk_size = length(every_t_steps),
out_dir    = out_dir,
model_name = model_name)

plot_traces(fit, sprintf("delta_r[%d]", every_t_steps),
prefix     = "06_delta_r_sample",
ncol       = 3,
chunk_size = length(every_t_steps),
out_dir    = out_dir,
model_name = model_name)

plot_traces(fit, sprintf("N_tot[%d]", every_t_steps),
prefix     = "07_N_tot_sample",
ncol       = 3,
chunk_size = length(every_t_steps),
out_dir    = out_dir,
model_name = model_name)

set.seed(2025)
pars_r  <- sample(sprintf("r[%d]"  , seq_len(C_adult)), 50)
pars_pF <- sample(sprintf("p_F[%d]", seq_len(C_adult)), 50)
pars_pG <- sample(sprintf("p_G[%d]", seq_len(C_adult)), 50)

plot_traces(fit, pars_r,
prefix     = "08_r_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name)

plot_traces(fit, pars_pF,
prefix     = "09_pF_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name)

plot_traces(fit, pars_pG,
prefix     = "10_pG_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name)


#####################################################
## Summary statistics
#####################################################
fit_summary <- fit$summary(.cores = ncores)
write.csv(fit_summary, file.path(out_dir, model_name, "eval", "fit_summary.csv"), row.names = FALSE)
  
not_converged <- which(round(fit_summary[["rhat"]], 1) > 1.1) 
  if (length(not_converged) > 0) {
    print(fit_summary[not_converged, ])
  }
write.csv(not_converged, file.path(out_dir, model_name, "eval", "not_converged_summary.csv"), row.names = FALSE)
