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

ncores <- 6

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
model_name <- "339_covs_model"


fit <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("fit_339_covs_model2",".rds")))
#recompiled <- cmdstanr::cmdstan_model(write_stan_file(fit$code()))  #see code of origin of fit objetc
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
names_N <- paste0("N[", 1:(md$T * md$C), "]")

draws_df <- fit$draws(variables = "N", format = "df")
head(names(draws_df))

N_draws <- as.matrix(draws_df[, grep("^N\\[", names(draws_df))])
N_means <- colMeans(N_draws)

N_t_mean <- sapply(seq_len(md$T), function(t) {
  idxs <- ((t - 1) * md$C + 1):(t * md$C)
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

params <- c("N", "r", "p_F", "p_G")

param_labels <- c(
  "N"   = "Total population (N)", 
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


plot_df <- data.frame(
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) %>%
  tibble::rownames_to_column(var = "parameter") %>%
  mutate(
    param = gsub("\\[.*", "", parameter),  
    tias = gsub(".*\\[|\\]", "", parameter)  %>% as.integer()
  ) %>%
  filter(param=="N"|param=="r"|param=="p_F"|param=="p_G") %>%
  left_join(md$idx, by = c("tias"))



for (name in unique(plot_df$i_name)) {
  df_i <- plot_df %>% filter(i_name == name)

  plots <- list()

  for (param in params) {
    df_param <- df_i %>% filter(param == !!param)

    breaks_vec <- {
      labels <- unique(df_param$t_name)
      if (length(labels) >= 5) labels[seq(1, length(labels), by = 5)] else labels
    }

    p <- ggplot(df_param, aes(x = t_name, y = predicted, group = a_name)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = a_name), alpha = 0.2, colour = NA) +
      geom_line(aes(colour = a_name)) +
      geom_point(aes(colour = a_name, shape = a_name), size = 1.8) +
      scale_x_discrete(breaks = breaks_vec) +
      labs(
        x = "Date",
        y = param_labels[param],
        colour = "Age group",
        fill   = "Age group",
        shape  = "Age group"
      ) +
      facet_wrap(~s_name, ncol = 2) +  # One column per sex (F/M)
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(size = 12)
      )

    if (param == "N") {
      p <- p + ggtitle(name)
    }

    plots[[param]] <- p
  }

  combined_plot <- wrap_plots(plots, ncol = 1)

  safe_name <- gsub("[^A-Za-z0-9_]", "_", name)
  ggsave(
    filename = paste0(safe_name, ".jpg"),
    plot = combined_plot,
    path = file.path(out_dir, model_name, "eval", "time_series_plots"),
    width = 14,
    height = 12
  )
}

#####################################################
# Evolution of population pyramids
#####################################################
n_steps      <- 15
times_to_plot <- unique(round(seq(1, md$T, length.out = n_steps)))

draws_N <- fit$draws(variables = "N", format = "df") |> select(!starts_with("."))
prob_lower <- 0.025
prob_upper <- 0.975

params_mean  <- apply(draws_N, 2, mean)
params_lower <- apply(draws_N, 2, quantile, probs = prob_lower)
params_upper <- apply(draws_N, 2, quantile, probs = prob_upper)


plot_df <- data.frame(
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) %>%
  tibble::rownames_to_column(var = "parameter") %>%
  mutate(
    param = gsub("\\[.*", "", parameter),  
    tias = gsub(".*\\[|\\]", "", parameter)  %>% as.integer()
  ) %>%
  filter(param=="N") %>%
  left_join(md$idx, by = c("tias")) %>% 
  mutate(pop = ifelse(s==1, -predicted, predicted),
         lower1 = ifelse(s==1, -lower, lower),
         upper1 = ifelse(s==1, -upper, upper)) %>%
  filter(t %in% times_to_plot)


oblasts <- unique(plot_df$i_name)

for (ob in oblasts) {
  # select only the current oblast
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
pars_model <- c("N", "r", "p_F", "p_G")

t_values_to_plot <- seq(1, md$T, by = 5)

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
      labeller  = label_parsed
    ))

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
plot_traces <- function(fit, params, prefix, ncol = 5,
                        n_warmup = 300, chunk_size = 10,
                        plots_dir) {
  chunks <- split(params, ceiling(seq_along(params) / chunk_size))
  for (i in seq_along(chunks)) {
    pars_chunk <- chunks[[i]]

    draws <- fit$draws(variables = pars_chunk, inc_warmup = FALSE)

    plt <- mcmc_trace(
      draws,
      pars       = pars_chunk,
      n_warmup   = n_warmup,
      facet_args = list(ncol = ncol, labeller = label_parsed)
    ) + ggtitle(sprintf("%s: chunk %d/%d", prefix, i, length(chunks)))

    ggsave(
      filename = sprintf("%s_chunk%02d.jpg", prefix, i),
      plot     = plt,
      path     = plots_dir,
      width    = 12,
      height   = 8
    )}}

I   <- md$I         
A   <- md$A         
S   <- md$S         
K_r <- md$K_r       
K_p <- md$K_p       
T_  <- md$T         

plots_dir <- file.path(out_dir, model_name, "eval", "trace_plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Global hyper‑parameters 
pars_global <- c(
  "alpha_r",
  "sigma_r",           
  "alpha_p",
  paste0("phi_p[", 1:A, "]"),
  paste0("beta_p[", 1:K_p, "]"),
  "sigma_F",
  "sigma_G",
  "log_sigma_delta_p",     
  "log_sigma_gamma_i",
  "log_sigma_gamma_a",
  "log_sigma_gamma_s"
)
plot_traces(fit, pars_global, prefix = "01_global", plots_dir = plots_dir)

# Growth‑rate slopes  beta_r
rows_beta_r <- A * S
pars_beta_r <- paste0(
  "beta_r[",
  rep(1:rows_beta_r, each = K_r), ",",
  rep(1:K_r,        times = rows_beta_r),
  "]"
)
plot_traces(fit, pars_beta_r, prefix = "02_beta_r", plots_dir = plots_dir)

# Oblast, age and sex RE - gamma_p
pars_loc <- c(
  paste0("gamma_p_i[", 1:I, "]"),
  paste0("gamma_p_a[", 1:A, "]"),
  paste0("gamma_p_s[", 1:S, "]")
)
plot_traces(fit, pars_loc, prefix = "03_gamma_p", plots_dir = plots_dir)

# Time‑specific detection effects  delta_p
t_vals    <- seq(1, T_, by = 5)
pars_time <- paste0("delta_p[", t_vals, "]")
plot_traces(fit, pars_time,
            prefix    = "04_delta_p_sample",
            ncol      = 3,
            plots_dir = plots_dir)

# Sample of N_tot
pars_Ntot <- paste0("N_tot[", seq(1, T_, by = 5), "]")
plot_traces(fit, pars_Ntot,
            prefix    = "05_N_tot_sample",
            ncol      = 3,
            plots_dir = plots_dir)


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
