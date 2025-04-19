# Load required libraries
library(bayesplot)
library(ggplot2)
library(cmdstanr)
library(posterior)
library(dplyr)
library(here)
library(patchwork)

# Load environment
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
model_name <- "334_covs_model"

fit <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("fit_", model_name, ".rds")))
md <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("md_", model_name, ".rds")))

dir.create(file.path(out_dir, model_name, "eval"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, model_name, "eval", "time_series_plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, model_name, "eval", "trace_plots"), showWarnings = FALSE, recursive = TRUE)

out_dir_idx <- file.path(env$out_dir)
idx = read.csv(file.path(out_dir_idx, paste0(tolower(country), "_master_index", ".csv"))) 

##############################
# Checking total population
##############################
# names_N_tot <- paste0("N_tot", "[", 1:md$T, "]")
# N_tot <- apply(fit$draws(names_N_tot, format = "df"), 2, mean)
  
# jpeg(file.path(out_dir, model_name, "eval", "total_population_check.jpg"), width = 800, height = 800)
# plot(
#       x = md[["y_N_tot"]],
#       y = N_tot[names_N_tot],
#       main = paste("Total Population Check -"),
#       xlab = "Observed",
#       ylab = "Predicted"
#   )
# abline(0, 1, col = "red")
# dev.off()
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

  

####################################################
# Trace plots for location-time-specific parameters
#####################################################
pars_model <- c("N", "r", "p_F", "p_G")

t_values_to_plot <- seq(1, 117, 5)

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

  # Build the one‐index parameter names
  pars_batch <- vapply(
    pars_model,
    function(p) paste0(p, "[", b, "]"),
    character(1)
  )

  # Extract the posterior draws for these parameters
  draws <- fit$draws(variables = pars_batch, inc_warmup = FALSE)

  # Make a trace plot
  trace_plot <- mcmc_trace(
    draws,
    n_warmup   = 300,
    facet_args = list(
      ncol      = 4,           
      labeller  = label_parsed
    )
  )

  # Save out
  outfile <- file.path(folder_path, paste0("traceplot_", plot_label, ".jpg"))
  ggsave(
    filename = outfile,
    plot     = trace_plot,
    width    = 12,   
    height   = length(pars_batch) * 1.5
  )
}



####################################################
# Other trace plots for location-time-specific parameters
#####################################################
# === Global Parameters Trace Plots ===
pars <- list(
  "334_covs_model" = c(
    "alpha_r",
    "mu_beta_r",    # vector[K_r]
    "sigma_beta_r", # vector<lower=0>[K_r]
    "log_sigma_r",

    # Detection intercepts & hyper‐parameters
    "alpha_p",
    "phi_p",        # vector[A]
    "sigma_phi_p",  # scalar
    "mu_beta_p",    # vector[K_p]
    "sigma_beta_p", # vector<lower=0>[K_p]

    # Residual SDs
    "log_sigma_F",
    "log_sigma_G",

    # Random‐effect hyper‐SDs
    "log_sigma_delta_p",
    "log_sigma_gamma_i",
    "log_sigma_gamma_a",
    "log_sigma_gamma_s"
  )
)

draws <- fit$draws(variables = pars[[model_name]], inc_warmup = FALSE)

trace_plot <- bayesplot::mcmc_trace(draws, 
                                    n_warmup = 300)
outfile <- file.path(out_dir, model_name, "eval", "trace_plots", "0_global_parameters.jpg")
ggplot2::ggsave(filename = outfile, 
  plot = trace_plot, width = 30, height = 30)



# === Location-specific Parameters (gamma_p) ===
pars <- list(
  "333_covs_model" = c(
    "gamma_p_i",   # location‐level effects
    "gamma_p_a",   # age‐group effects
    "gamma_p_s"    # sex‐specific effects
  )
)

draws <- fit$draws(variables = pars, inc_warmup = FALSE)
  
trace_plot <- bayesplot::mcmc_trace(draws, 
                                    n_warmup = 300)
outfile <- file.path(out_dir, model_name, "eval", "trace_plots", "0_loc_parameters.jpg")
ggplot2::ggsave(filename = outfile, 
  plot = trace_plot, width = 30, height = 30)


# === Time-specific Parameters (delta_p) ===
param <- c("delta_p")
t_values_to_plot <- seq(1, 117, 5)

folder_path <- file.path(out_dir, model_name, "eval", "trace_plots", "time_parameters")
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

for (b in seq_len(nrow(grid_info_all))) {
  grid_info <- grid_info_all %>% slice(b)
  t_label   <- gsub(" ", "_", grid_info$t_name)

  pars_batch <- unlist(lapply(param, function(p) {
    if (p == "delta_p") {
      paste0("delta_p[", grid_info$t, "]")
    } else if (p == "N_tot") {
      paste0("N_tot[", grid_info$t, "]")
    } else {
      NULL
    }
  }))

  draws <- fit$draws(variables = pars_batch, inc_warmup = FALSE)

  trace_plot <- bayesplot::mcmc_trace(
    draws,
    n_warmup = 300,
    facet_args = list(ncol = 1, labeller = label_parsed)
  )

  outfile <- file.path(
    folder_path,
    paste0("traceplot_", t_label, ".jpg")
  )
  ggplot2::ggsave(
    filename = outfile,
    plot     = trace_plot,
    width    = 10,
    height   = 10
  )
}

#####################################################
## Summary statistics
#####################################################
fit_summary <- fit$summary(.cores = ncores)
write.csv(fit_summary, file.path(out_dir, model_name, "eval", "fit_summary.csv"), row.names = FALSE)
  
not_converged <- which(fit_summary[["rhat"]] > 1.1) # 1.01 is cutoff for publication quality
  if (length(not_converged) > 0) {
    print(fit_summary[not_converged, ])
  }
write.csv(not_converged, file.path(out_dir, model_name, "eval", "not_converged_summary.csv"), row.names = FALSE)
