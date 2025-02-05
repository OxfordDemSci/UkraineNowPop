# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(here)

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# cores for parallel processing
ncores <- 2

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = F, recursive = T)
setwd(file.path(here::here(), "wd"))

# directories
repo_dir <- env$repo_dir
src_dir <- file.path(repo_dir, "src", "2_model")
in_dir <- env$in_dir
out_dir <- file.path(env$out_dir, "modelling")



#---- load data ----#
model_name <- "2_covs_model"

fit <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("fit_", model_name, ".rds")))
md <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("md_", model_name, ".rds")))

dir.create(file.path(out_dir, model_name, "eval"), showWarnings = F, recursive = T)


# consider subsetting MCMC chains to keep the final X draws


#---- check total population ----#
jpeg(
  filename = file.path(out_dir, model_name, "eval", "total_population.jpg"),
  height = 720, width = 720
)

names_N_tot <- paste0("N_tot[", 1:md$T, "]")
N_tot <- apply(fit$draws(names_N_tot, format = "df"), 2, mean)

plot(
  x = md$y_N_tot,
  y = N_tot[names_N_tot],
  main = "Total Population Check",
  xlab = "Observed",
  ylab = "Predicted"
)
abline(0, 1, col = "red")

dev.off()



#---- check observation ratios ----#
# jpeg(
#   filename = file.path(out_dir, model_name, "eval", "observation_ratio.jpg"),
#   height = 720, width = 720
# )

# names_FG_ratio <- paste0("FG_ratio[", 1:md$n_FG, "]")
# FG_ratio <- apply(fit$draws(names_FG_ratio, format = "df"), 2, mean)

# plot(
#   x = md$y_FG_ratio,
#   y = FG_ratio[names_FG_ratio],
#   main = "Observation Ratio Check",
#   xlab = "Observed",
#   ylab = "Predicted"
# )
# abline(0, 1, col = "red")

# dev.off()


# # Check dependence of F to F:G, etc.
# jpeg(
#   filename = file.path(out_dir, model_name, "eval", "observation_ratio_dependency.jpg"),
#   height = 1080, width = 720
# )

# layout(
#   mat = matrix(1:2, ncol = 1, nrow = 2),
#   heights = rep(1, 2)
# )

# x <- y <- c()

# for (i in 1:md$n_F) {
#   ti <- md$ti_F[i]
#   if (ti %in% md$ti_FG) {
#     y <- c(y, md$y_F[i])
#     x <- c(x, md$y_FG_ratio[md$ti_FG == ti])
#   }
# }

# plot(
#   x, y,
#   xlab = "G:F",
#   ylab = "F",
#   main = paste0("Facebook\n", paste("Spearman R =", round(cor(x, y, method = "spearman"), 2)))
# )

# x <- y <- c()

# for (i in 1:md$n_G) {
#   ti <- md$ti_G[i]
#   if (ti %in% md$ti_FG) {
#     y <- c(y, md$y_G[i])
#     x <- c(x, md$y_FG_ratio[md$ti_FG == ti])
#   }
# }

# plot(
#   x, y,
#   xlab = "G:F",
#   ylab = "G",
#   main = paste0("Instagram\n", paste("Spearman R =", round(cor(x, y, method = "spearman"), 2)))
# )

# dev.off()
# rm(x, y, i, ti)


#---- in-sample posterior predictive check ----#

# Facebook
jpeg(
  filename = file.path(out_dir, model_name, "eval", "postpredict_insamp_facebook.jpg"),
  height = 720, width = 720
)

draws <- fit$draws("F_hat", format = "df") |> select(!starts_with("."))

F_hat_mean <- apply(draws, 2, mean)
F_hat_lower <- apply(draws, 2, quantile, probs = c(0.025))
F_hat_upper <- apply(draws, 2, quantile, probs = c(0.975))

plot(
  x = md$y_F,
  y = F_hat_mean,
  main = "Facebook Posterior Preditive Check (in-sample)",
  xlab = "Observed",
  ylab = "Predicted",
  ylim = c(min(F_hat_lower), max(F_hat_upper))
)

for (i in 1:md$n_F) {
  arrows(
    x0 = md$y_F[i],
    x1 = md$y_F[i],
    y0 = F_hat_lower[i],
    y1 = F_hat_upper[i],
    length = 0
  )
}

abline(0, 1, col = "red")

dev.off()

rm(F_hat_mean, F_hat_lower, F_hat_upper, draws, i)

# Instagram
jpeg(
  filename = file.path(out_dir, model_name, "eval", "postpredict_insamp_instagram.jpg"),
  height = 720, width = 720
)

draws <- fit$draws("G_hat", format = "df") |> select(!starts_with("."))

G_hat_mean <- apply(draws, 2, mean)
G_hat_lower <- apply(draws, 2, quantile, probs = c(0.025))
G_hat_upper <- apply(draws, 2, quantile, probs = c(0.975))

plot(
  x = md$y_G,
  y = G_hat_mean,
  main = "Instagram Posterior Preditive Check (in-sample)",
  xlab = "Observed",
  ylab = "Predicted"
)

abline(0, 1, col = "red")

for (i in 1:md$n_G) {
  arrows(
    x0 = md$y_G[i],
    x1 = md$y_G[i],
    y0 = G_hat_lower[i],
    y1 = G_hat_upper[i],
    length = 0
  )
}

dev.off()

rm(G_hat_mean, G_hat_lower, G_hat_upper, draws, i)


#---- time series plots ----#
dir.create(file.path(out_dir, model_name, "eval", "time_series_plots"), showWarnings = F, recursive = T)

# plotting function
plot_time_series <- function(fit, md, model_name, plot_vars = c("N", "r", "p_F", "p_G"), locs = 1:md$I) {
  outpath <- file.path(out_dir, model_name, "eval", "time_series_plots")

  for (i in locs) {
    i_name <- gsub(" ", "_", paste(i, unique(md$idx$i_name[md$idx$i == i])))
    filename <- paste0(i_name, ".jpg")

    jpeg(filename = file.path(outpath, filename), height = 960, width = 720)

    layout(
      mat = matrix(1:length(plot_vars), ncol = 1, nrow = length(plot_vars)),
      heights = rep(1, length(plot_vars))
    )

    for (k in 1:length(plot_vars)) {
      y_name <- plot_vars[k]
      col_names <- paste0(y_name, "[", which(md$ii == i), "]")
      draws <- fit$draws(col_names, format = "df")

      dat <- data.frame(mean = rep(NA, md$T), lower = NA, upper = NA)
      for (t in 1:md$T) {
        col_name <- paste0(y_name, "[", which(md$tt == t & md$ii == i), "]")
        dat$mean[t] <- mean(draws[[col_name]])
        dat$lower[t] <- quantile(draws[[col_name]], probs = c(0.025))
        dat$upper[t] <- quantile(draws[[col_name]], probs = c(0.975))
      }


      par(mar = c(
        ifelse(k == length(plot_vars), 5, 2),
        5,
        ifelse(k == 1, 4, 2),
        2
      ))

      plot(
        y = dat$mean,
        x = 1:md$T,
        type = "l",
        main = ifelse(k == 1, i_name, NA),
        ylab = y_name,
        xlab = ifelse(k == length(plot_vars), "t", NA),
        xlim = c(1, md$T),
        ylim = c(min(dat$lower), max(dat$upper)),
        cex.axis = 1.25,
        cex.lab = 1.75,
        cex.main = 2
      )

      if (y_name == "r") abline(h = 1)

      lines(
        y = dat$lower,
        x = 1:md$T,
        lty = 2
      )

      lines(
        y = dat$upper,
        x = 1:md$T,
        lty = 2
      )
    }
    dev.off()
  }
}

# make time series plots
plot_time_series(
  fit = fit,
  md = md,
  model_name = model_name,
  plot_vars = c("N", "r", "p_F", "p_G"),
  # plot_vars = c("N", "p_F", "p_G"),
  locs = 1:md$I
)



#---- trace plots ----#

dir.create(file.path(out_dir, model_name, "eval", "trace_plots"), showWarnings = F, recursive = T)


## global parameters
pars <- list(
  "1_base_model" = c("sigma_p_F", "sigma_p_G"),
  "2_covs_model" = c(
    "alpha_r", "beta_r", "log_sigma_r",
    "alpha_p", "log_sigma_delta_p", "log_sigma_gamma_p", "log_sigma_F", "log_sigma_G", "phi_p" 
  )
)
dat <- fit$draws(pars[[model_name]])
trace_plot <- mcmc_trace(dat)
size <- sqrt(dim(dat)[3]) * 2
ggplot2::ggsave(
  trace_plot,
  filename = file.path(out_dir, model_name, "eval", "trace_plots", "0_global_parameters.jpg"),
  width = max(6, size),
  height = max(6, size)
)


## location-specific parameters
pars <- list(
  "1_base_model" = c("mu_p_G"),
  "2_covs_model" = c("gamma_p")
)
for (i in 1:length(pars[[model_name]])) {
  dat <- fit$draws(pars[[model_name]][i])
  trace_plot <- mcmc_trace(dat)
  size <- sqrt(dim(dat)[3]) * 2
  ggplot2::ggsave(
    trace_plot,
    filename = file.path(out_dir, model_name, "eval", "trace_plots", paste0("0_location_parameters_", i, ".jpg")),
    width = max(6, size),
    height = max(6, size)
  )
}


## time-specific parameters
pars <- list(
  "1_base_model" = c("mu_p_F", "N_tot"),
  "2_covs_model" = c("delta_p", "N_tot")
)
for (i in 1:length(pars[[model_name]])) {
  dat <- fit$draws(pars[[model_name]][i])
  trace_plot <- mcmc_trace(dat)
  size <- sqrt(dim(dat)[3]) * 2
  ggplot2::ggsave(
    trace_plot,
    filename = file.path(out_dir, model_name, "eval", "trace_plots", paste0("0_time_parameters_", i, ".jpg")),
    width = max(6, size),
    height = max(6, size)
  )
}


#---- diagnostics that are slow to run ----#


## trace plots for location-time-specific parameters
for (i in 1:md$I) {
  for (t in 1:md$T) {
    i_name <- gsub(" ", "_", paste(i, t, unique(md$idx$i_name[md$idx$i == i])))
    j <- which(md$tt == t & md$ii == i)

    pars <- c(paste0(c("N", "r", "p_F", "p_G"), "[", j, "]"))
    dat <- fit$draws(pars)
    trace_plot <- mcmc_trace(dat)
    size <- sqrt(dim(dat)[3]) * 2

    ggplot2::ggsave(
      trace_plot,
      filename = file.path(out_dir, model_name, "eval", "trace_plots", paste0(i_name, ".jpg")),
      width = max(6, size),
      height = max(6, size)
    )
  }
}


## summary statistics
fit_summary <- fit$summary(.cores = ncores)
print(fit_summary)

not_converged <- which(fit_summary[["rhat"]] > 1.1) # 1.01 is cutoff for publication quality
fit_summary[not_converged, ]

write.csv(fit_summary, file.path(out_dir, model_name, "eval", "fit_summary.csv"), row.names = F)


## LOO cross-validation

# Facebook
loo_F <- fit$loo("log_lik_F", cores = ncores)
print(loo_F)
plot(loo_F)

# Instagram
loo_G <- fit$loo("log_lik_G", cores = ncores)
print(loo_G)
plot(loo_G)

# save to disk
saveRDS(loo_F, file.path(out_dir, model_name, "eval", "loo_F.rds"))
saveRDS(loo_G, file.path(out_dir, model_name, "eval", "loo_G.rds"))
