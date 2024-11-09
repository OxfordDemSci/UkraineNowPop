# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(here)

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
repo_dir <- env$repo_dir
src_dir <- file.path(repo_dir, "src", "2_model", "nmixture")

wd <- file.path(repo_dir, "wd")
dir.create(wd, showWarnings = F, recursive = T)
setwd(wd)

in_dir <- env$in_dir
out_dir <- file.path(env$out_dir, "modelling", "nmixture")
dir.create(out_dir, showWarnings = F, recursive = T)



#---- load data ----#
model_name <- "1_base_model"

fit <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("fit_", model_name, ".rds")))
md <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("md_", model_name, ".rds")))



#---- summary statistics ----#
fit_summary <- fit$summary()
print(fit_summary)

not_converged <- which(fit_summary[["rhat"]] > 1.1) # 1.01 is cutoff for publication quality
fit_summary[not_converged, ]

write.csv(fit_summary, file.path(out_dir, model_name, "eval", "fit_summary.csv"), row.names = F)



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
jpeg(
  filename = file.path(out_dir, model_name, "eval", "observation_ratio.jpg"),
  height = 720, width = 720
)

names_FG_ratio <- paste0("FG_ratio[", 1:md$n_FG, "]")
FG_ratio <- apply(fit$draws(names_FG_ratio, format = "df"), 2, mean)

plot(
  x = md$y_FG_ratio,
  y = FG_ratio[names_FG_ratio],
  main = "Observation Ratio Check",
  xlab = "Observed",
  ylab = "Predicted"
)
abline(0, 1, col = "red")

dev.off()



#---- time series plots ----#
dir.create(file.path(out_dir, model_name, "eval", "time_series_plots"), showWarnings = F, recursive = T)

# plotting function
plot_time_series <- function(fit, md, model_name, plot_vars = c("N", "p_F", "p_G"), locs = 1:md$I) {
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
  plot_vars = c("N", "p_F", "p_G"),
  locs = 1:md$I
)



#---- trace plots ----#

dir.create(file.path(out_dir, model_name, "eval", "trace_plots"), showWarnings = F, recursive = T)


## global parameters
pars <- c("alpha_r", "beta_r", "sigma_r", "mu_p_F", "sigma_p_F", "mu_p_G", "sigma_p_G")
trace_plot <- mcmc_trace(fit$draws(pars))

ggplot2::ggsave(trace_plot, filename = file.path(out_dir, model_name, "eval", "trace_plots", "global_parameters.jpg"))


## location-time-specific parameters
for (i in 1:md$I) {
  for (t in 1:md$T) {
    i_name <- gsub(" ", "_", paste(i, t, unique(md$idx$i_name[md$idx$i == i])))
    j <- which(md$tt == t & md$ii == i)

    pars <- c(paste0(c("N", "r", "p_F", "p_G"), "[", j, "]"))
    trace_plot <- mcmc_trace(fit$draws(pars))

    ggplot2::ggsave(trace_plot, filename = file.path(out_dir, model_name, "eval", "trace_plots", paste0(i_name, ".jpg")))
  }
}
