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

plot_vars <- c("N", "p_F", "p_G")

for (y_name in plot_vars) {
  dat <- fit$draws(y_name, format = "df")

  for (i in 1:md$I) {
    i_dat <- dat[paste0(y_name, "[", which(md$ii == i), "]")]
    i_name <- gsub(" ", "_", paste(i, unique(md$idx$i_name[md$idx$i == i])))

    jpeg(
      filename = file.path(out_dir, model_name, "eval", "time_series_plots", paste0(i_name, "_", y_name, ".jpg")),
      height = 720, width = 720
    )

    plot(NA,
      main = i_name,
      ylab = y_name,
      xlab = "time",
      xlim = c(1, md$T),
      ylim = c(
        min(apply(i_dat, 2, quantile, probs = c(0.025))),
        max(apply(i_dat, 2, quantile, probs = c(0.975)))
      )
    )

    for (t in 1:md$T) {
      j <- which(md$tt == t & md$ii == i)

      points(
        x = t,
        y = mean(fit$draws(paste0(y_name, "[", j, "]")))
      )

      arrows(
        x0 = t,
        x1 = t,
        y0 = quantile(fit$draws(paste0(y_name, "[", j, "]")), probs = c(0.025)),
        y1 = quantile(fit$draws(paste0(y_name, "[", j, "]")), probs = c(0.975)),
        length = 0,
        lty = 2
      )
    }
    dev.off()
  }
}



#---- trace plot checks ----#

dir.create(file.path(out_dir, model_name, "eval", "trace_plots"), showWarnings = F, recursive = T)


## global parameters
jpeg(
  filename = file.path(out_dir, model_name, "eval", "trace_plots", "global_parameters.jpg"),
  height = 720, width = 720
)

pars <- c("alpha_r", "beta_r", "sigma_r", "mu_p_F", "sigma_p_F", "mu_p_G", "sigma_p_G")
mcmc_trace(fit$draws(pars))

dev.off()


## location-time-specific parameters
for (i in 1:md$I) {
  for (t in 1:md$T) {
    i_name <- gsub(" ", "_", paste(i, t, unique(md$idx$i_name[md$idx$i == i])))

    jpeg(
      filename = file.path(out_dir, model_name, "eval", "trace_plots", paste0(i_name, ".jpg")),
      height = 720, width = 720
    )

    j <- which(md$tt == t & md$ii == i)

    pars <- c(paste0(c("N", "r", "p_F", "p_G"), "[", j, "]"))

    mcmc_trace(fit$draws(pars))

    dev.off()
  }
}
