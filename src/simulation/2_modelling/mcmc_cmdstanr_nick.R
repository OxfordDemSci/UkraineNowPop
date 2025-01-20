# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)

# check working directory
getwd()

# working directory
wd <- file.path(getwd(), "wd")
dir.create(wd, recursive = T, showWarnings = F)

# define model name
model_name <- "8_prop_model_RW"

# directories
srcdir <- file.path("src", "simulation", "2_modelling")
outdir <- file.path(wd, "out", "simulation", model_name)
dir.create(outdir, showWarnings = F, recursive = T)


#---- configure model ----#

# soure model-specific config code
source(file.path(srcdir, "models", paste0(model_name, "_config.R")))

#---- MCMC for Bayesian model ----#

# MCMC configuration
chains <- 4
warmup <- 2e2
samples <- 2e2
inits <- lapply(1:chains, function(id) init_generator(md = md, chain_id = id))

# compile the stan model
mod <- cmdstan_model(file.path(srcdir, "models", paste0(model_name, ".stan")))

# run MCMC to sample from the posterior distribution of our model, given our data
fit <- mod$sample(
  data = md,
  parallel_chains = chains,
  init = inits,
  iter_sampling = samples,
  iter_warmup = warmup,
  save_warmup = TRUE,
  seed = md$seed
)

# save fitted model to disk
fit$save_object(file = file.path(outdir, paste0("fit_", model_name, ".rds")))


# fit <- readRDS(file.path(outdir, paste0('fit_', model_name, '.rds')))
# md  <- readRDS(file.path(outdir, paste0('md_', model_name, '.rds')))

#---- model eval ---#


# summaries and rhat check
fit_summary <- fit$summary()
print(fit_summary)

not_converged <- which(fit_summary[["rhat"]] > 1.1) # 1.01 is cutoff for publication quality
fit_summary[not_converged, ]


# plot estimates
sims <- fit$draws(format = "df")

hist(sims$`logit_p[1,1]`)
hist(sims$`logit_p[2,1]`)
hist(exp(sims$mu_p_GF + exp(2 * sims$log_sigma_p_GF) / 2))
# hist(sims$sigma_p_GF)
# hist(sims$sigma_p_F)
# hist(sims$sigma_p)
hist(sims$log_sigma_p_GF)
hist(sims$log_sigma_p_F)
hist(sims$log_sigma_p_F_t)
hist(sims$log_sigma_p_F_i)
hist(sims$log_sigma_p)
hist(sims$log_kappa_F)
hist(sims$log_kappa_G)

fg.hat <- apply(
  sims %>% select(starts_with("p_GF[")),
  2, function(x) {
    quantile(x, probs = c(0.5, 0.025, 0.975))
  }
)
plot(md$y_FG_ratio, fg.hat[1, md$ti_FG], pch = ".")
arrows(
  x0 = md$y_FG_ratio,
  x1 = md$y_FG_ratio,
  y0 = fg.hat[2, md$ti_FG],
  y1 = fg.hat[3, md$ti_FG],
  length = 0,
  lty = 2
)
lines(md$y_FG_ratio, md$y_FG_ratio, col = "red")
mean((md$y_FG_ratio <= fg.hat[3, md$ti_FG]) & (md$y_FG_ratio >= fg.hat[2, md$ti_FG]))


fg.hat <- apply(
  sims %>% select(starts_with("FG_ratio[")),
  2, function(x) {
    quantile(x, probs = c(0.5, 0.025, 0.975))
  }
)
plot(md$y_FG_ratio, fg.hat[1, ], pch = ".")
arrows(
  x0 = md$y_FG_ratio,
  x1 = md$y_FG_ratio,
  y0 = fg.hat[2, ],
  y1 = fg.hat[3, ],
  length = 0,
  lty = 2
)
lines(md$y_FG_ratio, md$y_FG_ratio, col = "red")
mean((md$y_FG_ratio <= fg.hat[3, ]) & (md$y_FG_ratio >= fg.hat[2, ]))


p_GF <- apply(
  sims %>% select(paste0("p_GF[", md$ti_FG, "]")),
  2, function(x) {
    quantile(x, probs = c(0.5, 0.025, 0.975))
  }
)
plot(md$y_FG_ratio, p_GF[1, ], pch = ".")
arrows(
  x0 = md$y_FG_ratio,
  x1 = md$y_FG_ratio,
  y0 = p_GF[2, ],
  y1 = p_GF[3, ],
  length = 0,
  lty = 2
)
abline(0, 1, col = "red")
mean((md$y_FG_ratio <= p_GF[3, ]) & (md$y_FG_ratio >= p_GF[2, ]))


plot(fg.hat[1,], p_GF[1, ], pch = ".")
arrows(
  x0 = fg.hat[1,],
  x1 = fg.hat[1,],
  y0 = p_GF[2, ],
  y1 = p_GF[3, ],
  length = 0,
  lty = 2
)
abline(0, 1, col="red")


f.hat <- apply(
  sims %>% select(starts_with("F_hat[")),
  2, function(x) {
    quantile(x, probs = c(0.5, 0.025, 0.975))
  }
)
plot(log(md$y_F), log(f.hat[1, ]), pch = ".")
arrows(
  x0 = log(md$y_F),
  x1 = log(md$y_F),
  y0 = log(f.hat[2, ]),
  y1 = log(f.hat[3, ]),
  length = 0,
  lty = 2
)
lines(log(md$y_F), log(md$y_F), col = "red")
mean((md$y_F <= f.hat[3, ]) & (md$y_F >= f.hat[2, ]))
# points(log(md$y_F),log(f.hat[2,]),col='red')
# points(md$y_F,f.hat[3,],col='blue')
# points(md$y_F,f.hat[2,],col='red')
# points(md$y_F,f.hat[3,],col='blue')

g.hat <- apply(
  sims %>% select(starts_with("G_hat[")),
  2, function(x) {
    quantile(x, probs = c(0.5, 0.025, 0.975))
  }
)
plot(log(md$y_G), log(g.hat[1, ]), pch = ".")
arrows(
  x0 = log(md$y_G),
  x1 = log(md$y_G),
  y0 = log(g.hat[2, ]),
  y1 = log(g.hat[3, ]),
  length = 0,
  lty = 2
)
lines(log(md$y_G), log(md$y_G), col = "red")
mean((md$y_G <= g.hat[3, ]) & (md$y_G >= g.hat[2, ]))


#---- time series plots ----#
dir.create(file.path(outdir, "time_series_plots"), showWarnings = F, recursive = T)

# plotting function
plot_time_series <- function(fit, md, model_name, plot_vars = c("N", "r", "p_F", "p_G"), locs = 1:md$I) {
  outpath <- file.path(outdir, "time_series_plots")

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


#--- trace plots ----#
dir.create(file.path(outdir, "trace_plots"))

## global parameters
pars <- c(
  "log_sigma_p", "mu_p_F0",
  "log_sigma_p_F_t", "log_sigma_p_F_i", "log_sigma_p_F",
  "mu_p_GF", "log_sigma_p_GF",
  "log_kappa_F", "log_kappa_G"
)
dat <- fit$draws(pars)
trace_plot <- mcmc_trace(dat)
size <- sqrt(dim(dat)[3]) * 2
ggplot2::ggsave(
  trace_plot,
  filename = file.path(outdir, "trace_plots", "0_global_parameters.jpg"),
  width = max(6, size),
  height = max(6, size)
)


## location-specific parameters
pars <- c("mu_p_F_i")
for (i in 1:length(pars)) {
  dat <- fit$draws(pars[i])
  trace_plot <- mcmc_trace(dat)
  size <- sqrt(dim(dat)[3]) * 2
  ggplot2::ggsave(
    trace_plot,
    filename = file.path(outdir, "trace_plots", paste0("0_location_parameters_", i, ".jpg")),
    width = max(6, size),
    height = max(6, size)
  )
}


## time-specific parameters
pars <- c("mu_p_F_t")
for (i in 1:length(pars)) {
  dat <- fit$draws(pars[i])
  trace_plot <- mcmc_trace(dat)
  size <- sqrt(dim(dat)[3]) * 2
  ggplot2::ggsave(
    trace_plot,
    filename = file.path(outdir, "trace_plots", paste0("0_time_parameters_", i, ".jpg")),
    width = max(6, size),
    height = max(6, size)
  )
}


## location-time-specific parameters
pars <- c("N", "p_F", "p_G", "p_GF", "mu_p_F")
for (i in 1:md$I) {
  for (t in 1:md$T) {
    i_name <- gsub(" ", "_", paste(i, t, unique(md$idx$i_name[md$idx$i == i])))
    j <- which(md$tt == t & md$ii == i)

    pars_select <- c(paste0(pars, "[", j, "]"))
    dat <- fit$draws(pars_select)
    trace_plot <- mcmc_trace(dat)
    size <- sqrt(dim(dat)[3]) * 2

    ggplot2::ggsave(
      trace_plot,
      filename = file.path(outdir, "trace_plots", paste0(i_name, ".jpg")),
      width = max(6, size),
      height = max(6, size)
    )
  }
}






# #---- observed vs predicted plots (long format) ----#

# plot_vars <- list(N = "N_true", p_F = "p_F_true", p_G = "p_G_true")

# for (p in 1:length(plot_vars)) {
#   y_name <- names(plot_vars)[p]
#   y_true_name <- plot_vars[[y_name]]

#   plot(NA,
#     main = y_name,
#     xlim = range(md[[y_true_name]]),
#     ylim = range(md[[y_true_name]]),
#     xlab = "observed",
#     ylab = "predicted"
#   )

#   y_hat <- apply(fit$draws(paste0(y_name, "[", 1:(md$T * md$I), "]"), format = "df"), 2, mean)
#   y_hat_lower <- apply(fit$draws(paste0(y_name, "[", 1:(md$T * md$I), "]"), format = "df"), 2, quantile, probs = c(0.025))
#   y_hat_upper <- apply(fit$draws(paste0(y_name, "[", 1:(md$T * md$I), "]"), format = "df"), 2, quantile, probs = c(0.975))

#   for (t in 2:md$T) {
#     for (i in 1:md$I) {
#       j <- which(md$tt == t & md$ii == i)

#       points(
#         x = md[[y_true_name]][t, i],
#         y = y_hat[j]
#       )

#       arrows(
#         x0 = md[[y_true_name]][t, i],
#         x1 = md[[y_true_name]][t, i],
#         y0 = y_hat_lower[j],
#         y1 = y_hat_upper[j],
#         length = 0
#       )
#     }
#   }
#   abline(0, 1, col = "red")
# }

# #---- check total population ----#
# names_N_tot <- paste0("N_tot[", 1:md$T, "]")
# N_tot <- apply(fit$draws(names_N_tot, format = "df"), 2, mean)

# plot(
#   x = md$y_N_tot,
#   y = N_tot[names_N_tot]
# )
# abline(0, 1, col = "red")




# #---- trace plot checks (long format) ----#

# i <- sample(1:md$I, 1)
# t <- sample(2:md$T, 1)

# j <- which(md$tt == t & md$ii == i)


# mcmc_trace(fit$draws(paste0("N[", j, "]")))
# print(mean(fit$draws(paste0("N[", j, "]"))))
# print(md$N_true[t, i])

# mcmc_trace(fit$draws(paste0("r[", j, "]")))
# print(mean(fit$draws(paste0("r[", j, "]"))))
# print(exp(md$r_true[t, i]))

# mcmc_trace(fit$draws("sigma_r"))
# print(mean(fit$draws("sigma_r")))

# mcmc_trace(fit$draws("alpha_r"))
# print(mean(fit$draws("alpha_r")))

# mcmc_trace(fit$draws("beta_r"))
# md$beta_r_true

# for (k in 1:md$K) {
#   print(mcmc_trace(fit$draws(paste0("beta_r[", k, "]"))))
#   print(mean(fit$draws(paste0("beta_r[", k, "]"))))
#   print(md$beta_r_true[paste0("beta", k)])
# }



# mcmc_trace(fit$draws(paste0("p_F[", j, "]")))
# print(mean(fit$draws(paste0("p_F[", j, "]"))))
# print(mean(md$p_F_true[t, i]))

# mcmc_trace(fit$draws("alpha_p_F"))
# print(mean(fit$draws("alpha_p_F")))

# mcmc_trace(fit$draws("sigma_p_F"))
# print(mean(fit$draws("sigma_p_F")))




# mcmc_trace(fit$draws(paste0("p_G[", j, "]")))
# print(mean(fit$draws(paste0("p_G[", j, "]"))))
# print(mean(md$p_G_true[t, i]))

# mcmc_trace(fit$draws("alpha_p_G"))
# print(mean(fit$draws("alpha_p_G")))

# mcmc_trace(fit$draws("sigma_p_G"))
# print(mean(fit$draws("sigma_p_G")))








# #---- quick observed vs predicted plot (array format) ----#
# plot(NA,
#   xlim = range(md$N_true),
#   ylim = range(md$N_true),
#   xlab = "observed N",
#   ylab = "predicted N"
# )

# for (t in 2:md$T) {
#   for (i in 1:md$I) {
#     points(
#       x = md$N_true[t, i],
#       y = mean(fit$draws(paste0("N[", t, ",", i, "]")))
#     )
#   }
# }
# abline(0, 1, col = "red")


# #---- quick checks (array format)----#

# print(fit, max_rows = 1e3)
# summary(fit$summary()[["rhat"]])

# i <- sample(1:md$I, 1)
# t <- sample(2:md$T, 1)

# mcmc_trace(fit$draws(paste0("N[", t, ",", i, "]")))
# print(mean(fit$draws(paste0("N[", t, ",", i, "]"))))
# print(md$N_true[t, i])

# mcmc_trace(fit$draws(paste0("r[", t, ",", i, "]")))
# print(mean(fit$draws(paste0("r[", t, ",", i, "]"))))

# mcmc_trace(fit$draws("mu"))
# print(mean(fit$draws("mu")))

# mcmc_trace(fit$draws("sigma"))
# print(mean(fit$draws("sigma")))


# mcmc_trace(fit$draws(paste0("p[", t, ",", i, "]")))
# print(mean(fit$draws(paste0("p[", t, ",", i, "]"))))
# print(mean(md$p_true[t, i]))

# mcmc_trace(fit$draws(paste0("delta[", t, "]")))
# print(mean(fit$draws(paste0("delta[", t, "]"))))

# mcmc_trace(fit$draws("mu_delta"))
# print(mean(fit$draws("mu_delta")))

# mcmc_trace(fit$draws("sd_delta"))
# print(mean(fit$draws("sd_delta")))

# mcmc_trace(fit$draws("alpha"))
# print(mean(fit$draws("alpha")))




# #---- old code -----#

# mcmc_trace(fit$draws("N_sum"))
# mean(fit$draws("N_sum"))
# md$N_tot


# mcmc_trace(fit$draws("p"))
# mean(fit$draws("p"))
# mean(md$p_true)

# for (i in 1:md$I) {
#   print(mean(fit$draws(paste0("N[", i, "]"))))
#   print(md$N_true[i])
#   print("")
# }
