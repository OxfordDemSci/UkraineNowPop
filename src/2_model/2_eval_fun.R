# libraries
library(bayesplot)
library(ggplot2)

# all functions assume global variables:  fit, md

#---- check total population ----#
plot_total_pop_fit <- function(outfile=NA){
  
  if(!is.na(outfile)){
    jpeg(
      filename = outfile,
      height = 720, width = 720
    )
  }
  
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
  
  if(!is.na(outfile)){
    dev.off()
  }
}


#---- in-sample posterior prediction ----#
plot_postpred_fit <- function(dat, hat, outfile=NA){
  
  if(!is.na(outfile)){
    jpeg(
      filename = outfile,
      height = 720, width = 720
    )
  }
  
  draws <- fit$draws(hat, format = "df") |> select(!starts_with("."))
  
  hat_mean <- apply(draws, 2, mean)
  hat_lower <- apply(draws, 2, quantile, probs = c(0.025))
  hat_upper <- apply(draws, 2, quantile, probs = c(0.975))
  
  plot(
    x = md[[dat]],
    y = hat_mean,
    main = "Posterior Preditive Check" ,
    xlab = paste0("Observed (", dat, ")"),
    ylab = paste0("Predicted (", hat, ")"),
    ylim = c(min(hat_lower), max(hat_upper))
  )
  
  for (i in 1:md$n_F) {
    arrows(
      x0 = md[[dat]][i],
      x1 = md[[dat]][i],
      y0 = hat_lower[i],
      y1 = hat_upper[i],
      length = 0
    )
  }
  
  abline(0, 1, col = "red")
  
  if(!is.na(outfile)){
    dev.off()   
  }
}


#---- time series plots ----#

# plotting function
plot_time_series <- function(fit, md, model_name, plot_vars = c("N", "r", "p_F", "p_G"), locs = 1:md$I) {
  outpath <- file.path(out_dir, model_name, "eval", "time_series_plots")
  vars_ti <- c("pi")

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
      if(y_name %in% vars_ti) {
        col_names <- paste0(y_name, "[", 1:md$T, ",", i, "]")
      } else {
        col_names <- paste0(y_name, "[", which(md$ii == i), "]")
      }
      draws <- fit$draws(col_names, format = "df")

      dat <- data.frame(mean = rep(NA, md$T), lower = NA, upper = NA)
      for (t in 1:md$T) {
        if(y_name %in% vars_ti) {
          col_name <- paste0(y_name, "[", t, ",", i, "]")
        } else {
          col_name <- paste0(y_name, "[", which(md$tt == t & md$ii == i), "]")
        }
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


#---- traceplots ----#

plot_trace <- function(params, outfile=NA){
  dat <- fit$draws(params)
  trace_plot <- bayesplot::mcmc_trace(dat)
  size <- sqrt(dim(dat)[3]) * 2
  
  if(!is.na(outfile)){
    ggplot2::ggsave(
      trace_plot,
      filename = outfile,
      width = max(6, size),
      height = max(6, size)
    )  
  } else {
    print(trace_plot)
  }  
}
