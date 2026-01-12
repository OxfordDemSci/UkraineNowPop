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

dir.create(file.path(here::here(), "wd"), showWarnings = FALSE, recursive = TRUE)
setwd(file.path(here::here(), "wd"))

# Directories
repo_dir <- env$repo_dir
src_dir <- file.path(repo_dir, "src", "data_prep", "pop_data")
in_dir <- env$in_dir
out_dir <- file.path(env$out_dir, "modelling")

# Source functions
source(file.path(src_dir, "2_eval_fun.R"))

country <- "UA"
model_name <- "397_covs_model"


fit <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("fit_", model_name,".rds")))
md <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("md_", model_name, ".rds")))

dir.create(file.path(out_dir, model_name, "eval"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, model_name, "eval", "time_series_plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, model_name, "eval", "trace_plots"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, model_name, "eval", "population_pyramids"), showWarnings = FALSE, recursive = TRUE)

out_dir_idx <- file.path("/data/home/andrea/git/OxfordDemSci/UkraineNowPop/wd/out")
idx = read.csv(file.path(out_dir_idx, paste0(tolower(country), "_master_index", ".csv"))) 


A <- 7
S <- 2
I <- 23
T <- 117
C_full <- I*A*S
C_adult <- I*(A-1)*S
C_young <- I*S


#selected_locations <- seq(1, 27, by = 1)[-c(11, 5, 13, 19)] %>% as.integer()

#selected_locations_key <- 
#  c(3800, 3801, 3781, 3804, 3802, 3803, 3783, 3790, 3787,
#    3792, 3793, 3794, 3795, 3796, 3798, 3799, 3784, 3785,
#    3786, 3778, 3780, 3779, 4290) %>% 
#  as.integer()

#selected_locations_pcode <- c(#"UA01", "UA14", "UA44", "UA85",
#"UA71", "UA74", "UA73", "UA12", "UA26", "UA63", "UA65", "UA68", "UA35",
#"UA32", "UA46", "UA48", "UA51", "UA53", "UA56", "UA59", "UA61", "UA05",
#"UA07", "UA21", "UA23", "UA18", "UA80")
##############################
# Checking total population
##############################
names_N <- paste0("N_full[", 1:(md$T * md$C_full), "]")

draws_df <- fit$draws(variables = "N_full", format = "df")
head(names(draws_df))

N_draws <- as.matrix(draws_df[, grep("^N_full\\[", names(draws_df))])
N_means <- colMeans(N_draws)


N_t_mean <- sapply(seq_len(md$T), function(t) {
  idxs <- ((t - 1) * md$C_full + 1):(t * md$C_full)
  sum(N_means[idxs])
})

t_names <- md$idx %>%
  dplyr::distinct(t, t_name) %>%
  dplyr::arrange(t) %>%
  dplyr::pull(t_name)

df_tot <- data.frame(t_name = t_names,
                     observed  = md$y_N_tot, 
                     predicted = N_t_mean)
  
p <- ggplot(df_tot, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Total Population Check",
    x     = "Observed y_N_tot",
    y     = "Posterior mean total N"
  ) + theme_minimal()
p
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


i_idx <- idx %>%
  dplyr::select(i_key) %>%
  distinct() %>%
  filter(i_key %in% selected_locations_key) %>%
  arrange(i_key) %>%
  mutate(i = row_number())

idx1 <- expand.grid(t = 1:T, i = 1:I, a = 1:A, s = 1:S) %>% 
  left_join(i_idx) %>%
  arrange(t, i, a, s) %>%                       
  mutate(tias = row_number()) %>%               
  mutate(tias_adult = NA_integer_,
    tias_young = NA_integer_)                   

idx1$tias_adult[idx1$a != 7L] <- seq_len(sum(idx1$a != 7L))
idx1$tias_young[idx1$a == 7L] <- seq_len(sum(idx1$a == 7L))


plot_df_adult <- tibble(
  parameter = names(params_mean),
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) |>
  mutate(param    = sub("\\[.*", "", parameter),
    tias_adult = sub(".*\\[([^]]+)]$", "\\1", parameter) |> as.integer(),
  tias_young = NA_real_) %>%
filter(param == "N_adult"| param =="r" | param =="p_F"|param =="p_G") %>%
left_join(idx1 %>% dplyr::select(tias_adult, tias, t, i_key, a, s)) %>%
dplyr::select(parameter, predicted, lower, upper, param, tias, t, i_key, a, s, tias_young, tias_adult)


plot_df_young <- tibble(
  parameter = names(params_mean),
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) |>
  mutate(param    = sub("\\[.*", "", parameter),
    tias_young = sub(".*\\[([^]]+)]$", "\\1", parameter) |> as.integer(),
    tias_adult = NA_real_) %>%
filter(param %in% c("N_young")) %>%
left_join(idx1 %>% dplyr::select(tias_young, tias, t, i_key, a, s)) %>%
dplyr::select(parameter, predicted, lower, upper, param, tias, t, i_key, a, s, tias_young, tias_adult)
  


plot_df <- plot_df_adult %>% rbind(plot_df_young) %>%
  mutate(param = ifelse(param=="N_young"|param=="N_adult", "N", param)) %>%  
  left_join(idx %>% distinct(t, i, s, t_name, i_name, s_name, i_key)) %>%
  mutate(a_name = case_when(a == 7 ~ "00-14", 
                            a == 6 ~ "15-19",
                            a == 1 ~ "20-29",
                            a == 2 ~ "30-39",
                            a == 3 ~ "40-49",
                            a == 4 ~ "50-59",
                            a == 5 ~ "60+")) %>%
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


i_idx <- idx %>%
  dplyr::select(i_key) %>%
  distinct() %>%
  filter(i_key %in% selected_locations_key) %>%
  arrange(i_key) %>%
  mutate(i = row_number())

idx1 <- expand.grid(t = 1:T, i = 1:I, a = 1:A, s = 1:S) %>% 
  left_join(i_idx) %>%
  arrange(t, i, a, s) %>%                       
  mutate(tias = row_number()) %>%               
  mutate(tias_adult = NA_integer_,
    tias_young = NA_integer_)                   

idx1$tias_adult[idx1$a != 7L] <- seq_len(sum(idx1$a != 7L))
idx1$tias_young[idx1$a == 7L] <- seq_len(sum(idx1$a == 7L))


plot_df_adult <- tibble(
  parameter = names(params_mean),
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) |>
  mutate(param    = sub("\\[.*", "", parameter),
    tias_adult = sub(".*\\[([^]]+)]$", "\\1", parameter) |> as.integer()) %>%
filter(param %in% c("N_adult", "r", "p_F", "p_G")) %>%
left_join(idx1 %>% dplyr::select(tias_adult, tias, t, i_key, a, s)) %>%
dplyr::select(-tias_adult) %>%
dplyr::select(parameter, predicted, lower, upper, param, tias, t, i_key, a, s)


plot_df_young <- tibble(
  parameter = names(params_mean),
  predicted = params_mean,
  lower     = params_lower,
  upper     = params_upper
) |>
  mutate(param    = sub("\\[.*", "", parameter),
    tias_young = sub(".*\\[([^]]+)]$", "\\1", parameter) |> as.integer()) %>%
filter(param %in% c("N_young")) %>%
left_join(idx1 %>% dplyr::select(tias_young, tias, t, i_key, a, s)) %>%
dplyr::select(-tias_young) %>%
dplyr::select(parameter, predicted, lower, upper, param, tias, t, i_key, a, s)
      
        
plot_df <- plot_df_adult %>% rbind(plot_df_young) %>%
      mutate(param = ifelse(param=="N_young"|param=="N_adult", "N", param),
             pop = ifelse(s==1, -predicted, predicted),
             lower1 = ifelse(s==1, -lower, lower),
             upper1 = ifelse(s==1, -upper, upper)) %>%  
      left_join(idx %>% distinct(t, i, s, t_name, i_name, s_name, i_key)) %>%
      mutate(a_name = case_when(a == 6 ~ "00-14", 
                                a == 7 ~ "15-19",
                                a == 1 ~ "20-29",
                                a == 2 ~ "30-39",
                                a == 3 ~ "40-49",
                                a == 4 ~ "50-59",
                                a == 5 ~ "60+")) %>%
      filter(t %in% times_to_plot) %>%
      arrange(t,i,a,s)


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
pars_model <- c("N_adult", "N_young", "log_r", "p_F", "p_G")

t_values_to_plot <- seq(1, md$T, by = 20)

grid_info_all <- idx1 %>%
  left_join(
    idx %>% distinct(t, i, s, t_name, i_name, s_name, i_key),
    by = c("t", "i", "s")
  ) %>%
  mutate(
    a_name = case_when(
      a == 7 ~ "00-14",
      a == 6 ~ "15-19",
      a == 1 ~ "20-29",
      a == 2 ~ "30-39",
      a == 3 ~ "40-49",
      a == 4 ~ "50-59",
      a == 5 ~ "60+",
      TRUE ~ as.character(a)
    )
  ) %>%
  distinct(t, i, a, s, t_name, i_name, s_name, tias_adult, tias_young, a_name) %>%
  filter(t %in% t_values_to_plot) %>%
  arrange(t, i, a, s)

set.seed(123)

n_t_keep <- 3   
n_a_keep <- 3   
n_s_keep <- 2   

t_keep <- sample(t_values_to_plot, size = min(n_t_keep, length(t_values_to_plot)))
a_keep <- sample(sort(unique(grid_info_all$a)), size = min(n_a_keep, dplyr::n_distinct(grid_info_all$a)))
s_keep <- sample(sort(unique(grid_info_all$s)), size = min(n_s_keep, dplyr::n_distinct(grid_info_all$s)))

grid_info_all <- grid_info_all %>%
  filter(t %in% t_keep, a %in% a_keep, s %in% s_keep) %>%
  arrange(t, i, a, s)


include_warmup <- FALSE
n_warmup <- fit$metadata()$iter_warmup
if (is.null(n_warmup) || is.na(n_warmup)) n_warmup <- 0

for (row_id in seq_len(nrow(grid_info_all))) {

  grid_info <- slice(grid_info_all, row_id)

  loc_folder  <- gsub("\\s+", "_", grid_info$i_name)
  folder_path <- file.path(out_dir, model_name, "eval", "trace_plots", loc_folder)
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

  idx_adult <- grid_info$tias_adult   
  idx_young <- grid_info$tias_young   

  idx_for_par <- function(p) if (p == "N_young") idx_young else idx_adult

  pars_batch <- vapply(
    pars_model,
    function(p) sprintf("%s[%d]", p, idx_for_par(p)),
    character(1)
  )

  pars_batch <- pars_batch[!grepl("\\[NA\\]", pars_batch)]
  if (length(pars_batch) == 0) next

  draws <- fit$draws(
    variables  = pars_batch,
    inc_warmup = include_warmup
  )

  trace_plot <- bayesplot::mcmc_trace(
    draws,
    n_warmup = if (include_warmup) n_warmup else 0,
    facet_args = list(
      ncol = 4,
      labeller = label_parsed
    )
  )

  plot_label <- paste(
    gsub("\\s+", "_", grid_info$t_name),
    loc_folder,
    paste0("a", grid_info$a, "_", grid_info$a_name),
    paste0("s", grid_info$s, "_", gsub("\\s+", "_", grid_info$s_name)),
    sep = "__"
  )

  outfile <- file.path(folder_path, paste0("traceplot_", plot_label, ".jpg"))

  ggsave(
    filename = outfile,
    plot     = trace_plot,
    width    = 12,
    height   = max(3, length(pars_batch) * 2),
    dpi      = 300
  )
}


####################################################
# Other trace plots for location–time–specific parameters
####################################################
plot_traces <- function(fit, params, prefix,
  out_dir, model_name,
  ncol        = 5,
  n_warmup    = 300,
  inc_warmup  = FALSE,
  chunk_size  = 10,
  width       = 12,
  height      = 8) {

base_path <- file.path(out_dir, model_name, "eval", "trace_plots", prefix)
dir.create(base_path, recursive = TRUE, showWarnings = FALSE)

all_vars <- posterior::variables(fit$draws())
params   <- intersect(params, all_vars)

if (length(params) == 0L) {
message(sprintf("[plot_traces] No matching parameters for prefix '%s' — skipping.", prefix))
return(invisible(NULL))
}

chunks <- split(params, ceiling(seq_along(params) / chunk_size))

n_warmup_plot <- if (isTRUE(inc_warmup)) n_warmup else 0

for (i in seq_along(chunks)) {
pars_chunk <- chunks[[i]]
draws <- fit$draws(variables = pars_chunk, inc_warmup = inc_warmup)

plt <- bayesplot::mcmc_trace(
draws,
pars       = pars_chunk,
n_warmup   = n_warmup_plot,
facet_args = list(ncol = ncol, labeller = ggplot2::label_parsed)
) +
ggplot2::ggtitle(sprintf("%s: chunk %d / %d", prefix, i, length(chunks)))

ggplot2::ggsave(
filename = sprintf("%s_chunk%02d.jpg", prefix, i),
plot     = plt,
path     = base_path,
width    = width,
height   = height
)
}

invisible(NULL)
}

T   <- md$T
I   <- md$I
A   <- md$A
S   <- md$S
K_r <- md$K_r
K_p <- md$K_p

A_adult <- A - 1 
A_free  <- A - 2 

C_adult <- if (!is.null(md$C_adult)) md$C_adult else I * A_adult * S
N_adult_time <- T * C_adult  


pars_global <- c(
"alpha_r",
sprintf("beta_r[%d]", seq_len(K_r)),
"sigma_r",
"sigma_delta_r",
"sigma_gamma_r_i", "sigma_gamma_r_a", "sigma_gamma_r_s",

"alpha_p",
sprintf("beta_p[%d]", seq_len(K_p)),
"phi_p",
"sigma_delta_p",
"sigma_gamma_p_i", "sigma_gamma_p_a", "sigma_gamma_p_s",

"sigma_phi_p_a",
"sigma_F", "sigma_G"
)

plot_traces(
fit, pars_global,
prefix     = "01_global",
chunk_size = length(pars_global),
out_dir    = out_dir,
model_name = model_name
)

pars_rw_innov <- c(
sprintf("delta_r_innov_raw[%d]", seq_len(T - 1)),
sprintf("delta_p_innov_raw[%d]", seq_len(T - 1))
)

plot_traces(
fit, pars_rw_innov,
prefix     = "02_rw_innov_raw",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

every_t_steps <- unique(pmin(seq(2, T, by = 30), T))

plot_traces(
fit, sprintf("delta_r[%d]", every_t_steps),
prefix     = "03_delta_r_sample",
ncol       = 3,
chunk_size = length(every_t_steps),
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("delta_p[%d]", every_t_steps),
prefix     = "04_delta_p_sample",
ncol       = 3,
chunk_size = length(every_t_steps),
out_dir    = out_dir,
model_name = model_name
)

pars_gamma_r_raw <- c(
sprintf("gamma_r_i_raw[%d]", seq_len(I)),
sprintf("gamma_r_a_raw[%d]", seq_len(A_free)),
sprintf("gamma_r_s_raw[%d]", seq_len(S))
)

plot_traces(
fit, pars_gamma_r_raw,
prefix     = "05_gamma_r_raw",
out_dir    = out_dir,
model_name = model_name
)

pars_gamma_p_raw <- c(
sprintf("gamma_p_i_raw[%d]", seq_len(I)),
sprintf("gamma_p_a_raw[%d]", seq_len(A_free)),
sprintf("gamma_p_s_raw[%d]", seq_len(S))
)

plot_traces(
fit, pars_gamma_p_raw,
prefix     = "06_gamma_p_raw",
out_dir    = out_dir,
model_name = model_name
)

pars_gamma_r_expanded <- c(
sprintf("gamma_r_i[%d]", seq_len(I)),
sprintf("gamma_r_a[%d]", seq_len(A_adult)),
sprintf("gamma_r_s[%d]", seq_len(S))
)

plot_traces(
fit, pars_gamma_r_expanded,
prefix     = "07_gamma_r_expanded",
out_dir    = out_dir,
model_name = model_name
)

pars_gamma_p_expanded <- c(
sprintf("gamma_p_i[%d]", seq_len(I)),
sprintf("gamma_p_a[%d]", seq_len(A_adult)),
sprintf("gamma_p_s[%d]", seq_len(S))
)

plot_traces(
fit, pars_gamma_p_expanded,
prefix     = "08_gamma_p_expanded",
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("phi_p_a_raw[%d]", seq_len(A_free)),
prefix     = "09_phi_p_a_raw",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("phi_p_a[%d]", seq_len(A_adult)),
prefix     = "10_phi_p_a_expanded",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("N_tot[%d]", every_t_steps),
prefix     = "11_N_tot_sample",
ncol       = 3,
chunk_size = length(every_t_steps),
out_dir    = out_dir,
model_name = model_name
)

set.seed(2025)
idx50 <- sample.int(N_adult_time, 50)

plot_traces(
fit, sprintf("log_r_raw[%d]", idx50),
prefix     = "12_log_r_raw_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("log_r[%d]", idx50),
prefix     = "13_log_r_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("r[%d]", idx50),
prefix     = "14_r_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("p_F[%d]", idx50),
prefix     = "15_pF_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("p_G[%d]", idx50),
prefix     = "16_pG_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("nu_r_log[%d]", idx50),
prefix     = "17_nu_r_log_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)

plot_traces(
fit, sprintf("nu_p[%d]", idx50),
prefix     = "18_nu_p_sample",
chunk_size = 10,
out_dir    = out_dir,
model_name = model_name
)



#####################################################
## Summary statistics
#####################################################
fit_summary <- fit$summary(.cores = ncores)
write.csv(fit_summary, file.path(out_dir, model_name, "eval", "fit_summary.csv"), row.names = FALSE)

not_converged <- fit_summary %>%
  filter(!is.na(rhat), round(rhat, 1) > 1.1) %>%
  mutate(var_base = sub("\\[.*$", "", variable)) %>%
  arrange(desc(rhat))

write.csv(not_converged, file.path(out_dir, model_name, "eval", "bad_pars.csv"), row.names = FALSE)

