# load data
mddir <- file.path(file.path(wd, 'out', 'population_proxy', 'model_data'))
md <- readRDS(file.path(mddir, 'md.rds'))


# set seed for random number generators
if('seed' %in% names(md)){
  seed <- md$seed
} else {
  seed <- round(runif(1, 1, 1e6))
}
set.seed(seed)


#---- idx ----#


# meta_id of every location
locations <- sort(unique(c(md$locations_F, md$locations_G)))
md$I <- length(locations)

md$I_F <- NULL
md$I_G <- NULL


# dates
md$T <- max(md$T_F, md$T_G)

md$T_F <- NULL
md$T_G <- NULL

start_date <- as.Date("2022-02-21")
end_date <- start_date + (md$T - 1) * 7

first_monday <- start_date + (8 - as.integer(format(start_date, "%u"))) %% 7
weeks <- seq(from = first_monday, to = end_date, by = "week")


## master idx
idx <- data.frame(t = rep(1:md$T, each=md$I), 
                  i = rep(1:md$I, times=md$T),
                  ti = seq(1, md$I * md$T),
                  i_key = rep(locations, md$T),
                  t_key = rep(gsub('-', '', weeks), each=md$I))

md$idx <- idx

rm(locations, start_date, end_date, first_monday, weeks)


## idx_F
n_F <- prod(dim(md$y_F))

idx_F <- data.frame(t = rep(NA, n_F),
                    i = rep(NA, n_F),
                    m = rep(NA, n_F),
                    ti_F = rep(NA, n_F),
                    value = rep(NA, n_F))

cnt <- 0
for(t in 1:dim(md$y_F)[1]) {
  t_key <- unique(idx[idx$t == t, 't_key'])
  for(i in 1:dim(md$y_F)[2]) {
    i_key <- md$locations_F[i]
    for(m in 1:dim(md$y_F)[3]) {
      cnt <- cnt + 1
      idx_F[cnt,'t'] <- unique(idx$t[idx$t_key == t_key])
      idx_F[cnt,'i'] <- unique(idx$i[idx$i_key == i_key])
      idx_F[cnt,'m'] <- m
      idx_F[cnt,'ti_F'] <- idx$ti[idx$t_key==t_key & idx$i_key==i_key]
      idx_F[cnt,'value'] <- md$y_F[t,i,m]
    }
  }
}
idx_F <- idx_F[!is.na(idx_F$value) & idx_F$value > 0,]

# drop
md$locations_F <- NULL
md$M_F <- NULL
rm(n_F, cnt,t, t_key, i, i_key, m)


## idx_G
n_G <- prod(dim(md$y_G))

idx_G <- data.frame(t = rep(NA, n_G),
                    i = rep(NA, n_G),
                    m = rep(NA, n_G),
                    ti_G = rep(NA, n_G),
                    value = rep(NA, n_G))

cnt <- 0
for(t in 1:dim(md$y_G)[1]) {
  t_key <- unique(idx[idx$t == t + md$T - dim(md$y_G)[1], 't_key'])
  for(i in 1:dim(md$y_G)[2]) {
    i_key <- md$locations_G[i]
    for(m in 1:dim(md$y_G)[3]) {
      cnt <- cnt + 1
      idx_G[cnt,'t'] <- unique(idx$t[idx$t_key == t_key])
      idx_G[cnt,'i'] <- unique(idx$i[idx$i_key == i_key])
      idx_G[cnt,'m'] <- m
      idx_G[cnt,'ti_G'] <- idx$ti[idx$t_key==t_key & idx$i_key==i_key]
      idx_G[cnt,'value'] <- md$y_G[t,i,m]
    }
  }
}
idx_G <- idx_G[!is.na(idx_G$value) & idx_G$value > 0,]

# drop
md$locations_G <- NULL
md$M_G <- NULL
rm(n_G, cnt,t, t_key, i, i_key, m)



#---- prepare data ----#

# Facebook 
md$idx_F <- idx_F
md$y_F <- idx_F$value
md$n_F <- length(md$y_F)
md$ti_F <- idx_F$ti_F

# Instagram
md$idx_G <- idx_G
md$y_G <- idx_G$value
md$n_G <- length(md$y_G)
md$ti_G <- idx_G$ti_G

rm(idx_F, idx_G)

# Facebook:Instagram ratio
md$ti_FG <- unique(md$ti_F[which(md$ti_F %in% md$ti_G)])
md$n_FG <- length(md$ti_FG)

md$y_FG_ratio <- c()
for(i in 1:length(md$ti_FG)){
  ti <- md$ti_FG[i]
  G_ti <- mean(md$y_G[which(md$ti_G == ti)])
  F_ti <- mean(md$y_F[which(md$ti_F == ti)])
  md$y_FG_ratio[i] <- G_ti / F_ti
}

drop <- which(!is.finite(md$y_FG_ratio))
if(length(drop) > 0){
  md$y_FG_ratio <- md$y_FG_ratio[-drop]
  md$ti_FG <- md$ti_FG[-drop]
  md$n_FG <- md$n_FG - length(drop)
}

rm(drop, F_ti, G_ti, i)


## population ##

# baseline population
md$N0 <- codps[as.character(unique(idx$i_key[order(idx$i)])),'T_TL']

# total population at each time step
weekly_avg <- outside_border |>
  mutate(week = floor_date(as.Date(date), 'week', week_start=1)) |>
  group_by(week) |>
  summarise(avg_value = mean(individuals, na.rm=TRUE)) |>
  filter(week >= min(as.Date(md$idx$t_key, format='%Y%m%d')) & 
           week <= max(as.Date(md$idx$t_key, format='%Y%m%d')))

md$y_N_tot <- sum(md$N0) - weekly_avg$avg_value

rm(weekly_avg)


# indexing: long format for N
md$ti_N0 <- idx$ti[idx$t==1]
md$ti_N <- idx$ti[idx$t>1]
md$ti_N_lag <- idx$ti[idx$t>1]-md$I

md$tt <- idx$t
md$ii <- idx$i


## covariates
md$K <- 2
md$X <- matrix(0, nrow=nrow(idx), ncol=md$K)
colnames(md$X) <- paste0('x', 1:md$K)
rownames(md$X) <- idx$ti



rm(idx, ti)

## save to disk ##
saveRDS(md, file.path(outdir, paste0('md_', model_name, '.rds')))





#---- initial values ----#
init_generator <- function(md=md, chain_id=1){
  result <- list()
  
  N <- matrix(NA, nrow=md$T, ncol=md$I)
  theta <- rep(NA, md$I)
  for(t in 1:md$T){
    for(i in 1:md$I){
      ti <- unique(md$idx$ti[md$idx$t==t & md$idx$i==i])
      if(any(md$ti_F %in% ti)){
        theta[i] <- max(md$y_F[which(md$ti_F %in% ti)], na.rm=T) / md$y_N_tot[t]
      }
    }
    theta[!is.finite(theta)] <- mean(theta[is.finite(theta)])
    
    theta <- theta / sum(theta)
    
    N[t,] <- rbinom(md$I, md$y_N_tot[t], theta)
  }

  result[['N_tot']] <- md$y_N_tot
  result[['N']] <- reshape2::melt(N, varnames=c('T', 'I'))$value
  result[['r']] <- rlnorm(md$T * md$I, 0, 0.1)
  result[['sigma_r']] <- runif(1, 0, 0.5)
  result[['mu_r']] <- rnorm(md$T * md$I, 0, 0.1)
  result[['alpha_r']] <- rnorm(1, 0, 3)
  result[['beta_r']] <- rnorm(md$K, 0, 1)

  result[['p_F']] <- rlnorm(md$T * md$I, log(mean(md$y_F, na.rm=T)/mean(md$N0)), 0.5)
  result[['mu_p_F']] <- rnorm(1, 1, 1)
  result[['sigma_p_F']] <- runif(1, 0, 0.05)

  result[['p_G']] <- rlnorm(md$T * md$I, log(mean(md$y_G, na.rm=T)/mean(md$N0)), 0.5)
  result[['mu_p_G']] <- rnorm(1, 1, 1)
  result[['sigma_p_G']] <- runif(1, 0, 0.05)

  return(result)
}

