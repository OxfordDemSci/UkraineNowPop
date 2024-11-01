# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# check working directory
getwd()

# output directory
outdir <- file.path('wd', 'out', 'simulation')
dir.create(outdir, showWarnings=F, recursive=T)


#---- simulation parameters ----#

# number of time steps
T = 20

# number of locations
I = 10

# number of repeat observations
M = 7

  

# #---- simulate covariates ----#
# 
# # number of covariates
# K <- 2
# 
# # create covariates
# x <- matrix(NA, nrow=n, ncol=K)
# for(k in 1:K){
#   x[,k] <- rnorm(n, 0, 1)
# }
# 
# # define regression parameters
# alpha <- rnorm(1, 0, 1)
# beta <- rnorm(K, 0, 3)
# sigma <- runif(1, 0, 2)
# 
# # generate response variable
# y <- alpha + as.vector(x %*% beta) + rnorm(n, 0, sigma)
# 

#---- simulated population ----#

# migration
migration <- array(runif(I*I*T, 0, 1), dim=c(T,I,I))
for(t in 1:T){
  for(i in 1:I){
    migration[t,i,i] <- I
    migration[t,,i] <- migration[t,,i] / sum(migration[t,,i])
  }
}


# population
population <- matrix(NA, nrow=T, ncol=I)
population[1,] <- round(runif(I, 1e5, 1e6))
for(t in 2:T){
  population[t,] <- round(migration[t,,] %*% population[t-1,])
}


# growth rates
cov_growth <- growth <- matrix(NA, nrow=T, ncol=I)
growth[1,] <- 0
for(i in 1:I){
  growth[2:T,i] <- log(population[2:T, i] / population[1:(T-1), i])
} 

# covariates on growth rates
K <- 2
cov_growth <- array(NA, dim=c(T, I, K))
sd_k <- runif(K, 0.02, 0.1)
for(k in 1:K){
  sign <- sample(c(-1,1), 1)
  for(i in 1:I){
    cov_growth[,i,k] <- rnorm(T, sign * growth[,i], sd_k[k])
  }
  plot(growth~cov_growth[,,k])
  legend('topleft', legend=c(paste0('SD = ', round(sd_k[k], 3))))
  abline(0,1*sign)
}


#---- simulated surveys ----#

# detection probabilities
detection1 <- array(runif(I*T, 0.05, 0.15), dim=c(T,I))
detection2 <- array(runif(I*T, 0.15, 0.25), dim=c(T,I))

# audience sizes
audience1 <- array(NA, dim=c(T,I,M))
for(t in 1:T){
  for(i in 1:I){
    for(m in 1:M){
      audience1[t,i,m] <- rpois(1, population[t,i] * detection1[t,i])
    }
  }
}

audience2 <- array(NA, dim=c(T,I,M))
for(t in 1:T){
  for(i in 1:I){
    for(m in 1:M){
      audience2[t,i,m] <- rpois(1, population[t,i] * detection2[t,i])
    }
  }
}


#---- missing data ----#

M2_ti <- M1_ti <- matrix(M, nrow=T, ncol=I)

for(t in 1:T){
  for(i in 1:I){
    drop <- rbinom(1, M-1, 0.05)
    if(drop>0){
      audience1[t,i,(M+1-drop):M] <- NA
    }
    M1_ti[t,i] <- M - drop
  }
}

for(t in 1:T){
  for(i in 1:I){
    drop <- rbinom(1, M-1, 0.05)
    if(drop>0){
      audience2[t,i,(M+1-drop):M] <- NA
    }
    M2_ti[t,i] <- M - drop
  }
}

#---- estimate true regression parameters ----#

fit <- lm(as.vector(growth) ~ as.vector(cov_growth[,,1]) + as.vector(cov_growth[,,2]))
effects <- fit$coefficients
names(effects) <- c('alpha', paste0('beta', 1:2))


#---- save to disk ----#

# model data
md <- list(I = I,
           T = T,
           K = K,
           M1 = M1_ti,
           M2 = M2_ti,
           y1 = audience1,
           y2 = audience2,
           X = cov_growth,
           p1_true = detection1,
           p2_true = detection2,
           N_true = population,
           r_true = growth,
           beta_r_true = effects,
           seed=round(runif(1, 1, 1e6)))

saveRDS(md, file.path(outdir, 'simulated_data.rds'))


