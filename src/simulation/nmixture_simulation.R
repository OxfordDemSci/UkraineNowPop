# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# check working directory
getwd()

# output directory
outdir <- file.path('wd', 'out', 'simulation')
dir.create(outdir, showWarnings=F, recursive=T)


#---- simulation parameters ----#

# number of locations
I = 10

# number of repeat observations
M = 3
  
# number of time steps
T = 5


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


#---- simulated surveys ----#

# detection probabilities
detection <- array(runif(I*T, 0.1, 0.15), dim=c(T,I))

# audience sizes
audience <- array(NA, dim=c(T,I,M))
for(t in 1:T){
  for(i in 1:I){
    for(m in 1:M){
      audience[t,i,m] <- rbinom(1, population[t,i], detection[t,i])
    }
  }
}


#---- missing data ----#

M_ti <- matrix(M, nrow=T, ncol=I)

for(t in 1:T){
  for(i in 1:I){
    drop <- rbinom(1, M-1, 0.05)
    if(drop>0){
      audience[t,i,(M+1-drop):M] <- NA
    }
    M_ti[t,i] <- M - drop
  }
}



#---- save to disk ----#

# model data
md <- list(I = I,
           T = T,
           M = M_ti,
           y = audience,
           p_true = detection,
           N_true = population,
           seed=round(runif(1, 1, 1e6)))

saveRDS(md, file.path(outdir, 'md.rds'))


