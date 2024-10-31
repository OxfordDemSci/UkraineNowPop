# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# check working directory
getwd()

# output directory
outdir <- file.path('wd', 'out', 'simulation')
dir.create(outdir, showWarnings=F, recursive=T)


#---- simulation parameters ----#

# number of locations
I = 27

# number of repeat observations
M = 7
  
# number of time steps
T = 20


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
detection <- array(runif(I*T, 0.1, 0.25), dim=c(T,I))

# audience sizes
audience <- array(NA, dim=c(T,I,M))
for(t in 1:T){
  for(i in 1:I){
    for(m in 1:M){
      audience[t,i,m] <- rbinom(1, population[t,i], detection[t,i])
    }
  }
}


#---- save to disk ----#

# simulation
sim <- list(population=population,
            migration=migration,
            detection=detection,
            audience=audience)
saveRDS(sim, file.path(outdir, 'sim.rds'))

# model data
md <- list(I=I,
           T=T,
           M=M,
           population_baseline=population[1,],
           audience=audience,
           seed=round(runif(1, 1, 1e6)))

saveRDS(md, file.path(outdir, 'md.rds'))


