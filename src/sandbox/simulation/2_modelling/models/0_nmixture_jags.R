model {
  
  # likelihood
  for(i in 1:I){
    for(m in 1:M){
      F[i,m] ~ dpois(N[i] * rho)
    }
  }
  
  # total population constraint
  N_tot ~ dnorm(sum(N), pow(1,-2))
  
  # latent population process
  for(i in 1:I){ 
    N[i] ~ dpois(lambda[i])
    lambda[i] ~ dgamma(0.001, 0.001)
  }

  # priors
  rho ~ dbeta(1, 1)
}
