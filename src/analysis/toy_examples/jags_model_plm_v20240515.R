model {
    ######################
    # Observation model
    ######################
    for (j in 1:I) {
        for (a in 1:A) {
            for (s in 1:S) {
                for (t in 1:T) {
                  
                  log_F[j,a,s,t] ~ dnorm( log_N[j,a,s,t] + log_nu_F[j,a,s,t], tau_F)
                  log_nu_F[j,a,s,t] ~ dnorm(mu_nuF, tau_nuF)
                  
                  
                  log_G[j,a,s,t] ~ dnorm(log_N[j,a,s,t] + log_nu_G[j,a,s,t], tau_G) 
                  log_nu_G[j,a,s,t] ~ dnorm(mu_nuG, tau_nuG)              
                  
                  
            }}}}
    
    mu_nuF ~ dnorm(0, 1)
    mu_nuG ~ dnorm(0, 1)
    
    tau_F <- 1/pow(sigma_F, 2)
    sigma_F ~ dnorm(0, 1)T(0,)
    
    tau_nuF <- 1/pow(sigma_nuF, 2)
    sigma_nuF ~ dnorm(0, 1)T(0,)
    
    tau_G <- 1/pow(sigma_G, 2)
    sigma_G ~ dnorm(0, 1)T(0,)
    
    tau_nuG <- 1/pow(sigma_nuG, 2)
    sigma_nuG ~ dnorm(0, 1)T(0,)
    
    
    
    ########################
    #Population model
    ########################
    for (j in 1:I) {
        for (a in 1:A) {
            for (s in 1:S) {
                for (t in 1:30) {
                  log_N[j,a,s,t] ~ dnorm(log_N0_obs[j,a,s], tau_N)
                }}}}
                
    for (j in 1:I) {
        for (a in 1:A) {
            for (s in 1:S) {
                for (t in 31:T) {
                  log_N[j,a,s,t] ~ dnorm(log_N[j,a,s,t-1], tau_N)
                }}}}
    
    for(j in 1:I){
        u0[j] ~ dnorm(0, tau_int)
        u1[j] ~ dnorm(0, tau_slope)  
    }
    
    tau_int <- 1/pow(sigma_int, 2)
    sigma_int ~ dnorm(0, 1)T(0,)
    
    tau_slope <- 1/pow(sigma_slope, 2)
    sigma_slope ~ dnorm(0, 1)T(0,)
        
    tau_N <- 1/pow(sigma_N, 2)
    sigma_N ~ dnorm(0, 1)T(0,)
    
    
    #######################
    # Mobility patterns
    #######################
    for (j in 1:I) {
      for (a in 1:A) {
        for (s in 1:S) {
          for (t in 1:T) {
    
            for (i in 1:I) {
              logit(p_logit[i,j,a,s,t]) <- ifelse(i == j,
                
                beta_s,                         #push factors - stayers
                
                beta_m + contiguity1[i,j])          #pull factors - movers
                
              p[i,j,a,s,t] <- exp(p_logit[i,j,a,s,t]) / (1 + exp(p_logit[i,j,a,s,t]))
            }
    
            # Normalisation of probabilities
              for (i in 1:I) {
                   p_norm[i,j,a,s,t] <- p[i,j,a,s,t] / sum(p[1:I,j,a,s,t])
           
                   lambda_ijast[i,j,a,s,t] <- exp(log_N[j,a,s,t])*p_norm[i,j,a,s,t]
           
                   N_ijast[i,j,a,s,t] ~ dpois(lambda_ijast[i,j,a,s,t])
                        }
          }
        }
      }
    }
    
    beta_s ~ dnorm(0, 1)
    beta_m ~ dnorm(0, 1)
}





                    
