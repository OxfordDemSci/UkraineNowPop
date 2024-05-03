model {
    # Observation model: N-mixture[?] model for estimating population
    for (i in 1:I) {
        for (a in 1:A) {
            for (s in 1:S) {
                for (t in 1:T) {
                   F[i,a,s,t] ~ dpois(exp(log_N[i,a,s,t]) * nu[i,t])
                }}}}
    
    # Smoothing true unobserved flows
    # First point in time
    for (i in 1:I) {
        for (a in 1:A) {
            for (s in 1:S) {
                for (t in 1:30) {
                    log_N[i,a,s,t] ~ dnorm(u0[i], tau_N)                                          
                }}}}
                
    # Rest 
    for (i in 1:I) {
        for (a in 1:A) {
            for (s in 1:S) {
                for (t in 31:T) {
                    log_N[i,a,s,t] ~ dnorm(u0[i], tau_N)         
                }}}}
    
                
              
    # Oblast-specific intercepts for log_N
    for(i in 1:I){
        u0[i] ~ dnorm(0, 1)  # Random intercepts
        }    
                
    # Log-normal link for probability of Facebook use
    for (i in 1:I) {
        for (t in 1:T) {
            nu[i,t] ~ dlnorm(mu_nu[i,t], tau_p)  
            
            mu_nu[i,t] <- alpha_0 + alpha_t[t]
        }}
    
    # Priors for parameters affecting nu[i,t]
    alpha_0 ~ dnorm(0, 0.01)   # Prior for intercept
    for (t in 1:T) {
        alpha_t[t] ~ dnorm(0, 1)  # time-specific effects
    }
    
  #Modelling mobility patterns  
    for (i in 1:I) {
     for (a in 1:A) {
      for (s in 1:S) {
       for (t in 1:(T-1)) {
          
          p[i,1:I,a,s,t] ~ ddirch(alpha_prior[i,1:I,a,s,t])
                
       for (j in 1:I) {
          # Multinomial transitions with auxiliary data
          N_ijast[i,j,a,s,t] ~ dmulti(p[i,j,a,s,t], round(exp(log_N[i,a,s,t]))
     }}}}}
                                      
    for (i in 1:I) {
     for (a in 1:A) {
       for (s in 1:S) {
         for (t in 1:T) {
           for (j in 1:I) {
             alpha_prior[i,j,a,s,t] <- exp(beta_0 + beta[j]*contiguity[i,j,a,s,t])
             
        }}}}}

        beta_0 ~ dnorm(0, 1)
        
        for(j in 1:I){
          beta[j] ~ dnorm(0, 1) 
        }
        
        tau_mu_N ~ dgamma(0.01, 0.01)  # Precision for uncorrected N
        tau_N ~ dgamma(0.01, 0.01)  # Precision for corrected N
        
        tau_p ~ dgamma(0.01, 0.01)     # Precision for scaling factor 
    
}
