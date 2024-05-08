model {
    # Observation model
    for (i in 1:I) {
        for (a in 1:A) {
            for (s in 1:S) {
                for (t in 1:T) {
                  F[i,a,s,t] ~ dpois(exp(log_N[i,a,s,t]) * nu[i,t])
            }}}}
    
    # Population model[?]
    for (i in 1:I) {
        for (a in 1:A) {
            for (s in 1:S) {
                for (t in 1:T) {
                    log_N[i,a,s,t] ~ dnorm(u0[i], tau_N) 
                    
                    N[i,a,s,t] <- round(exp(log_N[i,a,s,t]))
                }}}}

    # Oblast-specific intercepts for log_N
    for(i in 1:I){
        u0[i] ~ dnorm(0, 1)  # Random intercepts
    }
        
    tau_N ~ dgamma(0.01, 0.01)  # Precision for (corrected) N
    
                
    # Log-normal link for probability of Facebook use
    for (i in 1:I) {
        for (t in 1:T) {
            nu[i, t] <- exp(log_nu[i, t])
            
            log_nu[i, t] ~ dnorm(mu_nu, tau_nu)
        }}
    
    mu_nu ~ dnorm(0, 0.001)  
    tau_nu ~ dgamma(0.01, 0.01)
    
    # Mobility patterns
    for (i in 1:I) {
      for (a in 1:A) {
       for (s in 1:S) {
         for (t in 1:T) {
                    
           for (j in 1:I) {
             logit(p_logit[i,j,a,s,t]) <- i[i] + j[j] + a[a] + s[s] + t[t] + contiguity1[i,j]
             
             p[i,j,a,s,t] <- exp(p_logit[i,j,a,s,t])/(1+p_logit[i,j,a,s,t])
             }
                    
           # Normalisation of probabilities
            for (j in 1:I) {
               p_norm[i,j,a,s,t] <- p[i,j,a,s,t] / sum(p[i,1:I,a,s,t])
       
               lambda_ijast[i,j,a,s,t] <- exp(log_N[i,a,s,t]) * p_norm[i,j,a,s,t]
       
               N_ijast[i,j,a,s,t] ~ dpois(lambda_ijast[i,j,a,s,t])
                    }
                }
            }
        }
    }
    }
