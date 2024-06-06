data {
    int<lower=0> I; // number of oblasts i
    int<lower=0> J; // number of oblasts j (dim i=j)
    vector<lower=0>[I] row_margins;  // known margin for orig i
    vector<lower=0>[J] col_margins;  // known margin for dest j
}

parameters {
    matrix<lower=0>[I, J] cell_counts;    // unknown cell counts
}
model {
    // Define the log-linear model
    
    for (i in 1:I) {
        for (j in 1:J) {
         if (i == j) {
            log(cell_counts[i, j]) ~ normal(0, 1); //stayers
            
         } else {
            log(cell_counts[i, j]) ~ normal(0, 1); //movers
            
        }
      }
    }
    
   // Priors for the lambda parameters
    lambda_diag ~ gamma(0.01, 0.01); 
    lambda_off_diag ~ gamma(0.01, 0.01); 
    

   // Constraints to match margins
    for (i in 1:I) {
        sum(cell_counts[i, ]) ~ normal(row_margins[i], 0.1);   //allow small variance
    }
    
    for (j in 1:J) {
        sum(cell_counts[, j]) ~ normal(col_margins[j], 0.1);   //allow small variance
    }
    
}
