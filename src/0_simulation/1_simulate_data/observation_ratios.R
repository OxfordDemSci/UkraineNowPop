N <- sample(1e3:1e5, 1)

p1 <- runif(1, 0, 2)
p2 <- runif(1, 0, 2)

M <- sample(1:100, 1)

y1 <- rpois(M, N * p1)
y2 <- rpois(M, N * p2)

p1
p2 * (y1 / y2)

prior <- rlnorm(1e5, log(mean(y1/y2)), 0.01)
plot(density(prior))
legend('topleft', 
       legend=c(paste0('N = ',N),
                paste0('p1 = ',round(p1,3)),
                paste0('p2 = ',round(p2,3)),
                paste0('M = ',M)))
abline(v=p1/p2, col='red')


# p1_hat <- rlnorm(1e5, log(mean(p2 * (y1/y2))), 0.01)
# plot(density(p1_hat))
# legend('topleft', 
#        legend=c(paste0('N = ',N),
#                 paste0('p1 = ',round(p1,3)),
#                 paste0('p2 = ',round(p2,3)),
#                 paste0('J = ',J)))
# rug(p2 * (y1 / y2))
# abline(v=p1, col='red')
 

