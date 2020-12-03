x = c(12,13,11,9,13,15,12,14,12,12)
hist(x)
n = length(x)
k = 100000
simsamples = replicate(k, rnorm(n, mean(x), sd(x)))
sim = apply(simsamples,2,mean)
length(sim)
quantile(sim, c(0.05, 0.95))
