# load libraries
library(randomForest)
library(boot)
library(parallel)

# load scripts
source("functions/ICP.R")
source("functions/bootstrap.R")
source("functions/bootstrap_parallel.R")
source("functions/plot_prediction_bands.R")
source("functions/normalize.R")
source("functions/split_data.R")

# load full dataset and normalize target
load("synth_data_50.RData")
dataset$y = normalize(dataset$y)

# create dataframe to hold results
n_seq = seq(1000, 10000, by = 500)
results = data.frame(matrix(nrow = length(n_seq), ncol = 7))
colnames(results) = c('size', 'icp_covrate', 'icp_mean_interval_size', 'icp_runtime', 
                      'bs_covrate', 'bs_mean_interval_size', 'bs_runtime')



# set up parameters
n = 1000 # set this in the for loop
conf = 0.95
formula = formula(y ~ .)
beta = 0.01
R = 500
ntree = 125
cores = 8


# temporary n_seq
n_seq = seq(1000, 3000, by = 500)
#n_seq = 1000
############### for loop start
for (i in 1:length(n_seq)) {
  n = n_seq[i]
  results$size[i] = n
  print(n)
  
  # split data into train and test sets
  splits = split_data(dataset, n)
  train = splits$train
  test = splits$test
  
  # ICP
  res_icp = ICP(train, test, formula = formula, normalized = TRUE, beta = beta, ntree = ntree, conf = conf)
  
  results$icp_covrate[i] = res_icp$test_coverage_rate
  results$icp_mean_interval_size[i] = res_icp$mean_interval_size
  results$icp_runtime[i] = res_icp$runtime
  
  print(res_icp$runtime)
  
  
  # BOOTSTRAP PARALLEL
  res_bs = bootstrap_parallel(train, test , formula = formula, conf = conf, ntree = ntree, R = R, cores = cores)
  
  # n = 1000 : 117
  
  results$bs_covrate[i] = res_bs$test_coverage_rate_e
  results$bs_mean_interval_size[i] = res_bs$mean_interval_size_e
  #res_bs$test_coverage_rate_q
  #res_bs$mean_interval_size_q
  results$bs_runtime[i] = res_bs$runtime
  
  print(res_bs$runtime)
  
}
############### for loop end

save(results, file = "results.RData")
load('results.RData')

plot(results$size, results$icp_covrate, type = 'l', ylim = c(0.55,1), col = 'blue', ylab = 'Coverage rate', xlab = 'Data size')
points(results$size, results$bs_covrate, type = 'l', col = 'red')
abline(h = 0.95, lty = 2)
legend('left', legend=c('ICP', 'BS'), lty=c(1,1), col=c('blue', 'red'))

plot(results$size, results$icp_mean_interval_size, type = 'l', ylim = c(0,0.3), col = 'blue', xlab = 'Data size', ylab = 'Mean interval region size')
points(results$size, results$bs_mean_interval_size, type = 'l', col = 'red')
legend('bottom', legend=c('ICP', 'BS'), lty=c(1,1), col=c('blue', 'red'))

plot(results$size, results$icp_runtime, type = 'l', ylim = c(1,1000), col = 'blue', xlab = 'Data size', ylab = 'Runtime')
points(results$size, results$bs_runtime, type = 'l', col = 'red')
legend('topleft', legend=c('ICP', 'BS'), lty=c(1,1), col=c('blue', 'red'))





#####
par(mar = c(5,5,2,5))
with(results, plot(size, icp_covrate, type="l", col='blue', ylab = 'Coverage rate', xlab = 'Data size', ylim = c(0.5,1)))
with(results, points(size, bs_covrate, type="l", ylab = NA, xlab = NA, col='blue'))
par(new = T)
with(results, plot(size, icp_runtime, type = 'l', col = 'red', ylab = NA, xlab = NA, axes = F))
axis(side = 4)
mtext(side = 4, line = 3, 'Runtime (secs)')
legend('bottom',
       legend=c('MSE', 'Runtime'),
       lty=c(1,1), col=c('blue', 'red'))
