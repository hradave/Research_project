
############################## OPTIMIZE BETA #################################


# find the best beta parameter for the normalized conformal prediction
library(randomForest)
source('functions/ICP.R')
source('functions/normalize.R')

beta_seq = seq(0.01, 1, by = 0.05)
beta_table = matrix(nrow=length(beta_seq), ncol = 4)
beta_table = as.data.frame(beta_table)
colnames(beta_table) = c('beta', 'coverage_rate', 'mean_interval_size', 'runtime')

load("data/synth_data_50.RData")
dataset = dataset[1:10000,]
dataset$y = normalize(dataset$y)

formula = formula(y ~ .)
conf = 0.95

for (i in 1:length(beta_seq)){
  print(i)
  beta_table[i,1] = beta_seq[i]
  
  start_time <- Sys.time()
  set.seed(12345)
  res = ICP(dataset,formula = formula, normalized = TRUE, conf = conf, beta = beta_seq[i], ntree = 125, data_split = c(0.575, 0.125, 0.3))
  end_time <- Sys.time()
  runtime = as.numeric(difftime(end_time, start_time, units = 'secs'))
  
  beta_table[i,2] = res$test_coverage_rate
  beta_table[i,3] = res$mean_interval_size
  beta_table[i,4] = runtime
}

# save df for future use
save(beta_table, file = "data/beta_table.RData")


par(mar = c(5,5,2,5))
with(beta_table, plot(beta, coverage_rate, type="l", col='blue', ylab = 'coverage rate', xlab = expression(beta)))
par(new = T)
with(beta_table, plot(beta, mean_interval_size, type = 'l', col = 'red', ylab = NA, xlab = NA, axes = F))
axis(side = 4)
mtext(side = 4, line = 3, 'Mean interval size')
legend('bottomright',
       legend=c('coverage rate', 'mean interval size'),
       lty=c(1,1), col=c('blue', 'red'))


plot(beta_table$beta, beta_table$runtime, type = 'l', xlab = 'number of trees', ylab = 'runtime', col = 'blue', main = 'ICP')

