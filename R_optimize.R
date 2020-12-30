
############################## OPTIMIZE R #################################

# find the best R parameter for the random forest bootstrap
library(randomForest)
library(boot)
library(parallel)

#R_seq = seq(100,1000,by=100)
R_seq=200
R_seq_table = matrix(nrow=length(R_seq), ncol = 6)
R_seq_table = as.data.frame(R_seq_table)
colnames(R_seq_table) = c('R', 'runtime', 'coverage_rate_e', 'mean_interval_size_e', 'coverage_rate_q', 'mean_interval_size_q')

load("data/synth_data_50.RData")

source("functions/bootstrap_parallel.R")
source("functions/plot_prediction_bands.R")
source('functions/normalize.R')

dataset = dataset[1:10000,]
dataset$y = normalize(dataset$y)


conf = 0.95
formula = formula(y ~ .)

for (i in 1:length(R_seq)){
  print(i)
  R_seq_table[i,1] = R_seq[i]
  
  start_time <- Sys.time()
  res = bootstrap_parallel(dataset, formula, conf = conf, ntree = 125, R = R_seq[i], data_split = c(0.7, 0.3), cores = 8)
  end_time <- Sys.time()
  runtime = as.numeric(difftime(end_time, start_time, units = 'secs'))
  R_seq_table[i,2] = runtime
  R_seq_table[i,3] = res$test_coverage_rate_e #R100: 0.7942 R1000(70% training): 0.7565
  R_seq_table[i,4] = res$mean_interval_size_e #R100: 4.4519 R1000(70% training): 4.118 (0.14)
  R_seq_table[i,5] = res$test_coverage_rate_q #R100: 0.74545 R1000(70% training): 0.7541
  R_seq_table[i,6] = res$mean_interval_size_q #R100: 3.9087 R1000(70% training): 4.083 (0.14)
}


R_seq_table
# n = 10000
# p = 50
#    R  runtime coverage_rate_e mean_interval_size_e coverage_rate_q mean_interval_size_q
#1 100 1329.773       0.6116667            0.1129363       0.5566667           0.09862672
#    R  runtime coverage_rate_e mean_interval_size_e coverage_rate_q mean_interval_size_q
#1 200 2507.655       0.5796667             0.104951       0.5556667            0.1006335
