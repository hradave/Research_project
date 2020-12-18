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


# split data into train and test sets
splits = split_data(dataset, n)
train = splits$train
test = splits$test


############### for loop start

# ICP
start_time <- Sys.time()
res_icp = ICP(train, test, formula = formula, normalized = TRUE, beta = beta, ntree = ntree, conf = conf)
end_time <- Sys.time()
runtime_icp = as.numeric(difftime(end_time, start_time, units = 'secs'))
runtime_icp

res_icp$test_coverage_rate
res_icp$mean_interval_size



# BOOTSTRAP PARALLEL
start_time <- Sys.time()
res_bs = bootstrap_parallel(train, test , formula = formula, conf = conf, ntree = ntree, R = R, cores = 8)
end_time <- Sys.time()
runtime_bs = as.numeric(difftime(end_time, start_time, units = 'secs'))
runtime_bs
# n = 1000 : 117

res_bs$test_coverage_rate_e
res_bs$mean_interval_size_e
res_bs$test_coverage_rate_q
res_bs$mean_interval_size_q

############### for loop end



