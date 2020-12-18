library(randomForest)
library(boot)
library(parallel)

# load scripts
source("functions/ICP.R")
source("functions/bootstrap.R")
source("functions/bootstrap_parallel.R")
source("functions/plot_prediction_bands.R")
source("functions/normalize.R")

# Read data
abalone = read.table('data/abalone.data', sep = ',')

colnames = c("Sex", "Length", "Diameter", "Height", "Whole_weight", "Shucked_weight", "Viscera_weight",
             "Shell_weight", "Rings")

colnames(abalone) = colnames

# normalize target variable
abalone$Rings = normalize(abalone$Rings)
load('synth_data_50.RData')


# variables set up
conf = 0.95
formula = formula(y ~ .)
data = dataset[1:10000,]

data$y = normalize(data$y)


# ICP
beta = 0.01

set.seed(12345)
start_time <- Sys.time()
res_icp = ICP(data, formula, normalized = TRUE, beta = beta, ntree = 125)
end_time <- Sys.time()
end_time - start_time
runtime = as.numeric(difftime(end_time, start_time, units = 'secs'))
runtime

# 3.4 sec (win) 3.7 (ubuntu)

icp = res_icp$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(icp, x = 'x1', y = 'y',type = 'cp', conf = conf,
                      title = paste('beta =',beta), sample_size = 100)
res_icp$test_coverage_rate #norm(1): 0.9454545 norm(0.01): 0.9464 unnorm = 0.9588
res_icp$mean_interval_size #norm(1): 9.49939   norm(0.01): 12.87  unnorm = 9.47
hist(icp$upper - icp$lower)
range(icp$upper - icp$lower) #beta4: 4.044-24.65

# bootstrap


start_time <- Sys.time()
R = 100
res = bootstrap(data, formula, conf = conf, ntree = 500, R = R, data_split = c(0.7, 0.3))
end_time <- Sys.time()
end_time - start_time 
# R100: 9.8 mins, 8.2(win)
# R1000: 1.64 hours

bs = res$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(bs, x = 'x1', y = 'y', type = 'bs', bs_type = 'e',conf = conf, title = paste('R = ', R), sample_size = 100)
set.seed(1234)
plot_prediction_bands(bs, x = 'x1', y = 'y', type = 'bs', bs_type = 'q',conf = conf, title = paste('R = ', R), sample_size = 100)
res$test_coverage_rate_e #R100: 0.7942 R1000(70% training): 0.7565
res$mean_interval_size_e #R100: 4.4519 R1000(70% training): 4.118 (0.14)
res$test_coverage_rate_q #R100: 0.74545 R1000(70% training): 0.7541
res$mean_interval_size_q #R100: 3.9087 R1000(70% training): 4.083 (0.14)
hist(res$test$upper_e-res$test$lower_e)
hist(res$test$upper_q-res$test$lower_q)
range(res$test$upper_e-res$test$lower_e) #R1000(70% training): 3.76-4.617 (0.134 - 0.164)
range(res$test$upper_e-res$test$lower_q) #R1000(70% training): 3.74-4.596 (0.133 - 0.164)



# bootstrap PARALLEL
  
  start_time <- Sys.time()
  R = 500
  res = bootstrap_parallel(data , formula, conf = conf, ntree = 125, R = R, data_split = c(0.7, 0.3), cores = 8)
  end_time <- Sys.time()
  end_time - start_time 
#R100: 9.9 mins
#R100 multicore 8: 3.14 mins
#R100 multicore 4: 3.48 mins
#R100 multicore 2: 5.46 mins
#R1000 multicore 8: 30.5 mins

# parallel is 3 times faster 

bs = res$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(bs, x = 'x1', y = 'y', type = 'bs', bs_type = 'e',conf = conf, title = paste('R = ', R), sample_size = 100)
set.seed(1234)
plot_prediction_bands(bs, x = 'x1', y = 'y', type = 'bs', bs_type = 'q',conf = conf, title = paste('R = ', R), sample_size = 100)
res$test_coverage_rate_e #R100: 0.7942 R1000(70% training): 0.7565
res$mean_interval_size_e #R100: 4.4519 R1000(70% training): 4.118 (0.1488)
res$test_coverage_rate_q #R100: 0.74545 R1000(70% training): 0.7541
res$mean_interval_size_q #R100: 3.9087 R1000(70% training): 4.083 (0.1475)
hist(res$test$upper_e-res$test$lower_e)
hist(res$test$upper_q-res$test$lower_q)
range(res$test$upper_e-res$test$lower_e) #R1000(70% training): 3.76-4.617 (0.132 - 0.164)
range(res$test$upper_e-res$test$lower_q) #R1000(70% training): 3.74-4.596 (0.132 - 0.163)






# check performance with different settings
betas = seq(0.01, 10, by = 0.1)
norm_ICP_perf = as.data.frame(matrix(0, nrow = length(betas), ncol = 2))
rownames(norm_ICP_perf) = betas
colnames(norm_ICP_perf) = c('test_coverage_rate', 'mean_interval_size')

for (i in 1:length(betas)) {
  # set seed before calling ICP function
  set.seed(12345)
  res = ICP(abalone, formula, normalized = TRUE, beta = betas[i])
  norm_ICP_perf[i,1] = res$test_coverage_rate
  norm_ICP_perf[i,2] = res$mean_interval_size
  print(i)
}

# plot performance metrics
plot(betas, norm_ICP_perf$test_coverage_rate, type = 'l')
plot(betas, norm_ICP_perf$mean_interval_size, type = 'l')
