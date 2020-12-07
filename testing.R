library(randomForest)
library(tidyverse)
library(boot)

# load scripts
source("icp.R")
source("bootstrap.R")
source("plot_prediction_bands.R")
source("normalize.R")

# Read data
abalone = read.table('data/abalone.data', sep = ',')

colnames = c("Sex", "Length", "Diameter", "Height", "Whole_weight", "Shucked_weight", "Viscera_weight",
             "Shell_weight", "Rings")

colnames(abalone) = colnames

# normalize target variable
abalone$Rings = normalize(abalone$Rings)


# variables set up
conf = 0.95
formula = formula(y ~ .)

# ICP
beta = 0.01
res_icp = ICP(data, formula, normalized = TRUE, beta = beta)
icp = res_icp$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(icp, x = 'x1', y = 'y',type = 'cp', conf = conf,
                      title = paste('beta =',beta), sample_size = 100)
res_icp$test_coverage_rate #norm(1): 0.9454545 norm(0.01): 0.9464 unnorm = 0.9588
res_icp$mean_interval_size #norm(1): 9.49939   norm(0.01): 12.87  unnorm = 9.47
hist(icp$upper - icp$lower)
range(icp$upper - icp$lower) #beta4: 4.044-24.65

# bootstrap
R = 500
res = bootstrap(data, formula, conf = conf, ntree = 100, R = R, data_split = c(0.7, 0.3))
bs = res$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(bs, x = 'x1', y = 'y', type = 'bs', bs_type = 'e',conf = conf, title = paste('R = ', R), sample_size = 100)
set.seed(1234)
plot_prediction_bands(bs, x = 'x1', y = 'y', type = 'bs', bs_type = 'q',conf = conf, title = paste('R = ', R), sample_size = 100)
res$test_coverage_rate_e #R100: 0.7942 R1000(70% training): 0.7565
res$mean_interval_size_e #R100: 4.4519 R1000(70% training): 4.118
res$test_coverage_rate_q #R100: 0.74545 R1000(70% training): 0.7541
res$mean_interval_size_q #R100: 3.9087 R1000(70% training): 4.083
hist(res$test$upper_e-res$test$lower_e)
hist(res$test$upper_q-res$test$lower_q)
range(res$test$upper_e-res$test$lower_e) #R1000(70% training): 3.76-4.617
range(res$test$upper_e-res$test$lower_q) #R1000(70% training): 3.74-4.596





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
