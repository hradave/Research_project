library(randomForest)
library(tidyverse)
library(boot)

# load scripts
source("icp.R")
source("bootstrap.R")
source("plot_prediction_bands.R")

# Read data
abalone = read.table('data/abalone.data', sep = ',')

colnames = c("Sex", "Length", "Diameter", "Height", "Whole_weight", "Shucked_weight", "Viscera_weight",
             "Shell_weight", "Rings")

colnames(abalone) = colnames


# variables set up
conf = 0.95
formula = formula(Rings ~ .)

# ICP
beta = 0.01
res_icp = ICP(abalone, formula, normalized = TRUE, beta = beta)
icp = res_icp$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(icp, x = 'Length', y = 'Rings',type = 'cp', conf = conf,
                      title = paste('beta =',beta), sample_size = 100)
res_icp$test_coverage_rate #norm(1): 0.9454545 norm(0.01): 0.9464 unnorm = 0.9588
res_icp$mean_interval_size #norm(1): 9.49939   norm(0.01): 12.87  unnorm = 9.47
hist(icp$upper - icp$lower)
range(icp$upper - icp$lower) #beta4: 4.044-24.65

# bootstrap
R = 100
res = bootstrap(abalone, formula, conf = conf, R = R, data_split = c(0.5, 0.25))
bs = res$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(bs, x = 'Length', y = 'Rings', type = 'bs', bs_type = 'e',conf = conf, title = paste('R = ', R), sample_size = 100)
set.seed(1234)
plot_prediction_bands(bs, x = 'Length', y = 'Rings', type = 'bs', bs_type = 'q',conf = conf, title = paste('R = ', R), sample_size = 100)
res$test_coverage_rate_e #R100: 0.7942 R1000(70% training): 0.7565
res$mean_interval_size_e #R100: 4.4519 R1000(70% training): 4.118
res$test_coverage_rate_q #R100: 0.74545 R1000(70% training): 0.7541
res$mean_interval_size_q #R100: 3.9087 R1000(70% training): 4.083
hist(res$test$upper_e-res$test$lower_e)
hist(res$test$upper_q-res$test$lower_q)
range(res$test$upper_e-res$test$lower_e) #R1000(70% training): 3.76-4.617
range(res$test$upper_e-res$test$lower_q) #R1000(70% training): 3.74-4.596
