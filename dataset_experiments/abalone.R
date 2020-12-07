library(randomForest)
library(tidyverse)
library(boot)

# Read data
abalone = read.table('data/abalone.data', sep = ',')

colnames = c("Sex", "Length", "Diameter", "Height", "Whole_weight", "Shucked_weight", "Viscera_weight",
             "Shell_weight", "Rings")

colnames(abalone) = colnames

#abalone = abalone[order(abalone$Length),]


################################ CONFORMAL PREDICTION ###################################

ICP <- function(data, formula, normalized = TRUE, conf = 0.95, beta = 1, ntree = 500, data_split = c(0.5, 0.25, 0.25)){
  # set seed, so that the data splits will always be the same
  # good for testing different settings, but might remove in final version
  set.seed(12345)
  
  # Divide data into train, calibration and test sets (50% - 25% - 25%)
  n = dim(data)[1]
  train_id = sample(1:n, floor(n * data_split[1])) # training ids
  train = data[train_id,]
  cal_test_id = setdiff(1:n, train_id) # cal + test ids
  cal_id = sample(cal_test_id, floor(n * data_split[2])) # calibration ids
  cal = data[cal_id,]
  test_id = setdiff(cal_test_id,cal_id) # test ids
  test = data[test_id,]
  
  # extract name of the response variable
  y = as.character(formula)[2]
  
  # train random forest on the training set
  rF <- randomForest(formula = formula, data = train, ntree = ntree)
  
  # predict for the calibration and test sets 
  # (predict.all is only necessary for the normalized version to get all individual tree predictions)
  rF_pred_cal <- predict(rF, newdata = cal, predict.all = TRUE)
  rF_pred_test <- predict(rF, newdata = test, predict.all = TRUE)
  
  
  if (normalized) {
    # DO NORMALIZED ICP WITH VARIANCE-BASED NONCONFORMITY MEASURE
    
    ## calculate normalized nonconformity score for the calibration set
    
    # difficulty estimate for the calibration set (variance of the predictions of the individual trees in the forest)
    mu_cal = apply(rF_pred_cal$individual, 1, var)
    
    score_cal = abs(cal[,y] - rF_pred_cal$aggregate) / (mu_cal + beta)
    
    # find the smallest score that satisfies equation (3)
    n_cal = dim(cal)[1]
    #score_bound = sort(score_cal)[n_cal * conf + 2] # find a better way than adding 2
    score_bound = sort(score_cal, decreasing = T)[floor((1 - conf) * (n_cal + 1))] #p78 Evaluation of Variance-based nonconformity

    # check if equation holds
    #(sum(score_cal < score_bound) + 1) / (n_cal + 1) >= conf #Eq. 3 from Regression Conformal Prediction
    
    # difficulty estimate for the test set
    mu_test = apply(rF_pred_test$individual, 1, var)
    
    # add prediction and bounds to the test df
    test$pred = rF_pred_test$aggregate # average of all trees in the forest
    test$lower = test$pred - score_bound * (mu_test + beta)
    test$upper = test$pred + score_bound * (mu_test + beta)
    
  } else{
    # DO UNNORMALIZED ICP
    
    # calculate nonconformity score for the calibration set
    score_cal = abs(cal[,y] - rF_pred_cal$aggregate)
    
    # find the smallest score that satisfies equation (3)
    n_cal = dim(cal)[1]
    #score_bound = sort(score_cal)[n_cal * conf + 2] # find a better way than adding 2
    score_bound = sort(score_cal, decreasing = T)[floor((1 - conf) * (n_cal + 1))] #p78 Evaluation of Variance-based nonconformity
    # check if equation holds
    #(sum(score_cal < score_bound) + 1) / (n_cal + 1) >= conf #Eq. 3 from Regression Conformal Prediction

    
    # add prediction and bounds to the test df
    test$pred = rF_pred_test$aggregate
    test$lower = test$pred - score_bound
    test$upper = test$pred + score_bound
  }
  
  # calculate test coverage rate
  n_test = dim(test)[1]
  #test_coverage_rate = dim(test %>% filter(Rings >= lower & Rings <= upper))[1] / n_test #0.9588
  test_coverage_rate = length(which(((test[,y] >= test$lower) * (test[,y] <= test$upper)) == 1)) / n_test
  
  # average size of intervals
  mean_interval_size = mean((test %>% mutate(interval_size = upper - lower))$interval_size)
  
  # output list
  res = list(test = test,
             test_coverage_rate = test_coverage_rate,
             mean_interval_size = mean_interval_size)
}




############### UNNORMALIZED CONFORMAL PREDICTION

unnorm_ICP <- function(data, conf = 0.95, ntree = 500) {
  # Divide data
  n = dim(data)[1]
  #set.seed(12345)
  id = sample(1:n, floor(n*(1/2))) # training ids
  train = data[id,]
  id1 = setdiff(1:n, id) # cal + test ids
  id2 = sample(id1, floor(n*(1/4))) # calibration ids
  cal = data[id2,]
  id3 = setdiff(id1,id2) # test ids
  test = data[id3,]
  
  rF <- randomForest(formula = Rings ~ ., data = train, ntree = ntree)
  #rF_pred_train <- predict(rF, newdata = train)
  rF_pred_cal <- predict(rF, newdata = cal)
  rF_pred_test <- predict(rF, newdata = test)
  
  # calculate nonconformity score for the calibration set
  score_cal = abs(cal$Rings - rF_pred_cal)
  
  # find the smallest score that satisfies equation (3)
  n_cal = dim(cal)[1]
  score_bound = sort(score_cal)[n_cal * conf + 2] # find a better way than adding 2
  
  # check if equation holds
  #(sum(score_cal < score_bound) + 1) / (n_cal + 1) >= conf
  
  # add prediction and bounds to the test df
  test$pred = rF_pred_test
  test$lower = test$pred - score_bound
  test$upper = test$pred + score_bound
  
  n_test = dim(test)[1]
  test_coverage_rate = dim(test %>% filter(Rings >= lower & Rings <= upper))[1] / n_test #0.9588
  #dim(test[1:size,] %>% filter(Rings > lower & Rings < upper))[1] / 100 #0.96
  
  # size of intervals
  mean_interval_size = mean((test %>% mutate(interval_size = upper - lower))$interval_size)
  
  res = list(test = test,
             test_coverage_rate = test_coverage_rate,
             mean_interval_size = mean_interval_size)
  
  }

# set seed before calling ICP()
set.seed(12345)
res = unnorm_ICP(abalone)



########### NORMALIZED CONFORMAL PREDICTION


norm_ICP <- function(data, beta = 1, conf = 0.95, ntree = 500) {
  # Divide data
  n = dim(data)[1]
  #set.seed(12345)
  id = sample(1:n, floor(n*(1/2))) # training ids
  train = data[id,]
  id1 = setdiff(1:n, id) # cal + test ids
  id2 = sample(id1, floor(n*(1/4))) # calibration ids
  cal = data[id2,]
  id3 = setdiff(id1,id2) # test ids
  test = data[id3,]
  
  rF_norm <- randomForest(formula = Rings ~ ., data = train, ntree = ntree)
  
  # predict for the calibration set (save the individual tree predictions, too)
  rF_pred_cal_norm <- predict(rF_norm, newdata = cal, predict.all = TRUE)
  
  ## calculate normalized nonconformity score for the calibration set
  
  # difficulty estimate for the calibration set (variance of the predictions of the individual trees in the forest)
  mu_cal = apply(rF_pred_cal_norm$individual, 1, var)
  
  score_cal_norm = abs(cal$Rings - rF_pred_cal_norm$aggregate) / (mu_cal + beta)
  
  # find the smallest score that satisfies equation (3)
  n_cal = dim(cal)[1]
  score_bound_norm = sort(score_cal_norm)[n_cal * conf + 2] # find a better way than adding 2
  
  # check if equation holds
  #(sum(score_cal_norm < score_bound_norm) + 1) / (n_cal + 1) >= conf
  #(sum(score_cal_norm < score_bound_norm) + 1) / (n_cal + 1)
  
  #score_bound_norm # 0.9353 (Beta1) ## 1.537 (Beta0.01)
  
  # predict for the test set (save the individual tree predictions, too)
  rF_pred_test_norm = predict(rF_norm, newdata = test, predict.all = TRUE)
  
  # difficulty estimate for the test set
  mu_test = apply(rF_pred_test_norm$individual, 1, var)
  
  # add prediction and bounds to the test df
  test$pred = rF_pred_test_norm$aggregate # average of all trees in the forest
  test$lower = test$pred - score_bound_norm * (mu_test + beta)
  test$upper = test$pred + score_bound_norm * (mu_test + beta)
  
  # check coverage rate
  n_test = dim(test)[1]
  test_coverage_rate = dim(test %>% filter(Rings >= lower & Rings <= upper))[1] / n_test #0.9473
  
  # calculate the average size of intervals on the test set
  mean_interval_size = mean((test %>% mutate(interval_size = upper - lower))$interval_size)
  
  res = list(test = test,
             test_coverage_rate = test_coverage_rate,
             mean_interval_size = mean_interval_size)
  
}



########## PLOT RESULTS

plot_prediction_bands <- function(data, x, y, sample_size = 100, type = 'Conformal', title = '', conf = 0.95){
  
  # type has to be either 'Conformal' or 'Bootstrap'
  
  # draw random sample from data
  data_sample = data[sample(1:dim(data)[1], sample_size),]
  # order by x for plotting
  data_sample = data_sample[order(data_sample[,x]),]
  #data[1:sample_size,] = data[order(data[1:sample_size,][,x]),]
  
  # plot data points and their conformal prediction bands
  plot(data_sample[,x], data_sample[,y], pch=21, bg="red", xlab = x, ylab = y,
       main = paste0(type, ' prediction ', '(', conf * 100, '%) ', title), 
       ylim = c(min(data_sample$lower, data_sample$upper, data_sample$pred, data_sample[,y]),
                max(data_sample$lower, data_sample$upper, data_sample$pred, data_sample[,y])))
  points(data_sample[,x], data_sample$pred, pch=21, bg="yellow")
  lines(data_sample[,x], data_sample$lower, col = 'blue')
  lines(data_sample[,x], data_sample$upper, col = 'blue')
  legend('topleft', legend = c('True', 'Predicted', 'Prediction band'), pch = c(21,21,NA), 
         lty = c(NA,NA,1), col = c('black', 'black', 'blue'), pt.bg = c('red','yellow', NA))
  
  #outside = dim(data_sample %>% filter(Rings < lower | Rings > upper))[1]
  outside = length(which(((data_sample[,y] >= data_sample$lower) * (data_sample[,y] <= data_sample$upper)) == 0))
  paste0('True data points outside of the prediction bands: ', outside, ' (', outside/sample_size*100,'%)')
}


beta = 4
formula = formula(Rings ~ .)
res = ICP(abalone, formula, normalized = TRUE, beta = beta)
icp = res$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(icp, x = 'Length', y = 'Rings', title = paste('beta =',beta), sample_size = 100)
res$test_coverage_rate #norm(1): 0.9454545 norm(0.01): 0.9464 unnorm = 0.9588
res$mean_interval_size #norm(1): 9.49939   norm(0.01): 12.87  unnorm = 9.47

# plot manually
test = icp
size = 100
test[1:size,] = test[order(test[1:size,]$Length),]

# plot test points and their conformal prediction bands
plot(test[1:size,]$Length, test[1:size,]$Rings, pch=21, bg="red", main = 'test CP', ylim = c(0,25))
points(test[1:size,]$Length, test[1:size,]$pred, pch=21, bg="yellow")
lines(test[1:size,]$Length, test[1:size,]$lower, col = 'blue')
lines(test[1:size,]$Length, test[1:size,]$upper, col = 'blue')#, pch = 20, cex = 1)



# check performance with different settings
betas = seq(1.5,10, by = 0.1)
norm_ICP_perf = as.data.frame(matrix(0, nrow = length(betas), ncol = 2))
rownames(norm_ICP_perf) = betas
colnames(norm_ICP_perf) = c('test_coverage_rate', 'mean_interval_size')

for (i in 1:length(betas)) {
  # set seed before calling ICP function
  set.seed(12345)
  res = norm_ICP(data = abalone, beta = betas[i])
  norm_ICP_perf[i,1] = res$test_coverage_rate
  norm_ICP_perf[i,2] = res$mean_interval_size
  print(i)
}

# plot performance metrics
plot(betas, norm_ICP_perf$test_coverage_rate, type = 'l')
plot(betas, norm_ICP_perf$mean_interval_size, type = 'l')


#dim(test[1:size,] %>% filter(Rings >= lower & Rings <= upper))[1] / 100 #0.93

# size of intervals
hist((test %>% mutate(interval_size = upper - lower))$interval_size) # not the same
range((test %>% mutate(interval_size = upper - lower))$interval_size) #2.08 - 40.99 (Beta1) ##0.55 - 70.37 (Beta0.01)
mean((test %>% mutate(interval_size = upper - lower))$interval_size) #9.62 (Beta1) ##13.85 (Beta0.01)

# #investigation = data.frame( mu_test, (test %>% mutate(interval_size = upper - lower))$interval_size)
# investigation = data.frame(investigation, mu_test, (test %>% mutate(interval_size = upper - lower))$interval_size)
# colnames(investigation) = c('mu_test_001', 'interval_size_001', 'mu_test_1', 'interval_size_1')

##################### PARAMETRIC BOOTSTRAP PREDICTION (rF)

bootstrap <- function(data, formula, conf = 0.95, R = 100, ntree = 500, band_type = 'envelope', data_split = c(0.5, 0.25, 0.25)){
  
  # band_type has to be one of these two: 'envelope', 'quantile'
  
  # set seed, so that the data splits will always be the same
  # good for testing different settings, but might remove in final version
  set.seed(12345)
  
  # REMOVE calibration set later!!!!!!!!!!!!!!!!!!!!!!!
  
  # Divide data into train, calibration and test sets (50% - 25% - 25%)
  n = dim(data)[1]
  train_id = sample(1:n, floor(n * data_split[1])) # training ids
  train = data[train_id,]
  cal_test_id = setdiff(1:n, train_id) # cal + test ids
  cal_id = sample(cal_test_id, floor(n * data_split[2])) # calibration ids
  cal = data[cal_id,]
  test_id = setdiff(cal_test_id,cal_id) # test ids
  test = data[test_id,]
  
  # extract name of the response variable
  y = as.character(formula)[2]
  
  
  # estimate the parameters
  est = randomForest(formula = formula, data = train, ntree = ntree)
  # predict using the estimated parameters
  prediction = predict(est, newdata = train)
  # calculate residuals
  residual = train[,y] - prediction
  pars = list(prediction = prediction, residual = residual)
  
  # function to generate random dataset from the original one and the estimates
  rng = function(data, mle){
    data_gen = data
    n = dim(data_gen)[1]
    # generate new Rings (response)
    data_gen$Rings = rnorm(n, mle$prediction, sd(mle$residual))
    return(data_gen)
  }
  
  
  # function to fit model to randomly generated dataset (by rng) and predict to some original set (test)
  prediction = function(data_gen, pars){
    # keep track of progress
    counter <<- counter + 1
    print(counter)
    # fit model
    model = randomForest(formula = formula, data = data_gen, ntree = ntree)
    # predict expected values for all x values from the original data (test)
    predict_exp = predict(model, newdata = test)
    n = dim(test)[1]
    
    # simulate prediction with added Gaussian noise
    sim = rnorm(n, predict_exp, sd(pars$residual))
    return(sim)
  }
  
  counter = 0
  param_bootstrap = boot(train, statistic = prediction, R = R, 
                         mle = pars, ran.gen = rng, sim = "parametric", pars = pars)
  
  
  if (band_type == 'envelope') {
    e = envelope(param_bootstrap, level = conf)
    # check if envelope() does the same thing as quantile()
    test$lower = e$point[2,]
    test$upper = e$point[1,]
  } else {
    quantiles = apply(param_bootstrap$t, 2, quantile, probs = c((1-conf)/2, 1-(1-conf)/2))
    test$lower = quantiles[1,]
    test$upper = quantiles[2,]
  }
  
  prediction = predict(est, newdata = test)
  test$pred = prediction
  
  # calculate test coverage rate
  n_test = dim(test)[1]
  #test_coverage_rate = dim(test %>% filter(Rings >= lower & Rings <= upper))[1] / n_test #0.9588
  test_coverage_rate = length(which(((test[,y] >= test$lower) * (test[,y] <= test$upper)) == 1)) / n_test
  
  # average size of intervals
  mean_interval_size = mean((test %>% mutate(interval_size = upper - lower))$interval_size)
  
  # output list
  res = list(test = test,
             test_coverage_rate = test_coverage_rate,
             mean_interval_size = mean_interval_size,
             band_type = band_type)
  
}

conf = 0.95
formula = formula(Rings ~ .)
res = bootstrap(abalone, formula, conf = conf)
icp = res$test
set.seed(1234) # set the seed if you don't want the plot function to use different random points
plot_prediction_bands(icp, x = 'Length', y = 'Rings', type = 'Bootstrap', conf = conf, title = paste('R = 100'), sample_size = 100)
res$test_coverage_rate #norm(1): 0.9454545 norm(0.01): 0.9464 unnorm = 0.9588
res$mean_interval_size #norm(1): 9.49939   norm(0.01): 12.87  unnorm = 9.47




plot(param_bootstrap)

#compute prediction bands
conf = 0.95
e = envelope(param_bootstrap, level = conf)
# check if envelope() does the same thing as quantile()
test$lower_e = e$point[2,]
test$upper_e = e$point[1,]

fit = randomForest(formula = Rings ~ ., data = train, ntree = 500)
prediction = predict(fit, newdata = test)
test$pred = prediction


quantiles = apply(param_bootstrap$t, 2, quantile, probs = c((1-conf)/2, 1-(1-conf)/2))

test$lower_q = quantiles[1,]
test$upper_q = quantiles[2,]
size = 100
test[1:size,] = test[order(test[1:size,]$Length),]


# plot with envelope
plot(test[1:size,]$Length, test[1:size,]$Rings, pch=21, bg="red", ylim = c(0, 25), main = 'test BS envelope R = 1000')
points(test[1:size,]$Length, test[1:size,]$pred, type="p", bg = 'yellow', pch = 21) #plot fitted line
#plot prediction bands
points(test[1:size,]$Length,test[1:size,]$lower_e, col = 'blue', type="l")
points(test[1:size,]$Length,test[1:size,]$upper_e, col = 'blue', type="l")



# plot with quantile
plot(test[1:size,]$Length, test[1:size,]$Rings, pch=21, bg="red", ylim = c(0, 25), main = 'test BS quantile R = 1000')
points(test[1:size,]$Length, test[1:size,]$pred, type="p", bg = 'yellow', pch = 21) #plot fitted line
#plot prediction bands
points(test[1:size,]$Length, test[1:size,]$lower_q, col = 'blue', type="l")
points(test[1:size,]$Length, test[1:size,]$upper_q, col = 'blue', type="l")


# check coverage rate
n_test = dim(test)[1]
dim(test %>% filter(Rings >= lower_e & Rings <= upper_e))[1] / n_test #79.7 #77.2 (R = 1000)
dim(test %>% filter(Rings >= lower_q & Rings <= upper_q))[1] / n_test #74.9 #77.0 (R = 1000)


dim(test[1:size,] %>% filter(Rings >= lower_e & Rings <= upper_e))[1] / 100 #0.71 #0.7 (R = 1000)
dim(test[1:size,] %>% filter(Rings >= lower_q & Rings <= upper_q))[1] / 100 #0.69 #0.7 (R = 1000)

View(test[1:size,] %>% 
       filter(Rings < lower_e | Rings > upper_e) %>% 
       select(Length, Rings, upper_e, lower_e, pred))

# size of intervals
range((test %>% mutate(interval_size = upper_e - lower_e))$interval_size) # #3.6 - 4.7 (R = 1000)
range((test %>% mutate(interval_size = upper_q - lower_q))$interval_size) # #3.6 - 4.6 (R = 1000)


# size of intervals
mean((test %>% mutate(interval_size = upper_e - lower_e))$interval_size) # #4.09 (R = 1000)
mean((test %>% mutate(interval_size = upper_q - lower_q))$interval_size) # #4.05 - 4.6 (R = 1000)


