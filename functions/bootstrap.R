################################ BOOTSTRAP PREDICTION ###################################


bootstrap <- function(data, formula, conf = 0.95, R = 100, ntree = 500, data_split = c(0.7, 0.3)){
  
  # set seed, so that the data splits will always be the same
  # good for testing different settings, but might remove in final version
  set.seed(12345)
  
  # REMOVE calibration set later!!!!!!!!!!!!!!!!!!!!!!!
  
  # Divide data into train, calibration and test sets (50% - 25% - 25%)
  n = dim(data)[1]
  train_id = sample(1:n, floor(n * data_split[1])) # training ids
  train = data[train_id,]
  
  test_id = setdiff(1:n, train_id) # test ids
  test_id = sample(test_id, floor(n * data_split[2])) # test ids
  test = data[test_id,]
  
  # cal_test_id = setdiff(1:n, train_id) # cal + test ids
  # cal_id = sample(cal_test_id, floor(n * data_split[2])) # calibration ids
  # cal = data[cal_id,]
  # test_id = setdiff(cal_test_id,cal_id) # test ids
  # test = data[test_id,]
  
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
    data_gen[,y] = rnorm(n, mle$prediction, sd(mle$residual))
    return(data_gen)
  }
  
  
  # function to fit model to randomly generated dataset (by rng) and predict to some original set (test)
  prediction = function(data_gen, pars){
    # keep track of progress
    counter <<- counter + 1
    print(paste0('Progress: ', counter/R*100, '%'))
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
  
  
  
  e = envelope(param_bootstrap, level = conf)
  # check if envelope() does the same thing as quantile()
  test$lower_e = e$point[2,]
  test$upper_e= e$point[1,]
  
  quantiles = apply(param_bootstrap$t, 2, quantile, probs = c((1-conf)/2, 1-(1-conf)/2))
  test$lower_q = quantiles[1,]
  test$upper_q = quantiles[2,]
  
  
  prediction = predict(est, newdata = test)
  test$pred = prediction
  
  # calculate test coverage rate
  n_test = dim(test)[1]
  #test_coverage_rate = dim(test %>% filter(Rings >= lower & Rings <= upper))[1] / n_test #0.9588
  test_coverage_rate_e = length(which(((test[,y] >= test$lower_e) * (test[,y] <= test$upper_e)) == 1)) / n_test
  test_coverage_rate_q = length(which(((test[,y] >= test$lower_q) * (test[,y] <= test$upper_q)) == 1)) / n_test
  
  # average size of intervals
  mean_interval_size_e = mean(test$upper_e - test$lower_e)
  mean_interval_size_q = mean(test$upper_q - test$lower_q)
  
  # output list
  res = list(test = test,
             test_coverage_rate_e = test_coverage_rate_e,
             mean_interval_size_e = mean_interval_size_e,
             test_coverage_rate_q = test_coverage_rate_q,
             mean_interval_size_q = mean_interval_size_q)
  
}
