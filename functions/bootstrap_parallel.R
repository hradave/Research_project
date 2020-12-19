
################################ PARALLEL BOOTSTRAP PREDICTION ###################################

bootstrap_parallel <- function(train, test, formula, conf = 0.95, R = 500, ntree = 125, cores = NA){
  ### Implement Bootstrapping on the given dataset using Random Forest as the underlying model 
  ### and evaluate it on the given test set to obtain prediction intervals
  
  ### Arguments
  # train (data.frame): training set that is used for training the Random Forest
  # test (data.frame): test set on which to evaluate the model and calculate prediction intervals
  # formula (formula): formula for training the Random Forest model
  # conf (numeric): confidence level of the prediction region (between 0 and 1)
  # R (integer): number of bootstrap replicates
  # ntree (integer): number of trees to use in the Random Forest
  # cores (integer): number of CPU cores to use in parallel during bootstrapping
  
  ### Values in the returned list
  # test (data.frame): test set augmented with the predicted value and the lower and upper bounds
  # test_coverage_rate_e (numeric): coverage rate (1 - error rate) observed on the test data using the envelope method
  # mean_interval_size_e (numeric): mean of the prediction region sizes observed on the test data using the envelope method
  # test_coverage_rate_q (numeric): coverage rate (1 - error rate) observed on the test data using the quantile method
  # mean_interval_size_q (numeric): mean of the prediction region sizes observed on the test data using the quantile method
  # runtime (numeric): time required to train the model (proper training + calibration) in seconds
  
  # set seed for reproducibility
  set.seed(12345)
  
  # extract name of the response variable
  y = as.character(formula)[2]
  
  # set number of cores to use in parallel if not specified
  if (is.na(cores)) {
    cores = detectCores()
  }
  
  # function to generate random bootstrap replicate
  rng = function(data, mle){
    data_gen = data
    n = dim(data_gen)[1]
    # generate new response based on initial model and residuals (parametric)
    data_gen[,y] = rnorm(n, mle$prediction, sd(mle$residual))
    return(data_gen)
  }
  
  counter = 0
  
  # function to fit model to randomly generated dataset (by rng) and predict to some original set (test)
  statistic_fun = function(data_gen, pars){
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
  
  # start CPU timer
  start_time <- Sys.time()
  
  # estimate the parameters
  est = randomForest(formula = formula, data = train, ntree = ntree)
  # predict on the training set using the estimated parameters
  prediction_0 = predict(est, newdata = train)
  # calculate residuals
  residual = train[,y] - prediction_0
  pars = list(prediction = prediction_0, residual = residual)
  
  param_bootstrap = boot(train, statistic = statistic_fun, R = R, 
                         mle = pars, ran.gen = rng, sim = "parametric", pars = pars,
                         parallel = "multicore", ncpus = cores)
  
  
  # end CPU timer (training over)
  end_time <- Sys.time()
  runtime = as.numeric(difftime(end_time, start_time, units = 'secs'))
  
  
  e = envelope(param_bootstrap, level = conf)
  test$lower_e = e$point[2,]
  test$upper_e = e$point[1,]
  
  # another way of calculating the intervals
  quantiles = apply(param_bootstrap$t, 2, quantile, probs = c((1-conf)/2, 1-(1-conf)/2))
  test$lower_q = quantiles[1,]
  test$upper_q = quantiles[2,]
  
  # predictions on the test set
  prediction_test = predict(est, newdata = test)
  test$pred = prediction_test
  
  # calculate test coverage rate (both for the envelope and the quantile methods)
  n_test = dim(test)[1]
  test_coverage_rate_e = length(which(((test[,y] >= test$lower_e) * (test[,y] <= test$upper_e)) == 1)) / n_test
  test_coverage_rate_q = length(which(((test[,y] >= test$lower_q) * (test[,y] <= test$upper_q)) == 1)) / n_test
  
  # average size of intervals (both for the envelope and the quantile methods)
  mean_interval_size_e = mean(test$upper_e - test$lower_e)
  mean_interval_size_q = mean(test$upper_q - test$lower_q)
  
  # output list
  res = list(test = test,
             test_coverage_rate_e = test_coverage_rate_e,
             mean_interval_size_e = mean_interval_size_e,
             test_coverage_rate_q = test_coverage_rate_q,
             mean_interval_size_q = mean_interval_size_q,
             runtime = runtime)
  
}
