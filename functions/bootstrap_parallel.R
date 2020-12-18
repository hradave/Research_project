################################ PARALLEL BOOTSTRAP PREDICTION ###################################


bootstrap_parallel <- function(train, test, formula, conf = 0.95, R = 500, ntree = 125, cores = NA){
  
  # set seed for reproducibility
  set.seed(12345)
  
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
  
  if (is.na(cores)) {
    cores = detectCores()
  }
  
  param_bootstrap = boot(train, statistic = prediction, R = R, 
                         mle = pars, ran.gen = rng, sim = "parametric", pars = pars,
                         parallel = "multicore", ncpus = cores)
  
  
  
  e = envelope(param_bootstrap, level = conf)
  # check if envelope() does the same thing as quantile()
  test$lower_e = e$point[2,]
  test$upper_e = e$point[1,]
  
  quantiles = apply(param_bootstrap$t, 2, quantile, probs = c((1-conf)/2, 1-(1-conf)/2))
  test$lower_q = quantiles[1,]
  test$upper_q = quantiles[2,]
  
  prediction = predict(est, newdata = test)
  test$pred = prediction
  
  # calculate test coverage rate
  n_test = dim(test)[1]
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
