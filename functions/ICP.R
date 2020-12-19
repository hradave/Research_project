
################################ CONFORMAL PREDICTION ###################################

ICP <- function(train_cal, test, formula, normalized = TRUE, conf = 0.95, beta = 0.01, ntree = 125){
  ### Implement Inductive Conformal Prediction (ICP) on the given dataset using Random Forest as the underlying model 
  ### and evaluate it on the given test set to obtain prediction intervals
  
  ### Arguments
  # train_cal (data.frame): training set that is used for both proper training and calibration
  # test (data.frame: test set on which to evaluate the model and calculate prediction intervals
  # formula (formula): formula for training the Random Forest model
  # normalized (logical): whether to use a normalized or unnormalized nonconformity function
  # conf (numeric): confidence level of the prediction region (between 0 and 1)
  # beta (numeric): beta parameter to control the sensitivity of the nonconformity measure in the normalized case
  # ntree (integer): number of trees to use in the Random Forest
  
  ### Values in the returned list
  # test (data.frame): test set augmented with the predicted value and the lower and upper bounds
  # test_coverage_rate (numeric): coverage rate (1 - error rate) observed on the test data
  # mean_interval_size (numeric): mean of the prediction region sizes observed on the test data
  # runtime (numeric): time required to train the model (proper training + calibration) in seconds
  
  
  # set seed for reproducibility
  set.seed(12345)
  
  # Divide training data into proper training and calibration sets
  train_cal_n = dim(train_cal)[1]
  cal_n = 100 * floor(train_cal_n/400) - 1 # following the paper
  
  cal_id = sample(1:train_cal_n, cal_n) # calibration ids
  cal = train_cal[cal_id,]
  
  train = setdiff(1:train_cal_n, cal_id) # train ids
  train = train_cal[train,]
  
  # extract name of the response variable
  y = as.character(formula)[2]
  
  # train random forest on the training set (measure CPU time)
  start_time <- Sys.time()
  rF <- randomForest(formula = formula, data = train, ntree = ntree)
  
  # predict for the calibration set
  # (predict.all is only necessary for the normalized version to get all individual tree predictions)
  rF_pred_cal <- predict(rF, newdata = cal, predict.all = TRUE)
  
  
  if (normalized) {
    # DO NORMALIZED ICP WITH VARIANCE-BASED NONCONFORMITY MEASURE
    ## calculate normalized nonconformity score for the calibration set
    
    # difficulty estimate for the calibration set (variance of the predictions of the individual trees in the forest)
    mu_cal = apply(rF_pred_cal$individual, 1, var)
    
    score_cal = abs(cal[,y] - rF_pred_cal$aggregate) / (mu_cal + beta)
    
    # find the smallest score that satisfies equation (3)
    score_bound = sort(score_cal, decreasing = T)[floor((1 - conf) * (cal_n + 1))] #p78 Evaluation of Variance-based nonconformity
    
    # check if equation holds
    #(sum(score_cal < score_bound) + 1) / (n_cal + 1) >= conf #Eq. 3 from Regression Conformal Prediction
    
    # end CPU timer (training over)
    end_time <- Sys.time()
    runtime = as.numeric(difftime(end_time, start_time, units = 'secs'))
    
    # predict for the test set
    # (predict.all is only necessary for the normalized version to get all individual tree predictions)
    rF_pred_test <- predict(rF, newdata = test, predict.all = TRUE)
    
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
    score_bound = sort(score_cal, decreasing = T)[floor((1 - conf) * (cal_n + 1))] #p78 Evaluation of Variance-based nonconformity
   
    # check if equation holds
    #(sum(score_cal < score_bound) + 1) / (n_cal + 1) >= conf #Eq. 3 from Regression Conformal Prediction
    
    # predict for the test set
    # (predict.all is only necessary for the normalized version to get all individual tree predictions)
    rF_pred_test <- predict(rF, newdata = test, predict.all = TRUE)
    
    # end CPU timer (training over)
    end_time <- Sys.time()
    runtime = as.numeric(difftime(end_time, start_time, units = 'secs'))
    
    # add prediction and bounds to the test df
    test$pred = rF_pred_test$aggregate
    test$lower = test$pred - score_bound
    test$upper = test$pred + score_bound
  }
  
  # calculate test coverage rate
  n_test = dim(test)[1]
  test_coverage_rate = length(which(((test[,y] >= test$lower) * (test[,y] <= test$upper)) == 1)) / n_test
  
  # average size of intervals
  mean_interval_size = mean(test$upper - test$lower)
  
  # output list
  res = list(test = test,
             test_coverage_rate = test_coverage_rate,
             mean_interval_size = mean_interval_size,
             runtime = runtime)
}