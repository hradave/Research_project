library(cvTools)

rF <- randomForest(formula = formula, data = train, ntree = ntree)

#cvFolds(4170, K = 10, type = "random", R =10)
fit <- randomForest(formula = Rings~., data = abalone[1:4170,], ntree = 500)
# perform cross-validation

start_time <- Sys.time()
res = cvFit(fit, data = abalone[1:4170,], y = abalone[1:4170,]$Rings, cost = rtmspe, 
      K = 10, R = 3, seed = 1234)
end_time <- Sys.time()
end_time - start_time

#R = 1: 1.4 mins
#R = 2: 2.8 mins
#R = 3: 4.3 mins


runtime = as.numeric(difftime("2018-07-08 21:12:56 EDT", "2018-07-08 20:07:56 EDT", units = 'secs'))
runtime
