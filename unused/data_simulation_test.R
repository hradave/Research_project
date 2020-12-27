library(ggplot2)

sim_data <- function(n = 500,
                     p = 10,
                     rho = 0.6,
                     x0 = NULL,
                     predictor_dist = c("uncorrelated", "correlated"),
                     mean_function = c("linear", "nonlinear", "nonlinear-interaction"),
                     error_dist = c("homoscedastic","heavy-tailed", "heteroscedastic","skewed")){
  
  if(predictor_dist == "uncorrelated")
    x <- matrix(rnorm(n*p, 0, 1), n, p)
  else{
    library(MASS)
    Sigma <- diag(rep(1,p))
    for(i in 1:p){
      for(j in i:p){
        Sigma[i,j] <- Sigma[j,i] <- rho^(abs(i-j))
      }
    }
    #print(Sigma)
    x <- mvrnorm(n, mu = rep(0,p), Sigma = Sigma)
  }
  
  if(mean_function == "linear")
    mx <- 1.1*x[,1] + 3.5*x[,2] - 1.2 * x[,3] + 0.12 * x[,4]
  else if(mean_function == "nonlinear")
    mx <- exp(-abs(x[,1])-abs(x[,2]))
  else
    mx <- exp(-abs(x[,1])-abs(x[,2])) + x[,1]*x[,2] 
  
  if(error_dist == "homoscedastic")
    epsilon <- rnorm(n, mean = 0, sd = 1) #change back sd to 1
  else if(error_dist == "heavy-tailed")
    epsilon <- rt(n, df = 2)
  else if(error_dist == "heteroscedastic")
    epsilon <- rnorm(n, mean = 0, sd = sqrt(1+abs(mx)/mean(abs(mx))))
  #else if(error_dist == "skewed")
  #  epsilon <- exp(rnorm(n, mean = 0,sd = sqrt(0.5))) - exp(0.25)
  
  if(!is.null(x0)){
    x0 <- matrix(x0, ncol = p)
    
    if(mean_function == "linear")
      mx0 <- x0[,1] + x0[,2] 
    else if(mean_function == "nonlinear")
      mx0 <- exp(-abs(x0[,1])-abs(x0[,2]))
    else
      mx0 <- exp(-abs(x0[,1])-abs(x0[,2])) + x0[,1]*x0[,2]      
    
    if(error_dist == "homoscedastic")
      epsilon0 <- rnorm(nrow(x0), mean = 0, sd = sqrt(1)) # change back sd to 1
    else if(error_dist == "heavy-tailed")
      epsilon0 <- rt(nrow(x0), df = 2)
    else
      epsilon0 <- rnorm(nrow(x0), mean = 0, sd = sqrt(1+abs(mx0)/mean(abs(mx))))
    
    return(list(x = x, y = mx + epsilon, mx = mx, x0 = x0, y0 = mx0 + epsilon0, mx0 = mx0))
  }
  
  df = data.frame(x = x, y = mx + epsilon)
  colnames(df) = c(paste0('x', 1:p),'y')
  return(df)
}


# sim_data_simple <- function(n = 500,
#                      p = 10,
#                      rho = 0.6,
#                      x0 = NULL,
#                      predictor_dist = c("uncorrelated", "correlated"),
#                      mean_function = c("linear", "nonlinear", "nonlinear-interaction"),
#                      error_dist = c("homoscedastic","heavy-tailed", "heteroscedastic","skewed")){
#   
#   
#   x <- matrix(rnorm(n*p, 0, 1), n, p)
#   mx <- x[,1]^2
#   epsilon <- rnorm(n, mean = 0, sd = 0) #change back sd to 1
#  
#   return(list(x = x, y = mx + epsilon))
# }


data = sim_data(n = 10000,
                p = 4,
                rho = 0.6,
                mean_function = "linear",
                #mean_function = "nonlinear", 
                #mean_function = "nonlinear-interaction",
                error_dist = "homoscedastic",
                #error_dist = "heavy-tailed",
                #error_dist = "heteroscedastic",
                predictor_dist = 'uncorrelated'
                #predictor_dist = 'correlated'
)


plot(data$x1, data$x2)
plot(data$x1, data$x3)
plot(data$x1, data$x4)
plot(data$x2, data$x3)
plot(data$x2, data$x4)
plot(data$x3, data$x4)

plot(data$x1, data$y)
plot(data$x2, data$y)
plot(data$x3, data$y)
plot(data$x4, data$y)


set.seed(12345)
n = dim(data)[1]
train_id = sample(1:n, floor(n * 0.7)) # training ids
train = data[train_id,]

test_id = setdiff(1:n, train_id) # test ids
test_id = sample(test_id, floor(n * 0.3)) # test ids
test = data[test_id,]

lm = lm(y ~ ., data = train)
summary(lm)




rf = randomForest(formula = y ~ ., data = train, ntree = 500)
# predict using the estimated parameters
prediction = predict(rf, newdata = test)
plot(test$x2, test$y, col = 'red')
points(test$x2, prediction, col = 'orange')










































data2 = sim_data_simple(n = 100,
                p = 1,
                rho = 0.6,
                mean_function = "linear",
                #mean_function = "nonlinear", 
                #mean_function = "nonlinear-interaction",
                error_dist = "homoscedastic",
                #error_dist = "heavy-tailed",
                #error_dist = "heteroscedastic",
                #predictor_dist = 'uncorrelated'
                predictor_dist = 'correlated'
)
hist(data2$y)
plot(data2$x[,1], data2$y)

# lm
x = as.data.frame(data2$x)
x$y = data2$y

model = lm(y ~ V1, data = x)
summary(model)
pred = predict(model, x)

plot(data2$x[,1], data2$y)
lines(data2$x[,1], pred, col = "red")



hist(data$y)

plot(data$x[,1], data$x[,2])
plot(data$x[,1], data$x[,3])
plot(data$x[,1], data$x[,4])
plot(data$x[,1], data$x[,5])
plot(data$x[,1], data$x[,6])
plot(data$x[,1], data$x[,7])
plot(data$x[,1], data$x[,8])
plot(data$x[,1], data$x[,9])
plot(data$x[,1], data$x[,10])


plot(data$x[,1], data$y)
plot(data$x[,2], data$y)
plot(data$x[,3], data$y)
plot(data$x[,4], data$y)
plot(data$x[,5], data$y)
plot(data$x[,6], data$y)
plot(data$x[,7], data$y)
plot(data$x[,8], data$y)
plot(data$x[,9], data$y)
plot(data$x[,10], data$y)


# lm
x = as.data.frame(data$x)
x$y = data$y

model = lm(y ~ V1+ V2, data = x)
summary(model)
pred = predict(model, x)
predlm = predict.lm(model, x)
plot(data$x[,1], data$y)
points(data$x[,1], pred, col = "red")
points(data$x[,1], pred, col = "yellow")

xGrid = seq(-3, 3, by = 0.01)
regline = model$coefficients[1] + model$coefficients[2]*xGrid
lines(xGrid,regline)
