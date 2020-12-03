# 1 

x1 <- 11:30
x2 <- runif(20,5,95)
x3 <- rbinom(20,1,.5)

b0 <- 17
b1 <- 0.5
b2 <- 0.037
b3 <- -5.2
sigma <- 1.4

eps <- rnorm(x1,0,sigma)
y <- b0 + b1*x1  + b2*x2  + b3*x3 + eps


# 2
library(simstudy)
def <- defData(varname = "x1", dist = "normal", formula = 10, variance = 2)
def <- defData(def, varname = "x2", dist = "normal", formula = "-2 + x1 * 0.1", variance = 0.01)
def <- defData(def, varname = "x3", dist = "normal", formula = "1.5 - 0.2 * x1 + 0.5 * x2")

dd <- genData(1000, def)
dd

hist(dd$x1)
hist(dd$x2)
hist(dd$x3)

plot(dd$x1, dd$x2)
plot(dd$x1, dd$x3)
plot(dd$x2, dd$x3)

train = dd[0:7000]
test = dd[7001:10000]

model = lm(x3 ~ x1 + x2, data = train)
summary(model)
pred = predict(model, newdata = test[,2:3])
plot(test$x1, test$x3, type = 'l')
lines(test$x1, pred, col = 'red')



# 3

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
    x <- mvrnorm(n, mu = rep(0,p), Sigma = Sigma)
  }
  
  if(mean_function == "linear")
    mx <- x[,1] + 10*x[,2] 
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
  
  return(list(x = x, y = mx + epsilon))
}


data = sim_data(n = 10000,
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




# generate beta coefficients with loop 
# https://blogs.sas.com/content/iml/2017/01/25/simulate-regression-model-sas.html
for (k in 1:10) {
  print(4*(-1)^(k+1)/(k+1))
}

# lm
x = as.data.frame(data$x)
x$y = data$y

model = lm(y ~ V1+ V2, data = x)
summary(model)

