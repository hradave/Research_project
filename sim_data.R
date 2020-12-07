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
  
  betas = 10*(-1)^(1:p + 1) / (1:p + 1)
  
  if(mean_function == "linear"){
    #mx <- 5*x[,1] - 3.3*x[,2] + 2.5 * x[,3]
    mx = x %*% betas
  }
  else if(mean_function == "nonlinear"){
    #mx <- exp(-abs(5*x[,1])-abs(-3.33*x[,2]))
    mx = 0
    for (i in 1:p) {
      mx = mx - betas[i] * abs(x[,i])
    }
    mx = exp(mx)
  }
  else{
    mx <- exp(-abs(x[,1])-abs(x[,2])) + x[,1]*x[,2]
  } 
  
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
