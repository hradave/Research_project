
################################ SIMULATE ARTIFICIAL DATA ###################################

# load library
library(MASS)

sim_data <- function(n = 500,
                     p = 10,
                     rho = 0.6,
                     predictor_dist = c("uncorrelated", "correlated")){
  ### Create artificial dataset for regression with one target variable
  
  ### Arguments
  # n (integer): number of observations to simulate
  # p (integer): number of features to simulate
  # rho (numeric): rho value for the AR(1) covariance matrix
  # predictor_dist (character): specifies whether the features are correlated or not. If not, rho is ignored.
  
  ### Values
  # df (data.frame): simulated dataset for regression containing p features and 1 target variable
  
  
  if(predictor_dist == "uncorrelated"){
    # features are independent
    x = matrix(nrow = n, ncol = p)
    for (i in 1:p) {
      # simulate x from the univariate normal distribution with somewhat random standard deviation
      x[,i] = rnorm(n, 0, 0.4 + 2*runif(1))
    }
  }
  else{
    # features are not independent
    Sigma <- diag(rep(1,p))
    for(i in 1:p){
      for(j in i:p){
        # AR(1) process
        Sigma[i,j] <- Sigma[j,i] <- rho^(abs(i-j))
      }
    }
    # simulate x matrix from the multivariate normal distribution with Sigma as covariance matrix
    x <- mvrnorm(n, mu = rep(0,p), Sigma = Sigma)
  }

  ## nonlinear mean function (the first 8 features at least)
  # create coefficients for the linear part
  betas = 5*(-1)^(1:(p-8) + 1) / (1:(p-8) + 1)
  mx = x[,1]^2 + 4*abs(x[,2])- 2*cos(x[,3])*cos((1-x[,4]))+ abs(x[,5]-x[,1])+ 0.5*x[,6]*x[,7] + 5*log(abs(x[,8])+1) + x[,9:p] %*% betas

  # add Gaussian noise
  epsilon <- rnorm(n, mean = 0, sd = 1)
  
  df = data.frame(x = x, y = mx + epsilon)
  colnames(df) = c(paste0('x', 1:p),'y')
  return(df)
}
