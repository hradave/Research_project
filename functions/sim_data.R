library(MASS)

sim_data <- function(n = 500,
                     p = 10,
                     rho = 0.6,
                     predictor_dist = c("uncorrelated", "correlated")){
  
  if(predictor_dist == "uncorrelated"){
    x = matrix(nrow = n, ncol = p)
    for (i in 1:p) {
      x[,i] = rnorm(n, 0, 0.4 + 2*runif(1))
    }
  }
  else{
    Sigma <- diag(rep(1,p))
    for(i in 1:p){
      for(j in i:p){
        Sigma[i,j] <- Sigma[j,i] <- rho^(abs(i-j))
      }
    }
    x <- mvrnorm(n, mu = rep(0,p), Sigma = Sigma)
  }

  # nonlinear mean function
  betas = 5*(-1)^(1:(p-8) + 1) / (1:(p-8) + 1)
  mx = x[,1]^2 + 4*abs(x[,2])- 2*cos(x[,3])*cos((1-x[,4]))+ abs(x[,5]-x[,1])+ 0.5*x[,6]*x[,7] + 5*log(abs(x[,8])+1) + x[,9:p] %*% betas

  # add Gaussian noise
  epsilon <- rnorm(n, mean = 0, sd = 1)
  
  df = data.frame(x = x, y = mx + epsilon)
  colnames(df) = c(paste0('x', 1:p),'y')
  return(df)
}
