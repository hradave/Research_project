# simulate data here

source('sim_data.R')

dataset = sim_data(n = 500,
                   p = 2,
                   rho = 0.6,
                   #mean_function = "linear",
                   mean_function = "nonlinear", 
                   #mean_function = "nonlinear-interaction",
                   error_dist = "homoscedastic",
                   #error_dist = "heavy-tailed",
                   #error_dist = "heteroscedastic",
                   predictor_dist = 'uncorrelated'
                   #predictor_dist = 'correlated'
                   )

plot(dataset)
lm = lm(formula = y ~ ., data = dataset2)
summary(lm)
