
############################## CREATE SIMULATED DATASET #################################


source('functions/sim_data.R')
source('functions/normalize.R')
set.seed(1234)
dataset = sim_data(n = 100000,
                   p = 50,
                   rho = 0.6,
                   #predictor_dist = 'uncorrelated'
                   predictor_dist = 'correlated'
                   )

# visual investigation
plot(dataset)
plot(dataset$x1, dataset$y)
hist(normalize(dataset$y), breaks = 50)
data = dataset[1:10000,]

save(dataset, file = "data/synth_data_50.RData")


# check if it can be modelled with a linear model
lm = lm(formula = y ~ ., data = data)
summary(lm)
pred = predict(lm, data)
plot(data$x1, data$y, pch = 15, col = 'black', main = 'LM'
     #,ylim=c(-6, 1000)
     )
points(data$x1, pred, col = 'red', pch = 16)
mean(abs(data$y-pred)) #3.24 error


# check performance of random forest
start_time <- Sys.time()
rF = randomForest(formula = y~., data = data, ntree = 50)
end_time <- Sys.time()
end_time - start_time 
# n1000p50(ntree=500) = 5.78 sec
# n10000p50(ntree=500) = 3.8 mins
# n10000p50(ntree=100) = 45 sec
# n10000p50(ntree=50) = 21 sec


pred_rf = predict(rF, data)
plot(data$x1, data$y, pch = 15, col = 'black', main = 'RF'
#     ,ylim=c(-6, 1000)
)
points(data$x1, pred_rf, col = 'red', pch = 16)
mean(abs(data$y-pred_rf)) #0.86, 0.71 error
