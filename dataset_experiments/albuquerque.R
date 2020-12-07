# read data
housing = read.csv('data\\albuquerque.csv')
plot(housing$Area, housing$Price, pch = 20)
hist(housing$Price)


library(boot)

housing = housing[order(housing$Area),]#reordering data according to Area
# check if reordering is only for plotting, or does it influence the coverage rate and interval lengths

# nonparametric bootstrap
# # computing non-parametric bootstrap samples
# f=function(data, ind){
#   data1=data[ind,]# extract bootstrap sample
#   res=lm(Price~Area, data=data1) #fit linear model
#   #predict values for all Area values from the original data
#   priceP=predict(res,newdata=data2)
#   return(priceP)
# }
# res=boot(data2, f, R=1000) #make bootstrap
# res$t0
# summary(res)
# res$R
# plot(res)
# print(res)
# c(res)



# parametric bootstrap

# estimate the parameters
mle = lm(Price ~ Area, data = housing)

# function to generate random dataset from the original one and the estimates
rng = function(data, mle) {
  data_gen = data.frame(Price = data$Price, Area = data$Area)
  n = length(data$Price)
  # generate new Price (response)
  data_gen$Price = rnorm(n, predict(mle, newdata = data_gen), sd(mle$residuals))
  return(data_gen)
}

# function to fit model to randomly generated dataset (by rng) and predict to some original set (test)
prediction = function(data_gen){
  # fit model
  model = lm(Price ~ Area, data = data_gen)
  # predict expected values for all Area values from the original data
  priceP = predict(model, newdata = housing)
  # simulate prediction with added Gaussian noise
  sim = rnorm(length(housing$Price), priceP, sd(mle$residuals))
  return(sim)
}

param_bootstrap = boot(housing, statistic = prediction, R = 1000, mle = mle, ran.gen = rng, sim = "parametric")

plot(param_bootstrap)

# f2=function(data1){
#   res=lm(Price~Area, data=data1) #fit linear model
#   #predict values for all Area values from the original data
#   priceP=predict(res,newdata=housing)
#   sim = rnorm(length(housing$Price), priceP, sd(summary(mle)$residuals))
#   return(sim)
# }
# res3 = boot(housing, statistic=f2, R=1000, mle=mle,ran.gen=rng, sim="parametric")



conf = 0.90

#compute prediction bands
e = envelope(param_bootstrap, level = conf)
# check if envelope() does the same thing as quantile()

fit = lm(Price ~ Area, data = housing)
priceP = predict(fit, newdata = housing)
plot(housing$Area, housing$Price, pch=21, bg="orange")
points(housing$Area, priceP, type="l") #plot fitted line
#plot prediction bands
points(housing$Area,e$point[2,], col = 'blue', type="l")
points(housing$Area,e$point[1,], col = 'blue', type="l")


quantiles = apply(param_bootstrap$t, 2, quantile, probs = c((1-conf)/2, 1-(1-conf)/2))
plot(housing$Area, housing$Price, pch=21, bg="orange")
points(housing$Area, priceP, type="l") #plot fitted line
#plot prediction bands
points(housing$Area,quantiles[1,], col = 'blue', type="l")
points(housing$Area,quantiles[2,], col = 'blue', type="l")

# almost the same results with envelope and quantile


# check residuals (they are the same)
mle=lm(Price~Area, data=housing)
mle$residuals
pred = predict(mle, housing)
residual = housing$Price - pred
residual
residual == mle$residuals