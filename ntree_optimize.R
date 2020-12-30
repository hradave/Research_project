
############################## OPTIMIZE NTREE #################################

# find the best ntree parameters for the random forest
library(randomForest)

# ntree values to do grid search on
ntree_seq = seq(25,500,by=25)
ntree_table = matrix(nrow=length(ntree_seq), ncol = 3)
ntree_table = as.data.frame(ntree_table)
colnames(ntree_table) = c('ntree', 'MSE', 'runtime')

load("data/synth_data_50.RData")
dataset = dataset[1:10000,]
source('functions/MSE.R')

set.seed(12345)
# Divide data into train and test sets (70% - 30%)
n = dim(dataset)[1]
train_id = sample(1:n, floor(n * 0.7)) # training ids
train = dataset[train_id,]

test_id = setdiff(1:n, train_id) # test ids
test = dataset[test_id,]

for (i in 1:length(ntree_seq)){
  print(i)
  ntree_table[i,1] = ntree_seq[i]
  start_time <- Sys.time()
  rF = randomForest(formula = y~., data = train, ntree = ntree_seq[i])
  end_time <- Sys.time()
  runtime = as.numeric(difftime(end_time, start_time, units = 'secs'))
  ntree_table[i,3] = runtime
  pred = predict(rF, test)
  ntree_table[i,2] = MSE(test$y, pred)
}

# save df for future use
save(ntree_table, file = "data/ntree_table.RData")


par(mar = c(5,5,2,5))
with(ntree_table, plot(ntree, MSE, type="l", col='blue', ylab = 'MSE', xlab = 'Number of trees'))
par(new = T)
with(ntree_table, plot(ntree, runtime, type = 'l', col = 'red', ylab = NA, xlab = NA, axes = F))
axis(side = 4)
mtext(side = 4, line = 3, 'Runtime (secs)')
legend(x = 50, y= 140,
       legend=c('MSE', 'Runtime'),
       lty=c(1,1), col=c('blue', 'red'))


# OPTIMAL CHOICE = 125

# check performance of linear model
lmodel = lm(formula = y ~ ., data = train)
summary(lmodel)
pred = predict(lmodel, test)
plot(test$x1, test$y, pch = 15, col = 'black', main = 'LM'
     #,ylim=c(-6, 1000)
)
points(test$x1, pred, col = 'red', pch = 16)
MSE(test$y, pred) #18.4


  