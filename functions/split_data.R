split_data <- function(data, size){
  # set seed, so that the data splits will always be the same
  set.seed(12345)
  
  # create training and test pools of the "big" dataframe, to ensure that we use a previously unseen test set
  n = dim(data)[1]
  train_pool_n = floor(0.7*n)
  test_pool_n = floor(0.3*n)
  train_pool = data[1:train_pool_n,]
  test_pool = data[(train_pool_n+1):n,]
  
  train_ids = sample(1:train_pool_n, floor(size*0.7))
  train = train_pool[train_ids,]
  
  test_ids = sample(1:test_pool_n, floor(size*0.3))
  test = test_pool[test_ids,]
  
  return(list(train = train,
              test = test))
}
