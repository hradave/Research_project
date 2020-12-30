
############################## SPLIT DATA #################################

split_data <- function(data, size){
  ### Create two subsets(train and test) of a larger dataset of given size and 70%-30% split ratio
  
  ### Arguments
  # data (data.frame): large dataset from which the subsets are sampled
  # size (integer): number of instances in the resulting train + test sets. (The ratio will always be 70-30.)
  
  ### Values in the returned list
  # train (data.frame): training set with size 0.7*size
  # test (data.frame): test set with size 0.3*size
  
  # set seed for reproducibility
  set.seed(12345)
  
  # create training and test pools of the data, to ensure that we use a previously unseen test set
  n = dim(data)[1]
  train_pool_n = floor(0.7*n)
  test_pool_n = floor(0.3*n)
  train_pool = data[1:train_pool_n,]
  test_pool = data[(train_pool_n+1):n,]
  
  # sample instances from the training pool
  train_ids = sample(1:train_pool_n, floor(size*0.7))
  train = train_pool[train_ids,]
  
  # sample instances from the test pool
  test_ids = sample(1:test_pool_n, floor(size*0.3))
  test = test_pool[test_ids,]
  
  return(list(train = train,
              test = test))
}
