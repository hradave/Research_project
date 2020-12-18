split_data <- function(data, size){
  # set seed, so that the data splits will always be the same
  # good for testing different settings, but might remove in final version
  set.seed(12345)
  
  # create subset of the "big" dataframe
  n = dim(data)[1]
  id = sample(1:n, size)
  data = data[id,]
  
  # Divide data into train and test sets (70% - 30%)
  
  train_id = sample(1:size, floor(size * 0.7)) # training ids
  train = data[train_id,]
  
  test_id = setdiff(1:size, train_id) # test ids
  test = data[test_id,]
  
  return(list(train = train,
              test = test))
}
