
################################ MEAN SQUARED ERROR ###################################

MSE = function(true, predicted){
  return(mean((true - predicted)^2))
}