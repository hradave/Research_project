
########## PLOT RESULTS (NOT USED IN THE REPORT)

plot_prediction_bands <- function(data, x, y, sample_size = 100, type = 'cp', title = '', conf = 0.95, bs_type = 'e'){
  
  # type has to be either 'cp' or 'bs'
  # bs_type has to be either 'e' or 'q'
  
  # draw random sample from data
  data_sample = data[sample(1:dim(data)[1], sample_size),]
  # order by x for plotting
  data_sample = data_sample[order(data_sample[,x]),]
  
  # plot data points and their conformal prediction bands
  plot(data_sample[,x], data_sample[,y], pch=15, col="black", xlab = x, ylab = y,
       #,main = paste0(title1, '(', conf * 100, '%) ', title)
       ylim = c(min(data_sample$lower, data_sample$upper, data_sample$pred, data_sample[,y]),
                max(data_sample$lower, data_sample$upper, data_sample$pred, data_sample[,y])))
  points(data_sample[,x], data_sample$pred, pch=21, bg="yellow")
  
  if (type == 'cp') {
    lines(data_sample[,x], data_sample$lower, col = 'blue')
    lines(data_sample[,x], data_sample$upper, col = 'blue')
    title(main = paste0('Conformal prediction ', '(', conf * 100, '%) ', title))
    outside = length(which(((data_sample[,y] >= data_sample$lower) * (data_sample[,y] <= data_sample$upper)) == 0))
    
  } else {
    if (bs_type == 'e') {
      lines(data_sample[,x], data_sample$lower_e, col = 'blue')
      lines(data_sample[,x], data_sample$upper_e, col = 'blue')
      title(main = paste0('Bootstrap prediction ', '(', conf * 100, '% envelope) ', title))
      outside = length(which(((data_sample[,y] >= data_sample$lower_e) * (data_sample[,y] <= data_sample$upper_e)) == 0))
      
    } else {
      lines(data_sample[,x], data_sample$lower_q, col = 'blue')
      lines(data_sample[,x], data_sample$upper_q, col = 'blue')
      title(main = paste0('Bootstrap prediction ', '(', conf * 100, '% quantile) ', title))
      outside = length(which(((data_sample[,y] >= data_sample$lower_q) * (data_sample[,y] <= data_sample$upper_q)) == 0))
      
    }
  }
  legend('topleft', legend = c('True', 'Predicted', 'Prediction band'), pch = c(15,21,NA), 
         lty = c(NA,NA,1), col = c('black', 'black', 'blue'), pt.bg = c(NA,'yellow', NA))
  
  #outside = dim(data_sample %>% filter(Rings < lower | Rings > upper))[1]
  #outside = length(which(((data_sample[,y] >= data_sample$lower) * (data_sample[,y] <= data_sample$upper)) == 0))
  paste0('True data points outside of the prediction bands: ', outside, ' (', outside/sample_size*100,'%)')
}