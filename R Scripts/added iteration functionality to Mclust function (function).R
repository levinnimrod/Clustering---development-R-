# this function runs 
typology <- function(data, n_groups, sample, iterations) {
  i = 1
  # Get the optimal loglikelihood and total iterations so far
  setwd("C:\\Users\\owner\\Desktop\\Study 4\\Mclust outputs\\")
  ll <- read.csv('optimal loglikelihood')[,-1]
  total_iteration = read.csv('total iterations')[-1]
  
  # to control for a new optimal result
  new = 0
  
  # run mclust to check for better solutions
  for (i in seq(iterations)) {
    print(paste0("Iteration #", i, " for sample #", sample, ' and groups = ', n_groups))
    i = i + 1
    result <- Mclust(data, G = n_groups, modelNames = 'VEV')
    
    if (result$loglik == ll[n_groups, sample]) {print("Replication of minimal ll")}
    
    if (result$loglik > ll[n_groups, sample]) {
      best = result

      ll[n_groups, sample] <- result$loglik %>% round(2) # update ll value in table 
      print(c("A new ll maximum = ", ll[n_groups, sample] %>% round(2)))
      
      # Save new best results
      best_save <- best$z %>% as.data.frame(); best_save <- cbind(best_save, best$classification)
      new = 1
      print(summary(best))
    }
    
  }
  
  # save new data for optimal ll + total iterations
  write.csv(ll, file = 'optimal loglikelihood')
  total_iteration[n_groups, sample] = total_iteration[n_groups, sample] + i - 1
  write.csv(total_iteration, file = 'total iterations')
  
  # save the results only if there is a new optimal result
  if (new == 1) {
  write.csv(best_save, file = paste0('z and classificaiton for sample ', sample, ' with groups ', n_groups,' ll = ', round(best$loglik,2), ' bic = ', round(best$bic,2)))
    best 
  }
  else NA
  
  
  
}