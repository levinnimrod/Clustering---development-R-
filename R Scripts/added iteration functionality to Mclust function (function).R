typology <- function(data, n_groups, sample, iterations) {
  i = 1

  #  setwd("C:\\Users\\owner\\Desktop\\Study 4\\Mclust outputs\\")
    setwd('/Users/nimrodlevin/Desktop/Study 4/Mclust outputs')
  
# Get the optimal log-likelihood and total iterations so far
  ll <- read.csv('optimal loglikelihood')[,-1]
  total_iteration = read.csv('total iterations')[-1]
  
  # to control for a new optimal result
  new = 0
  
  # run mclust to check for better solutions
  for (i in seq(iterations)) {
    print(paste0("Iteration #", i + total_iteration[n_groups, sample], " for sample #", sample, ' and groups = ', n_groups))
    i = i + 1
  #  mclust.options(subset = 600)
    result <- Mclust(data, G = n_groups, modelNames = c('VEV', 'VVE'))
    
    if (result$loglik >= ll[n_groups, sample]) {
      
      # compute the difference for the new best ll compared to previous
      ll[n_groups, sample + 6] = ll[n_groups, sample + 2]
      ll[n_groups, sample + 2] = i + total_iteration[n_groups, sample] - 1
      ll[n_groups, sample + 4] = (result$loglik-ll[n_groups, sample]) %>% round(2)
      ll[n_groups, sample + 8] = result$modelName
      ll[n_groups, sample + 10] = result$df
      
      # save the new best results
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
  write.csv(best_save, file = paste0('Clustering results/z and classificaiton for sample ', sample, ' with groups ', n_groups))
    best 
  }
  else NA
}



