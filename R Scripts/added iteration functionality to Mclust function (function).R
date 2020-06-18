# this function runs 
typology <- function(data, n_groups, sample, iterations) {
  i = 1
  # Get the optimal loglikelihood so far
  setwd("C:\\Users\\owner\\Desktop\\Study 4\\Mclust outputs\\")
  ll <- read.csv('optimal loglikelihood')[,-1]
  
  # run mclust to check for better solutions
  for (i in seq(iterations)) {
    print("Iteration #", i)
    i += 1
    result <- Mclust(data, G = n_groups, modelNames = 'VEV')
    
    if (result$loglik == ll[n_groups, sample]) {print("Replication of minimal ll")}
    
    if (result$loglik > ll[n_groups, sample]) {
      best = result

      ll[n_groups, sample] <- result$loglik # update ll value in table 
      print(c("A new ll maximum = ", ll[n_groups, sample] %>% round(2)))
      
      # Save new best results
      best_save <- best$z %>% as.data.frame(); best_save <- cbind(best_save, best$classification)
      print(summary(best))
    }
    
  }
  write.csv(ll, file = 'optimal loglikelihood')
  write.csv(best_save, file = paste0('z and classificaiton for sample ', sample, ' with groups ', n_groups,' ll = ', round(best$loglik,2), ' bic = ', round(best$bic,2)))
  
  best 
}


