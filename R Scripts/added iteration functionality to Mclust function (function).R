typology <- function(data, n_groups, sample, seed = 1) {

  #  setwd("C:\\Users\\owner\\Desktop\\Study 4\\Mclust outputs\\")
    setwd('/Users/nimrodlevin/Desktop/Study 4/Mclust outputs')
  
# Get the optimal log-likelihood and total iterations so far
  if (!file.exists(paste0('Optimal Likelihood/', seed))){
    ll <- read.csv(paste0('Optimal Likelihood/', 0))[,-1]
  } else {ll <- read.csv(paste0('Optimal Likelihood/', seed))[,-1]}
  
  total_iterations = read.csv('total iterations')[, -1]

  #  count numebr of iterations
  i = 0
  bestll = -10000000000; result$loglik == 0
  mclust.options(hcUse = 'RND', subset = 20)
  randpairs <- randomPairs(data, seed)
  
  # run mclust to check for better solutions
  while (i < 2000) {  
    i = i + 1
    print(paste0("Iteration #", i, " for sample #", sample, ' and groups = ', n_groups, ' (seed = ', seed, ')'))
    result <- Mclust(data, G = n_groups, modelNames = 'EII', initialization = list(hcpairs = randpairs))
    
    if (result$loglik >= ll[n_groups, sample]) {
      
      # compute the difference for the new best ll compared to previous
      
      
      ll[n_groups, sample + 6] = ll[n_groups, sample + 2]
      ll[n_groups, sample + 2] = i
      ll[n_groups, sample + 4] = (result$loglik-ll[n_groups, sample]) %>% round(2)
      ll[n_groups, sample + 8] = result$modelName
      ll[n_groups, sample + 10] = result$df
      
      # save the new best results
      bestll = result$loglik

      ll[n_groups, sample] <- result$loglik %>% round(2) # update ll value in table 
      print(c("A new ll maximum = ", ll[n_groups, sample] %>% round(2)))
      
      # Save new best results
      best_save <- result$z %>% as.data.frame(); best_save <- cbind(best_save, result$classification, result$uncertainty)
      print(summary(result))
    }
    
  }
  
  # save new data for optimal ll + total iterations
  write.csv(ll, file = paste0('Optimal Likelihood/', seed))

  total_iterations[seed, (sample-1)*9 + n_groups - 1] = i
  
  write.csv(total_iterations, file = 'total iterations')
  
  # save the results only if there is a new optimal result
  max <- read.csv('maximum loglikelihood')[-1]
  if (bestll >= max[n_groups, sample]) {
  write.csv(best_save, file = paste0('Clustering results/z and classificaiton for sample ', sample, ' with groups ', n_groups))
  max[n_groups, sample] <- bestll %>% round(2)
  max[n_groups, sample + 2] <- seed
  write.csv(max, 'maximum loglikelihood')
  }
}

