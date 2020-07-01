####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
remove(list = ls())
library(dplyr); library(tidyLPA)

####################      LOAD ALL SOLUTIONS                ####################
setwd(choose.dir()) # pc
#setwd('/Users/nimrodlevin/Desktop/Study 4/Mclust outputs/Clustering results/') # mac
files <- list.files()
ldf <- lapply(files, read.csv); remove(files)

####################      SET THE RESULTS TABLE AND ADD THE LL VALUES                ####################
# set the results table
results = c(rep(1,9), rep(2, 9))
results <- as.data.frame(results); 

results$profiles <- c(rep(2:10,2)); colnames(results) <- c('Sample', 'Profiles')

# extract the log-likelihood values
results$LL = NA
results$LL[1:9] <- read.csv('../maximum loglikelihood')[-1, 2]
results$LL[10:18] <- read.csv('../maximum loglikelihood')[-1, 3]

# set the relevant degrees of freedom
results$df<- rep(seq(2:10)*11,2)


# Set sample size
n = nrow(ldf[[1]])
  
####################     CALCULATE THE LRT p-values                ####################

results$LRTp = NA
for (i in c(seq(1,8), seq(10,17))) {
  results$LRTp[i] = calc_lrt(n, results$LL[i], results$df[i], results$Profiles[i],
                            results$LL[i + 1], results$df[i + 1], results$Profiles[i + 1])[4] %>% round(2)
}

# reorder rows according to file order
results <- rbind(results[9, ], results[1:8,], results[18,], results[10:17,])

####################     CALCULATE THE AIC + BIC VALUEs                ####################

results$AIC = -2*results$LL+2*results$df

results$BIC = -2*results$LL + results$df*log(n)

results$SaBIC = -2*results$LL + results$df*log((n + 2) / 24)

####################     CALCULATE THE RELATIVE ENTROPY VALUE                ####################
results$entropy = NA

for (i in seq(2, 19)) {
   results$entropy[i - 1] <- (-log(1/results$Profiles[i-1])*n + 
     sum(ldf[[i-1]][,2:results$Profiles[i-1]]*log(ldf[[i-1]][,2:results$Profiles[i-1]], 2), na.rm = TRUE)) /
     (-log(1/results$Profiles[i-1])*n)
   results$entropy <- round(results$entropy,2)
}

####################      GET MIN AND MAX NUMBER OF PARTICIPANTS IN A GROUP                ####################
# get minimum number of participants
results$min = NA
results$min_per = NA
results$max = NA
results$max_per = NA

for (i in seq(1, 18)) {
  results$min[i] = ldf[[i]]['result.classification'] %>% table %>% min
  results$min_per[i] = 100*results$min[i]/n ; results$min_per <- results$min_per %>% round(2)
  
  results$max[i] = ldf[[i]]['result.classification'] %>% table %>% max
  results$max_per[i] = 100*results$max[i]/n ; results$max_per <- results$max_per %>% round(2)

}

# reorder rows according to profiles
results <- rbind(results[2:9, ], results[1,], results[11:18,], results[10,])
results <- cbind(results[, 1:4], results[, 6:8], results[, 5], results[, 9:13]);
colnames(results) <- c("Sample", "Profiles", "LL", "df", "AIC", "BIC", "SaBIC",
                       "LRTp", "Entropy", "min", "min_per", "max", "max_per")
results

