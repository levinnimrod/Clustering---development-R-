####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
remove(list = ls())
library(dplyr); library(ggplot2); library(lsr); 

####################      LOAD THE CLUSTERING SOLUTIONS                ####################
setwd(choose.dir())
files <- list.files()
ldf <- lapply(files, read.csv); remove(files)

####################      LOAD TIDY FILES AND SUBSET FOR THE RELEVANT SAMPLE                ####################
cddq <- read.csv('../../../df.csv')

# get only participants with RCA based in the USA
cddq <- cddq[!is.na(cddq$RCA) & cddq$Country == "USA", ]

# remove participants with no difference in responses
source('../../R Scripts/zero-variance validity check (function).R')
cddq <- cddq[!exclude(cddq[, c(49:51, 53:56, 58:60)]) == 0, ]; remove(exclude)

####################      CREATE TWO RANDOM SAMPLES                ####################
sample1 <- cddq[seq(1, nrow(cddq), by = 2), ]; sample1 <- sample1[, -1]; 
sample2 <- cddq[seq(2, nrow(cddq), by = 2), ]; sample2 <- sample2[, -1];
remove(cddq)

# get only the variables you need
sample1 <- (sample1[, c(4:5, 8:11, 48:50, 52:55, 57:59, 61)]) %>% round(2)
sample2 <- (sample2[, c(4:5, 8:11, 48:50, 52:55, 57:59, 61)]) %>% round(2)

####################      CALCULATE RI SCORE                ####################
sample1$LI <- (sample1$Lp + sample1$Lo + sample1$Lp + sample1$Ls) / 4 %>% round(2)
sample2$LI <- (sample2$Lp + sample2$Lo + sample2$Lp + sample2$Ls) / 4 %>% round(2)

####################      EXCTRACT CLASSIFICATIONS FROM THE SOLUTIONS                ####################
classifications = data.frame(rep(NA, nrow(ldf[[1]])))
uncertainty = data.frame(rep(NA, nrow(ldf[[1]])))

for (i in seq(18)){ 
  classifications[i] = ldf[[i]]['result.classification']
  #  uncertainty[i] = ldf[[i]]['result.uncertainty']
  
}

classifications <- cbind(classifications[,2:9], classifications[,1], classifications[,11:18], classifications[,10])
colnames(classifications) <- paste0("S", c(rep(1,9), rep(2, 9)), "_G", seq(2,10))

####################      EVALUATE THE MEAN DIFFERENCES - Ri, Rd, LI, Ie                ####################
names <- c(paste0('S1_G', 2:10), paste0('S2_G', 2:10))

# define which two solutions you want to compare (i = ?)
i = 3

aggregate(sample1[c('Ri', 'Rd', 'LI', 'Ie')], by = as.data.frame(classifications[names[i-1]]), FUN = mean) 


# Extract the mean values of the variables for the clustering groups from sample 1
results = cbind(aggregate(sample1[c('Ri', 'Rd', 'LI', 'Ie', 'total')], by = as.data.frame(classifications[names[i-1]]), 
                          FUN = mean) %>% round(2), table(classifications[names[i-1]])) %>% arrange(desc(total)) %>% t

# Extract the mean values of the variables for the clustering groups from sample 2
results = results %>% cbind(cbind(aggregate(sample2[c('Ri', 'Rd', 'LI', 'Ie', 'total')], by = as.data.frame(classifications[names[i-1+9]]), 
                                            FUN = mean) %>% round(2), table(classifications[names[i-1+9]])) %>% arrange(desc(total)) %>% t) 


results[1, ] <- c(rep(1, i), rep(2, i)); results <- t(results) %>% as.data.frame; 
results <- arrange(results, desc(total)) %>% t %>% as.data.frame(); 
results

####################      EVALUATE THE MEAN DIFFERENCES - ALL 10 DIMENSIONS               ####################
names <- c(paste0('S1_G', 2:10), paste0('S2_G', 2:10))

# define which two solutions you want to compare (i = ?)
i = 5

# Extract the mean values of the variables for the clustering groups from sample 1
results = cbind(aggregate(sample1[, 7:17], by = as.data.frame(classifications[names[i-1]]), 
                          FUN = mean) %>% round(2), table(classifications[names[i-1]])) %>% arrange(desc(total)) %>% t

# Extract the mean values of the variables for the clustering groups from sample 2
results = results %>% cbind(cbind(aggregate(sample2[, 7:17], by = as.data.frame(classifications[names[i-1+9]]), 
                                            FUN = mean) %>% round(2), table(classifications[names[i-1+9]]))  %>% arrange(desc(total)) %>% t) 


results[1, ] <- c(rep(1, i), rep(2, i)); results <- t(results) %>% as.data.frame; 
results <- arrange(results, desc(total)) %>% t %>% as.data.frame(); 
x <- results; results




###################      COMPUTE THE MEAN OF THE MAXIMUM DIFFERENCE BETWEEN SAMPLES ACROSS THE 10 SCORES                ####################
results = rep(NA, 12) %>% as.data.frame()

for (i in seq(9)) {
  
  results[, i] =
    # Extract the mean values of the variables for the clustering groups from sample 1
    (aggregate(sample1[, 7:17], by = as.data.frame(classifications[names[i]]), FUN = mean) %>% round(2) %>%
       arrange(desc(total)) -
       # then substract the mean values of the variables for the clustering groups from sample 2 (get the absolute values)
       aggregate(sample2[, 7:17], by = as.data.frame(classifications[names[i + 9]]), FUN = mean) %>%
       arrange(desc(total))) %>% abs %>%
    
    apply(2, mean) %>% round(2)
  
}
results <- results[-1, ] %>% as.data.frame()
rownames(results) <- colnames(sample1[, 7:17]); colnames(results) <- c(seq(2, 10))
results
results[1:10,] %>% colMeans() %>% round(2)
  