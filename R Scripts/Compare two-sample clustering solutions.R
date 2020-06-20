####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
remove(list = ls())
library(dplyr); library(ggplot2)

####################      LOAD THE CLUSTERING SOLUTIONS                ####################
setwd(choose.dir())
files <- list.files()
ldf <- lapply(files, read.csv); remove(files)

####################      LOAD TIDY FILES AND SUBSET FOR THE RELEVANT SAMPLE                ####################
cddq <- file.choose() %>% read.csv

# get only participants with RCA based in the USA
cddq <- cddq[!is.na(cddq$RCA) & cddq$Country == "USA", ]

# remove participants with no difference in responses
url <- file.choose(); source(url)
cddq <- cddq[!exclude(cddq[, c(49:51, 53:56, 58:60)]) == 0, ]; remove(url, exclude)

####################      CREATE TWO RANDOM SAMPLES                ####################
sample1 <- cddq[seq(1, nrow(cddq), by = 2), ]; sample1 <- sample1[, -1]; 
sample2 <- cddq[seq(2, nrow(cddq), by = 2), ]; sample2 <- sample2[, -1];
remove(cddq)

# get only the variables you need
sample1 <- (sample1[, c(4:5, 8:11, 48:50, 52:55, 57:59, 61)]) %>% round(2)
sample2 <- (sample2[, c(4:5, 8:11, 48:50, 52:55, 57:59, 61)]) %>% round(2)

####################      EXCTRACT CLASSIFICATIONS FROM THE SOLUTIONS                ####################
classifications = data.frame(rep(NA, nrow(ldf[[1]])))

for (i in seq(18)){ 
  classifications[i] = ldf[[i]]['best.classification']
}

classifications <- cbind(classifications[,2:9], classifications[,1], classifications[,11:18], classifications[,10])
colnames(classifications) <- paste0("S", c(rep(1,9), rep(2, 9)), "_G", seq(2,10))

####################      EVALUATE THE MEAN DIFFERENCES                ####################
names <- c(paste0('S1_G', 2:10), paste0('S2_G', 2:10))

# define which two solutions you want to compare (i = ?)
i = 4

results = cbind(
# Exctract the mean values of the variables for the clustering groups from sample 1
aggregate(sample1[, 7:17], by = as.data.frame(classifications[names[i-1]]), FUN = mean) %>% round(2) %>%
       arrange(desc(total)) %>% t,

# Exctract the mean values of the variables for the clustering groups from sample 2
aggregate(sample2[, 7:17], by = as.data.frame(classifications[names[i - 1 + 9]]), FUN = mean) %>% round(2) %>%
       arrange(desc(total)) %>% t)   %>% as.data.frame()

results[1, ] <- c(rep(1, i), rep(2, i));
results <- t(results) %>% as.data.frame;
arrange(results, desc(total)) %>% t

####################      COMPUTE THE MEAN OF THE MAXIMUM DIFFERENCE BETWEEN SAMPLES ACROSS THE 10 SCORES                ####################
results = rep(NA, 12) %>% as.data.frame()

for (i in seq(9)) {

results[, i] = 
# Exctract the mean values of the variables for the clustering groups from sample 1
(aggregate(sample1[, 7:17], by = as.data.frame(classifications[names[i]]), FUN = mean) %>% round(2) %>%
        arrange(desc(total)) - 
# then substract the mean values of the variables for the clustering groups from sample 2 (get the absolute values)
aggregate(sample2[, 7:17], by = as.data.frame(classifications[names[i + 9]]), FUN = mean) %>%
  arrange(desc(total))) %>% abs %>%
  
  apply(2, max) %>% round(2)

}
results <- results[-1, ] %>% as.data.frame()
rownames(results) <- colnames(sample1[, 7:17]); colnames(results) <- c(seq(2, 10))
results  
results[1:10,] %>% colMeans() %>% round(2)

colnames(sample1)
sol <- Mclust(sample1[,7:16], G = 4)
MclustBootstrap(sol, nboot = 20) %>% summary
sol <- mclustBootstrapLRT(sample[,7:16])
mclustBootstrapLRT(sample1[,7:16], model = 'VEV', nboot = 100)
mclustBootstrapLRT(sample2[,7:16], model = 'VEV', nboot = 100)


