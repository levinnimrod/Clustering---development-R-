####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
remove(list = ls())
library(dplyr); library(tidyr); library(ggplot2); library(gridExtra)
graphics.off()


####################      DERIVE THE MAXIMUM LL FROM EACH RANDOM START                ####################
setwd(choose.dir()) # choose the directory "Optimal Likelihood" (pc)
setwd('/Users/nimrodlevin/Desktop/Study 4/Mclust outputs/Optimal Likelihood/') # mac
files <- list.files()
ldf = rep(NA,2) %>% as.data.frame()

for (i in seq(length(files)-2)) {
  ldf[1:9, i] <- read.table(as.character(i), sep = ',', header = TRUE)[-1, 2] %>% round(2)
  ldf[10:18, i] <- read.table(as.character(i), sep = ',', header = TRUE)[-1, 3] %>% round(2)
}

colnames(ldf) <- paste0("Seed_", seq(length(files)-2))

# And transpose the data set
ldf <- ldf %>% t %>% as.data.frame()
colnames(ldf) <- paste0("S", c(rep(1,9), rep(2, 9)), "_G", rep(2:10))

####################      PLOT THE SOLUTIONS WITH ALL DATA POINTS                ####################

 plot1_2 <- ggplot(ldf, aes(x=S1_G2, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot1_3 <- ggplot(ldf, aes(x=S1_G3, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot1_4 <- ggplot(ldf, aes(x=S1_G4, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot1_5 <- ggplot(ldf, aes(x=S1_G5, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot1_6 <- ggplot(ldf, aes(x=S1_G6, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot1_7 <- ggplot(ldf, aes(x=S1_G7, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot1_8 <- ggplot(ldf, aes(x=S1_G8, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot1_9 <- ggplot(ldf, aes(x=S1_G9, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot1_10 <- ggplot(ldf, aes(x=S1_G10, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')

 plot2_2 <- ggplot(ldf, aes(x=S2_G2, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot2_3 <- ggplot(ldf, aes(x=S2_G3, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot2_4 <- ggplot(ldf, aes(x=S2_G4, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot2_5 <- ggplot(ldf, aes(x=S2_G5, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot2_6 <- ggplot(ldf, aes(x=S2_G6, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot2_7 <- ggplot(ldf, aes(x=S2_G7, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot2_8 <- ggplot(ldf, aes(x=S2_G8, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot2_9 <- ggplot(ldf, aes(x=S2_G9, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')
 plot2_10 <- ggplot(ldf, aes(x=S2_G10, y=c(0), alpha = 0.3 )) + geom_count() +
   theme(axis.ticks = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
         legend.position = 'none')


 grid.arrange(plot1_2, plot2_2, plot1_3, plot2_3, plot1_4, plot2_4, plot1_5, plot2_5,
              plot1_6, plot2_6, plot1_7, plot2_7, plot1_8, plot2_8, plot1_9, plot2_9, plot1_10, plot2_10,
              nrow = 9, ncol = 2)

####################      CALCULATE PERCENTAGE OF PARTICIPANTS WITH CR < 2 FOR CLUSTER ASSIGNMENT                ####################

# get the relevant files with the probabilities for cluster assignment
#files <- paste0('..\\Clustering results\\', list.files('..\\Clustering results')) # pc 
files <- paste0('../Clustering results/', list.files('../Clustering results')) # mac
ldf <- lapply(files, read.csv); remove(files)

# extract the maximum p and the second maximum p and calculate its ratio
i = 14; limit <- ldf[[i]] %>% ncol - 2
result <- ldf[[i]][2:limit] %>% apply(MARGIN = 1, sort) ; result <- result %>% t
result <- result[, limit-1] / result[, limit-2]; result <- result < 2

# summarize results by groups (count of ratios smaller than 2)
aggregate(result, by = as.data.frame(ldf[[i]]['result.classification']), FUN = sum)
table(ldf[[i]]['result.classification'])

####################      CALCULATE PERCENTAGE OF PARTICIPANTS WITH P < .80 FOR CLUSTER ASSIGNMENT                ####################

# extract the maximum p and the second maximum p and calculate its ratio
i = 18
result <- ldf[[i]][c('result.uncertainty', 'result.classification')] 

# compute the mean posterior classification probability for each profile
aggregate(1-result$result.uncertainty, by = as.data.frame(result$result.classification), FUN = mean)
table(result$result.classification)

# compute the percentage of participants with classification probability lower than .80 in each group
aggregate(result$result.uncertainty <= .2, by = as.data.frame(result$result.classification), FUN = mean)
table(result$result.classification)

 # Compute p < .80 at the Solution level (across profiles)
(ldf[[i]]['result.uncertainty'] > .20) %>% sum/44.59 %>% round(2)

####################      CALCULATE PERCENTAGE P < .80 FOR CLUSTER ASSIGNMENT (OLD DATA FORMAT) ####################
# extract the maximum p and the second maximum p and calculate its ratio
i = 11; limit <- ldf[[i]] %>% ncol - 1
result <- ldf[[i]][2:limit] %>% apply(MARGIN = 1, sort) ; result <- result %>% t

# summarize results by groups (count of ratios smaller than 2)
aggregate(result, by = as.data.frame(ldf[[i]]['result.classification']), FUN = mean)
aggregate(result >= .80, by = as.data.frame(ldf[[i]]['result.classification']), FUN = mean)
table(ldf[[i]]['result.classification'])

