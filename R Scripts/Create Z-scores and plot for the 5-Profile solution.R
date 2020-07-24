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

####################      EXCTRACT CLASSIFICATIONS FROM THE SOLUTIONS                ####################
classifications = data.frame(rep(NA, nrow(ldf[[1]])))
uncertainty = data.frame(rep(NA, nrow(ldf[[1]])))

for (i in seq(18)){ 
  classifications[i] = ldf[[i]]['result.classification']
  #  uncertainty[i] = ldf[[i]]['result.uncertainty']
  
}

classifications <- cbind(classifications[,2:9], classifications[,1], classifications[,11:18], classifications[,10])
colnames(classifications) <- paste0("S", c(rep(1,9), rep(2, 9)), "_G", seq(2,10))

####################      STANDARDIZE THE SOLUTIONS ACROSS SAMPLES                ####################
classifications$S1_G5 %>% table
map = setNames(c("B", "C", "A", "E", "D"), c(1, 2, 3, 4, 5))
classifications$S1_G5 <- map[unlist(classifications$S1_G5)]

classifications$S2_G5 %>% table
map = setNames(c("A", "C", "D", "E", "B"), c(1, 2, 3, 4, 5))
classifications$S2_G5 <- map[unlist(classifications$S2_G5)]


####################      COMBINE DATA INTO ONE DF AND Z-SCORE THE 10 INDICATORS                ####################

sample1 <- cbind(sample1, classifications$S1_G5)
sample2 <- cbind(sample2, classifications$S2_G5); colnames(sample2) <- colnames(sample1)

df <- rbind(sample1, sample2); remove(sample1, sample2)

df[c('Rm', 'Ri', 'Rd', 'Lp', 'Ls', 'Lo', 'La', 'Iu', 'Ii', 'Ie')] <- scale(df[c('Rm', 'Ri', 'Rd', 'Lp', 'Ls', 'Lo', 'La', 'Iu', 'Ii', 'Ie')]) 

####################      PLOT GRAPH                ####################

dat <- aggregate(df[c('Rm', 'Ri', 'Rd', 'Lp', 'Ls', 'Lo', 'La', 'Iu', 'Ii', 'Ie')], 
          by = as.data.frame(df$`classifications$S1_G5`), FUN = mean)

library(reshape2)
dat <- melt(dat)
colnames(dat) <- c('moder', 'variable', 'value')

library(ggplot2)
g <- ggplot(data = dat, aes(x=variable, y = value, fill = moder))
g +   geom_bar(stat="identity", position=position_dodge(), width=0.5)


  