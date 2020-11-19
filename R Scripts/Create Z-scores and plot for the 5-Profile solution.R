####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
# last update - 16-11-2020; 12.11.2020
remove(list = ls())
library(dplyr); library(ggplot2); library(lsr); library(reshape2); library(ggpubr)

####################      LOAD THE CLUSTERING SOLUTIONS                ####################
setwd("C:\\Users\\owner\\Desktop\\Study 4\\Mclust outputs\\Clustering results")
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

####################      ADJUST THE PROFILES FOR THE CORRECT ORDER                ####################
classifications$S1_G5 %>% table
map = setNames(c("D", "A", "B", "C", "E"), c(1, 2, 3, 4, 5))
classifications$S1_G5 <- map[unlist(classifications$S1_G5)]

classifications$S2_G5 %>% table
map = setNames(c("D", "C", "E", "B", "A"), c(1, 2, 3, 4, 5))
classifications$S2_G5 <- map[unlist(classifications$S2_G5)]


####################      COMBINE DATA INTO ONE DF AND Z-SCORE THE 10 INDICATORS                ####################

sample1 <- cbind(sample1, classifications$S1_G5)
sample2 <- cbind(sample2, classifications$S2_G5); colnames(sample2) <- colnames(sample1)

df <- rbind(sample1, sample2); remove(sample1, sample2)

####################      CREATE CLUSTER SCORES + STANDARDIZE SCALES                ####################

df$LR <- ((df$Rd + df$Ri + df$Rm) / 3) %>% round(2)
df$LI <- ((df$La + df$Lo + df$Lp + df$Ls) / 4) %>% round(2)
df$II <- ((df$Ie + df$Iu + df$Ii + df$Ls) / 3) %>% round(2)
df$total <- ((df$LR + df$II + df$LI) / 3) %>% round(2)
temp <- df

df[c('Rm', 'Ri', 'Rd', 'Lp', 'Ls', 'Lo', 'La', 'Iu', 'Ii', 'Ie', 'LR', 'LI', 'II', 'total')] <- 
  scale(df[c('Rm', 'Ri', 'Rd', 'Lp', 'Ls', 'Lo', 'La', 'Iu', 'Ii', 'Ie', 'LR', 'LI', 'II', 'total')]) %>% round(2)

temp
df

####################      PLOT GRAPH                ####################

dat <- aggregate(df[c('Rm', 'Ri', 'Rd', 'Lp', 'Ls', 'Lo', 'La', 'Iu', 'Ii', 'Ie', 
                      'LR', 'LI', 'II', 'total')], 
          by = as.data.frame(df$`classifications$S1_G5`), FUN = mean)

dat <- melt(dat)
colnames(dat) <- c('moder', 'variable', 'value')


map = setNames(c("DB-Specific", "Rm-Specific", "Ri-Specific", "Information-Related", "Ie-Dominant"), c('A', 'B', 'C', 'D', 'E'))
dat$moder <- map[unlist(dat$moder)]

# create a plot for LR only
LR <- dat[dat$variable == 'LR',]
g <- ggplot(data = LR, aes(x=moder, y = value))
g1 <- g + geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey()

# create a plot for LI only
LI <- dat[dat$variable == 'LI',]
g <- ggplot(data = LI, aes(x=moder, y = value))
g2 <- g + geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey()

# create a plot for II only
II <- dat[dat$variable == 'II',]
g <- ggplot(data = II, aes(x=moder, y = value))
g3 <- g + geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey()

# create a plot for II only
total <- dat[dat$variable == 'total',]
g <- ggplot(data = total, aes(x=moder, y = value))
g4 <- g + geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey()

ggarrange(g1, g2, g3, g4, 
          labels = c("LR", "LI", "II", "total"),
          ncol = 2, nrow = 2)


# create a plot for motivation only
rm <- dat[dat$variable == 'Rm',]
g <- ggplot(data = rm, aes(x=moder, y = value))
g +   geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey()

# create a plot for indecisiveness beliefs only
Ri <- dat[dat$variable == 'Ri',]
g <- ggplot(data = Ri, aes(x=moder, y = value))
g +   geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey()

# create a plot for dysfunctional beliefs only
Rd <- dat[dat$variable == 'Rd',]
g <- ggplot(data = Rd, aes(x=moder, y = value))
g +   geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey()


g <- ggplot(data = dat, aes(x=variable, y = value, fill = moder))
g +   geom_bar(stat="identity", position=position_dodge())

g <- ggplot(data = dat, aes(x=moder, y = value, fill = variable))
g +   geom_bar(stat="identity", position=position_dodge())
   
ggplot(dat) +
  geom_col_pa
  


  
  