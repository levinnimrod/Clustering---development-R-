####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
# Update date: 20-11-2020; 16-11-2020; 13-11-2020
remove(list = ls())
library(dplyr); library(ggplot2); library(lsr); library(nnet)

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


####################      CREATE AN AGGREGATED FILE WITH BOTH SAMPLES AND CLASSIFICATIONS               ####################
sample1$sample <- "A"; sample2$sample <- "B"
data <- rbind(sample1, sample2)
data$profile <- c(classifications$S1_G5, classifications$S2_G5)  

data$profile <- as.factor(data$profile)
data$Gender <- as.factor(data$Gender)

####################      MULTINOMIAL LOGISTIC REGRESSION               ####################
temp <- data
data <- data[data$sample == 'A', ]
data$profile <- relevel(data$profile, ref = 'A')
test <- multinom(profile ~ Gender  + Age, data = data)
summary(test)  # this gives us the coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
z %>% round(2) 
p <- (1 - pnorm(abs(z), 0, 1)) * 2 # this gives us the p values
p %>% round(3)

## extract the coefficients from the model and exponentiate
exp(coef(test)) %>% round(2)

data$profile <- relevel(data$profile, ref = 'B')
test <- multinom(profile ~ Gender  + Age, data = data)
summary(test)  # this gives us the coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
z %>% round(2) 
p <- (1 - pnorm(abs(z), 0, 1)) * 2 # this gives us the p values
p %>% round(3)

## extract the coefficients from the model and exponentiate
exp(coef(test)) %>% round(2)

data$profile <- relevel(data$profile, ref = 'C')
test <- multinom(profile ~ Gender  + Age, data = data)
summary(test)  # this gives us the coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
z %>% round(2) 
p <- (1 - pnorm(abs(z), 0, 1)) * 2 # this gives us the p values
p %>% round(3)

## extract the coefficients from the model and exponentiate
exp(coef(test)) %>% round(2)

data$profile <- relevel(data$profile, ref = 'D')
test <- multinom(profile ~ Gender  + Age, data = data)
summary(test)  # this gives us the coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
z %>% round(2) 
p <- (1 - pnorm(abs(z), 0, 1)) * 2 # this gives us the p values
p %>% round(3)

## extract the coefficients from the model and exponentiate
exp(coef(test)) %>% round(2)
data <- temp

#################### SAMPLE B
data <- data[data$sample == 'B', ]

data$profile <- relevel(data$profile, ref = 'A')
test <- multinom(profile ~ Gender  + Age, data = data)
summary(test)  # this gives us the coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
z %>% round(2) 
p <- (1 - pnorm(abs(z), 0, 1)) * 2 # this gives us the p values
p %>% round(3)

## extract the coefficients from the model and exponentiate
exp(coef(test)) %>% round(2)

data$profile <- relevel(data$profile, ref = 'B')
test <- multinom(profile ~ Gender  + Age, data = data)
summary(test)  # this gives us the coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
z %>% round(2) 
p <- (1 - pnorm(abs(z), 0, 1)) * 2 # this gives us the p values
p %>% round(3)

## extract the coefficients from the model and exponentiate
exp(coef(test)) %>% round(2)

data$profile <- relevel(data$profile, ref = 'C')
test <- multinom(profile ~ Gender  + Age, data = data)
summary(test)  # this gives us the coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
z %>% round(2) 
p <- (1 - pnorm(abs(z), 0, 1)) * 2 # this gives us the p values
p %>% round(3)

## extract the coefficients from the model and exponentiate
exp(coef(test)) %>% round(2)

data$profile <- relevel(data$profile, ref = 'D')
test <- multinom(profile ~ Gender  + Age, data = data)
summary(test)  # this gives us the coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors
z %>% round(2) 
p <- (1 - pnorm(abs(z), 0, 1)) * 2 # this gives us the p values
p %>% round(3)

## extract the coefficients from the model and exponentiate
exp(coef(test)) %>% round(2)
data <- temp

