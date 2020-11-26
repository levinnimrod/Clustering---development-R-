####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
# last update - 16-11-2020; 12.11.2020
remove(list = ls())
library(dplyr); library(ggplot2); library(lsr); library(reshape2); library(ggpubr); library(gridExtra); library(psych)

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

####################      COMPUTE RELIABILITIES                ####################
res <- as.data.frame(cbind(NA, NA)); colnames(res) <- c("Sample 1", "Sample 2")

res[1, 1] = (alpha(sample1[, c('Rm1', 'Rm2', 'Rm3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)
res[1, 2] = (alpha(sample2[, c('Rm1', 'Rm2', 'Rm3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)

res[2, 1] = (alpha(sample1[, c('Ri1', 'Ri2', 'Ri3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)
res[2, 2] = (alpha(sample2[, c('Ri1', 'Ri2', 'Ri3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)

res[3, 1] = (alpha(sample1[, c('Rd1', 'Rd2', 'Rd3', 'Rd4')], keys = c(1,1,1,1))$total$raw_alpha) %>% round(2)
res[3, 2] = (alpha(sample2[, c('Rd1', 'Rd2', 'Rd3', 'Rd4')], keys = c(1,1,1,1))$total$raw_alpha) %>% round(2)

res[4, 1] = (alpha(sample1[, c('Lp1', 'Lp2', 'Lp3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)
res[4, 2] = (alpha(sample2[, c('Lp1', 'Lp2', 'Lp3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)

res[5, 1] = (alpha(sample1[, c('Ls1', 'Ls2', 'Ls3', 'Ls4')], keys = c(1,1,1,1))$total$raw_alpha) %>% round(2)
res[5, 2] = (alpha(sample2[, c('Ls1', 'Ls2', 'Ls3', 'Ls4')], keys = c(1,1,1,1))$total$raw_alpha) %>% round(2)

res[6, 1] = (alpha(sample1[, c('Lo1', 'Lo2', 'Lo3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)
res[6, 2] = (alpha(sample2[, c('Lo1', 'Lo2', 'Lo3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)

res[7, 1] = (alpha(sample1[, c('La1', 'La2')], keys = c(1,1))$total$raw_alpha) %>% round(2)
res[7, 2] = (alpha(sample2[, c('La1', 'La2')], keys = c(1,1))$total$raw_alpha) %>% round(2)

res[8, 1] = (alpha(sample1[, c('Iu1', 'Iu2', 'Iu3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)
res[8, 2] = (alpha(sample2[, c('Iu1', 'Iu2', 'Iu3')], keys = c(1,1,1))$total$raw_alpha) %>% round(2)

res[9, 1] = (alpha(sample1[, c('Ii1', 'Ii2', 'Ii3', 'Ii4', 'Ii5')], keys = c(1,1,1,1,1))$total$raw_alpha) %>% round(2)
res[9, 2] = (alpha(sample2[, c('Iu1', 'Iu2', 'Iu3', 'Ii4', 'Ii5')], keys = c(1,1,1,1,1))$total$raw_alpha) %>% round(2)

res[10, 1] = (alpha(sample1[, c('Ie1', 'Ie2')], keys = c(1,1))$total$raw_alpha) %>% round(2)
res[10, 2] = (alpha(sample2[, c('Ie1', 'Ie2')], keys = c(1,1))$total$raw_alpha) %>% round(2)

describe(res)
quantile(res$`Sample 1`, c(0.25, 0.75))
quantile(res$`Sample 2`, c(0.25, 0.75))
