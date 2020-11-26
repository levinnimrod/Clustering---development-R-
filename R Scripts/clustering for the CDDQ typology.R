####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
# LAST DATE - 09/11/2020
remove(list = ls())
library(dplyr); library(mclust); library(psych)

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

####################      RELIABILITY ANALYSES               ####################
cddq[c('Rm1', 'Rm2', 'Rm3')] %>% alpha()
cddq[c('Ri1', 'Ri2', 'Ri3')] %>% alpha()
cddq[c('Rd1', 'Rd2', 'Rd3', 'Rd4')] %>% alpha()

cddq[c('Lp1', 'Lp2', 'Lp3')] %>% alpha()
cddq[c('Ls1', 'Ls2', 'Ls3', 'Ls4')] %>% alpha()
cddq[c('Lo1', 'Lo2', 'Lo3')] %>% alpha()
cddq[c('La1', 'La2')] %>% alpha()

cddq[c('Iu1', 'Iu2', 'Iu3')] %>% alpha()
cddq[c('Ii1', 'Ii2', 'Ii3', 'Ii4', 'Ii5')] %>% alpha()
cddq[c('Ie1', 'Ie2')] %>% alpha()


remove(cddq)

####################      ESTIMATE RELIABILITIES                ####################
sample1[c('Rm1', 'Rm2', 'Rm3')] %>% alpha()
sample2[c('Rm1', 'Rm2', 'Rm3')] %>% alpha()

sample1[c('Ri1', 'Ri2', 'Ri3')] %>% alpha()
sample2[c('Ri1', 'Ri2', 'Ri3')] %>% alpha()

sample1[c('Rd1', 'Rd2', 'Rd3', 'Rd4')] %>% alpha()
sample2[c('Rd1', 'Rd2', 'Rd3', 'Rd4')] %>% alpha()

sample1[c('Lp1', 'Lp2', 'Lp3')] %>% alpha()
sample2[c('Lp1', 'Lp2', 'Lp3')] %>% alpha()

sample1[c('Ls1', 'Ls2', 'Ls3', 'Ls4')] %>% alpha()
sample2[c('Ls1', 'Ls2', 'Ls3', 'Ls4')] %>% alpha()

sample1[c('Lo1', 'Lo2', 'Lo3')] %>% alpha()
sample2[c('Lo1', 'Lo2', 'Lo3')] %>% alpha()

sample1[c('La1', 'La2')] %>% alpha()
sample2[c('La1', 'La2')] %>% alpha()

sample1[c('Iu1', 'Iu2', 'Iu3')] %>% alpha()
sample2[c('Iu1', 'Iu2', 'Iu3')] %>% alpha()

sample1[c('Ii1', 'Ii2', 'Ii3', 'Ii4', 'Ii5')] %>% alpha()
sample2[c('Ii1', 'Ii2', 'Ii3', 'Ii4', 'Ii5')] %>% alpha()

sample1[c('Ie1', 'Ie2')] %>% alpha()
sample2[c('Ie1', 'Ie2')] %>% alpha()


# get only the variables you need
sample1 <- (sample1[, c(4:5, 8:11, 48:50, 52:55, 57:59, 61)]) %>% round(2)
sample2 <- (sample2[, c(4:5, 8:11, 48:50, 52:55, 57:59, 61)]) %>% round(2)

####################      RUN MIXTURE MODEL CLUSTERING                ####################
# create ipsative scores for each participant
ipsative1 <- sample1[, 7:16] %>% t %>% scale %>% t %>% round(2) %>% as.data.frame()
ipsative2 <- sample2[, 7:16] %>% t %>% scale %>% t %>% round(2) %>% as.data.frame()

# get best estimate of loglikelihood from previous iteration
url <- file.choose(); source(url); remove(url)


for (s in seq(726 , 750)) { # number of random seeds
for (i in seq(2, 10)) { # number of groups
typology(data = ipsative1, n_groups = i, sample = 1, seed = s)
typology(data = ipsative2, n_groups = i, sample = 2, seed = s)
}
}
