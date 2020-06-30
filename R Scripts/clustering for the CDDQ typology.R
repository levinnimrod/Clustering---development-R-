####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
remove(list = ls())
library(dplyr); library(mclust)

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

####################      RUN MIXTURE MODEL CLUSTERING                ####################
# create ipsative scores for each participant
ipsative1 <- sample1[, 7:16] %>% t %>% scale %>% t %>% round(2) %>% as.data.frame()
ipsative2 <- sample2[, 7:16] %>% t %>% scale %>% t %>% round(2) %>% as.data.frame()

# get best estimate of loglikelihood from previous iteration
url <- file.choose(); source(url); remove(url)


for (s in seq(144, 150)) { # number of random seeds
for (i in seq(2, 10)) { # number of groups
typology(data = ipsative1, n_groups = i, sample = 1, seed = s)
typology(data = ipsative2, n_groups = i, sample = 2, seed = s)
}
}
