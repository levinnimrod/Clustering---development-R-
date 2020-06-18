####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
remove(list = ls())


library(dplyr)

####################      LOAD TIDY FILES AND SUBSET FOR THE RELEVANT SAMPLE                ####################
cddq <- file.choose() %>% read.csv

# get only participants with RCA based in the USA
cddq <- cddq[!is.na(cddq$RCA) & cddq$Country == "USA", ]

# remove participants with no difference in responses
url <- file.choose(); source(url)
cddq <- cddq[!exclude(cddq[, c(49:51, 53:56, 58:60)]) == 0, ]

####################      CREATE TWO RANDOM SAMPLES                ####################
sample1 <- cddq[seq(1, nrow(cddq), by = 2), ]; sample1 <- sample1[, -1]; 
sample2 <- cddq[seq(2, nrow(cddq), by = 2), ]; sample2 <- sample2[, -1]; 



