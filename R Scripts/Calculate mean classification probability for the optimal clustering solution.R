####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
# Last update - 12.11.2020
# Relevant for Table 2 in the first typology manuscript
remove(list = ls())
library(dplyr); library(tidyr); library(ggplot2); library(gridExtra)
graphics.off()


####################      LOAD THE Z-SCORES FOR THE TWO OPTIMAL SOLUTIONS             ####################
sample1 <- read.table("C:\\Users\\owner\\Desktop\\Study 4\\Mclust outputs\\Clustering results\\z and classificaiton for sample 1 with groups 5", sep = ',', header = TRUE)
sample2 <- read.table("C:\\Users\\owner\\Desktop\\Study 4\\Mclust outputs\\Clustering results\\z and classificaiton for sample 2 with groups 5", sep = ',', header = TRUE) 

result <- ldf[[i]][2:limit] %>% apply(MARGIN = 1, sort) ; result <- result %>% t
result <- result[, limit-1] / result[, limit-2]; result <- result < 2

table(sample1$result.classification)
sample1 %>% aggregate(by = as.data.frame(sample1$result.classification), FUN = mean) %>% round(3)

table(sample2$result.classification)
sample2 %>% aggregate(by = as.data.frame(sample2$result.classification), FUN = mean) %>% round(2)


