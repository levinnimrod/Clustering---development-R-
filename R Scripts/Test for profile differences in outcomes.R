####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
# Update date: 16-11-2020; 13-11-2020
remove(list = ls())
library(dplyr); library(ggplot2); library(lsr); 

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

####################      EVALUATE THE MEAN DIFFERENCES IN AGE               ####################
table(data$Gender, data$profile)
chisq.test(data$Gender, data$profile)

####################      EVALUATE THE MEAN DIFFERENCES IN AGE               ####################
# test if there is a main effect for sample or for profile
model <- aov(data$Age ~ data$sample * data$profile)
summary(model)
etaSquared(model, type = 2, anova = TRUE) %>% round(2)
pairwise.t.test(data$Age, data$profile, p.adj = "bonf")
x <- TukeyHSD(model); x$`data$profile` %>% round(2)

aggregate(data$Age, 
          by = as.data.frame(data$profile), FUN = mean)

aggregate(data$Age, 
          by = as.data.frame(data$profile), FUN = sd)

table(data$profile)

####################      EVALUATE THE MEAN DIFFERENCES IN DIFFICULTY AND DISTRESS               ####################
# test if there is a main effect for sample or for profile
model <- aov(((data$Difficulty + data$Stress)/2) ~ data$sample * data$profile)
summary(model)
etaSquared(model, type = 2, anova = TRUE) %>% round(2)
pairwise.t.test(((data$Difficulty + data$Stress)/2), data$profile, p.adj = "bonf")
x <- TukeyHSD(model); x$`data$profile` %>% round(2)


aggregate(((data$Difficulty + data$Stress)/2), 
                 by = as.data.frame(data$profile), FUN = mean)

aggregate(((data$Difficulty + data$Stress)/2), 
          by = as.data.frame(data$profile), FUN = sd)


res.aov3 <- aov(x ~  + S1_G5 + Var1 + S1_G5:Var1, data = x)
summary(res.aov3)

ggplot(data, aes(x=RCA, color=profile)) +
  geom_histogram(alpha=0.5, position="identity")

ggplot(data, aes(x=RCA, color=profile)) + facet_grid(profile ~ .) + 
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")



####################      EVALUATE THE MEAN DIFFERENCES IN RCA               ####################
# test if there is a main effect for sample or for profile
model <- aov(data$RCA ~ data$sample * data$profile)
summary(model)
etaSquared(model, type = 2, anova = TRUE) %>% round(2)
pairwise.t.test(data$RCA, data$profile, p.adj = "bonf")
x <- TukeyHSD(model); x$`data$profile` %>% round(2)

aggregate(data$RCA, 
          by = as.data.frame(data$profile), FUN = mean)

aggregate(data$RCA, 
          by = as.data.frame(data$profile), FUN = sd)

res.aov3 <- aov(x ~  + S1_G5 + Var1 + S1_G5:Var1, data = x)
summary(res.aov3)

ggplot(data, aes(x=RCA, color=profile)) +
  geom_histogram(alpha=0.5, position="identity")

ggplot(data, aes(x=RCA, color=profile)) + facet_grid(profile ~ .) + 
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")


