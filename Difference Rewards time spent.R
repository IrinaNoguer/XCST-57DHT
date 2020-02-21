###### Row data: rewards time spent normaliyed by time spent in the arm, A-B (higher reward - lower r) #######


#### PRE SURGERY ####

library(openxlsx)
Arm2vs5 <- read.xlsx("D:/DOCUMENTS/R_programming/XCST-57DHT/Arms/2vs5AandR.xlsx")
Arm2vs7 <- read.xlsx("D:/DOCUMENTS/R_programming/XCST-57DHT/Arms/2vs7AandR.xlsx")
Arm5vs7 <- read.xlsx("D:/DOCUMENTS/R_programming/XCST-57DHT/Arms/5vs7AandR.xlsx")
ArmJvs2 <- read.xlsx("D:/DOCUMENTS/R_programming/XCST-57DHT/Arms/Jvs2AandR.xlsx")
ArmJvs5 <- read.xlsx("D:/DOCUMENTS/R_programming/XCST-57DHT/Arms/Jvs5AandR.xlsx")
ArmJvs7 <- read.xlsx("D:/DOCUMENTS/R_programming/XCST-57DHT/Arms/Jvs7AandR.xlsx")

library(tidyverse)
library(ggplot2)

#### PLotting positive and negative values daily #####

Arm2vs5D1 <- filter(Arm2vs5, Date == "D1")

library(RColorBrewer)
coul <- brewer.pal(12, "PRGn") 

barplot(Arm2vs5D1$`RA3-RA2`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA3-RA2",
        cex.names=0.7,
        names.arg = Arm2vs5D1$Animal,
        main = "2vs5 Day 1 Pre",
        bg = 'white',
        col = coul)

Arm2vs5D4 <- filter(Arm2vs5, Date == "D4")

barplot(Arm2vs5D4$`RA3-RA2`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA3-RA2",
        cex.names=0.7,
        names.arg = Arm2vs5D4$Animal,
        main = "2vs5 Day 4 Pre",
        col = coul)

Arm2vs5D7 <- filter(Arm2vs5, Date == "D7")

barplot(Arm2vs5D7$`RA3-RA2`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA3-RA2",
        cex.names=0.7,
        names.arg = Arm2vs5D7$Animal,
        main = "2vs5 Day 7 Pre",
        col = coul)

### 2vs7 Pre ###

Arm2vs7D2 <- filter(Arm2vs7, Date == "D2")

barplot(Arm2vs7D2$`RA4-RA2`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA4-RA2",
        cex.names=0.7,
        names.arg = Arm2vs7D2$Animal,
        main = "2vs7 Day 2 Pre",
        col = coul)

Arm2vs7D5 <- filter(Arm2vs7, Date == "D5")

barplot(Arm2vs7D5$`RA4-RA2`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA4-RA2",
        cex.names=0.7,
        names.arg = Arm2vs7D5$Animal,
        main = "2vs7 Day 5 Pre",
        col = coul)

Arm2vs7D8 <- filter(Arm2vs7, Date == "D8")

barplot(Arm2vs7D8$`RA4-RA2`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA4-RA2",
        cex.names=0.7,
        names.arg = Arm2vs7D8$Animal,
        main = "2vs7 Day 8 Pre",
        col = coul)

### 5vs7 Pre ###

Arm5vs7D3 <- filter(Arm5vs7, Date == "D3")

barplot(Arm5vs7D3$`RA4-RA3`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA4-RA3",
        cex.names=0.7,
        names.arg = Arm5vs7D3$Animal,
        main = "5vs7 Day 3 Pre",
        col = coul)

Arm5vs7D6 <- filter(Arm5vs7, Date == "D6")

barplot(Arm5vs7D6$`RA4-RA3`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA4-RA3",
        cex.names=0.7,
        names.arg = Arm5vs7D6$Animal,
        main = "5vs7 Day 6 Pre",
        col = coul)

Arm5vs7D9 <- filter(Arm5vs7, Date == "D9")

barplot(Arm5vs7D9$`RA4-RA3`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA4-RA3",
        cex.names=0.7,
        names.arg = Arm5vs7D9$Animal,
        main = "5vs7 Day 9 Pre",
        col = coul)

### 2vsJ Pre ###

ArmJvs2D1 <- filter(ArmJvs2, Date == "D1")

barplot(ArmJvs2D1$`RA1-RA2`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA1-RA2",
        cex.names=0.7,
        names.arg = ArmJvs2D1$Animal,
        main = "Jvs2 D1 Pre",
        col = coul)

ArmJvs2D4 <- filter(ArmJvs2, Date == "D4")

barplot(ArmJvs2D4$`RA1-RA2`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA1-RA2",
        cex.names=0.7,
        names.arg = ArmJvs2D4$Animal,
        main = "Jvs2 D4 Pre",
        col = coul)

### 5vsJ Pre ###

ArmJvs5D2 <- filter(ArmJvs5, Date == "D2")

barplot(ArmJvs5D2$`RA3-RA1`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA3-RA1",
        cex.names=0.7,
        names.arg = ArmJvs5D2$Animal,
        main = "Jvs5 D2 Pre",
        col = coul)

barplot(ArmJvs5D2$`RA1-RA3`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA1-RA3",
        cex.names=0.7,
        names.arg = ArmJvs5D2$Animal,
        main = "Jvs5 D2 Pre",
        col = coul)

ArmJvs5D5 <- filter(ArmJvs5, Date == "D5")

barplot(ArmJvs5D5$`RA3-RA1`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA3-RA1",
        cex.names=0.7,
        names.arg = ArmJvs5D5$Animal,
        main = "Jvs5 D5 Pre",
        col = coul)

barplot(ArmJvs5D5$`RA1-RA3`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA1-RA3",
        cex.names=0.7,
        names.arg = ArmJvs5D5$Animal,
        main = "Jvs5 D5 Pre",
        col = coul)

### 7vsJ Pre ###

ArmJvs7D3 <- filter(ArmJvs7, Date == "D3")

barplot(ArmJvs7D3$`RA4-RA1`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA4-RA1",
        cex.names=0.7,
        names.arg = ArmJvs7D3$Animal,
        main = "Jvs7 D3 Pre",
        col = coul)

ArmJvs7D6 <- filter(ArmJvs7, Date == "D6")

barplot(ArmJvs7D6$`RA4-RA1`, 
        horiz=TRUE, 
        xlim=c(-1,1), 
        xlab="RA4-RA1",
        cex.names=0.7,
        names.arg = ArmJvs7D6$Animal,
        main = "Jvs7 D6 Pre",
        col = coul)





### One-way repeated measures ANOVA to check the influence of day on RewardArm-RewardArm ###
### (Reward value normalized by time spent in the arm) ###

library(tidyverse)
library(ggpubr)
library(rstatix)

# Renaming #

colnames(Arm2vs5)[13] <- "Rdif"

# Summary statistics #

Arm2vs5 %>%
  group_by(Date) %>%
  get_summary_stats("Rdif", type = "mean_sd")

# plot #

ggboxplot(Arm2vs5, x = "Date", y = "Rdif", add = "point")

# Checking for outliers #

Arm2vs5 %>%
  group_by(Date) %>%
  identify_outliers(Rdif)

# Normality assumption #

Arm2vs5 %>%
  group_by(Date) %>%
  shapiro_test(Rdif)

ggqqplot(Arm2vs5, "Rdif", facet.by = "Date")

# Computation #                       ### IS THIS THE BETTER WAY? ###

library(ICSNP)
library(emmeans)

Arm2vs5$Date <- as.factor(Arm2vs5$Date)
Arm2vs5$Animal <- as.factor(Arm2vs5$Animal)

summary(aov(Rdif ~ Date + Error(Animal/Date), data=Arm2vs5))             
OneRMAnova2vs5 <- aov(Rdif ~ Date + Error(Animal/Date), data=Arm2vs5)
emmORMA2vs5 <- emmeans(OneRMAnova2vs5, ~ Date)
pairs(emmORMA2vs5)

### 2vs7 ###

colnames(Arm2vs7)[13] <- "Rdif"

Arm2vs7 %>%
  group_by(Date) %>%
  get_summary_stats("Rdif", type = "mean_sd")

ggboxplot(Arm2vs7, x = "Date", y = "Rdif", add = "point")

Arm2vs7 %>%
  group_by(Date) %>%
  identify_outliers(Rdif)

Arm2vs7 %>%
  group_by(Date) %>%
  shapiro_test(Rdif)

ggqqplot(Arm2vs7, "Rdif", facet.by = "Date")


Arm2vs7$Date <- as.factor(Arm2vs7$Date)
Arm2vs7$Animal <- as.factor(Arm2vs7$Animal)

# Extreme outlier, removing it #

Arm2vs7NoOutlier <- Arm2vs7[-28,] 

summary(aov(Rdif ~ Date + Error(Animal/Date), data = Arm2vs7NoOutlier))
OneRMAnova2vs7 <- aov(Rdif ~ Date + Error(Animal/Date), data=Arm2vs7NoOutlier)
emmORMA2vs7 <- emmeans(OneRMAnova2vs7, ~ Date)
pairs(emmORMA2vs7)

# D2 not normally distributed #

Arm2vs7NoOutlier$Date <- factor(Arm2vs7NoOutlier$Date,
                                levels = c("D2", "D5", "D8"),
                                labels = c("2", "5", "8"))
Arm2vs7NoOutlier$Date <- as.numeric(Arm2vs7NoOutlier$Date)

Arm2vs7NoOutlier$Animal <- factor(Arm2vs7NoOutlier$Animal,
                                  levels = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12"),
                                  labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
Arm2vs7NoOutlier$Animal <- as.numeric(Arm2vs7NoOutlier$Animal)


anovaRMNP(Arm2vs7NoOutlier, measures = vars(Rdif, Date, Animal), desc = TRUE, pairs = TRUE) # DOn't know any post hoc #


# With outlier

summary(aov(Rdif ~ Date + Error(Animal/Date), data = Arm2vs7))
OneRMAnova2vs7 <- aov(Rdif ~ Date + Error(Animal/Date), data=Arm2vs7)
emmORMA2vs7 <- emmeans(OneRMAnova2vs7, ~ Date)
pairs(emmORMA2vs7)


### 5vs7 ###

colnames(Arm5vs7)[13] <- "Rdif"

Arm5vs7 %>%
  group_by(Date) %>%
  get_summary_stats("Rdif", type = "mean_sd")

ggboxplot(Arm5vs7, x = "Date", y = "Rdif", add = "point")

Arm5vs7 %>%
  group_by(Date) %>%
  identify_outliers(Rdif)

Arm5vs7 %>%
  group_by(Date) %>%
  shapiro_test(Rdif)

ggqqplot(Arm5vs7, "Rdif", facet.by = "Date")

Arm5vs7$Date <- as.factor(Arm5vs7$Date)
Arm5vs7$Animal <- as.factor(Arm5vs7$Animal)

summary(aov(Rdif ~ Date + Error(Animal/Date), data=Arm5vs7))             
OneRMAnova5vs7 <- aov(Rdif ~ Date + Error(Animal/Date), data=Arm5vs7)
emmORMA5vs7 <- emmeans(OneRMAnova5vs7, ~ Date)
pairs(emmORMA5vs7)

# Becuase I got this: Error() model is singular #

# Linear mixed model #

library(lme4)

lmm5vs7 <- lmer(Rdif ~ Date + (1 | Animal) + (1 | Date), data = Arm5vs7)
summary(lmm5vs7)
Anova(lmm5vs7)

# because Date is a fix effect shoulnd't be a random one too, right? #

lmm5vs7 <- lmer(Rdif ~ Date + (1 | Animal), data = Arm5vs7) 
summary(lmm5vs7)
Anova(lmm5vs7)

# because the residual value was so low #

lm5vs7 <- lm(Rdif ~ Date, data = Arm5vs7)  
print(lm5vs7)
summary(lm5vs7)


### Jvs2 ###

colnames(ArmJvs2)[12] <- "Rdif"

ArmJvs2 %>%
  group_by(Date) %>%
  get_summary_stats("Rdif", type = "mean_sd")

ggboxplot(ArmJvs2, x = "Date", y = "Rdif", add = "point")

ArmJvs2 %>%
  group_by(Date) %>%
  identify_outliers(Rdif)

ArmJvs2 %>%
  group_by(Date) %>%
  shapiro_test(Rdif)

ggqqplot(ArmJvs2, "Rdif", facet.by = "Date")

ArmJvs2$Date <- as.factor(ArmJvs2$Date)
ArmJvs2$Animal <- as.factor(ArmJvs2$Animal)

summary(aov(Rdif ~ Date + Error(Animal/Date), data=ArmJvs2))             
OneRMAnovaJvs2 <- aov(Rdif ~ Date + Error(Animal/Date), data=ArmJvs2)
emmORMAJvs2 <- emmeans(OneRMAnovaJvs2, ~ Date)
pairs(emmORMAJvs2)

# lmm #

lmmJvs2 <- lmer(Rdif ~ Date + (1 | Animal), data = ArmJvs2) 
summary(lmmJvs2)
Anova(lmmJvs2)

# because the residual value was so low #

lmJvs2 <- lm(Rdif ~ Date, data = ArmJvs2)  
print(lmJvs2)
summary(lmJvs2)


### Jvs5 ###

colnames(ArmJvs5)[12] <- "Rdif"

ArmJvs5 %>%
  group_by(Date) %>%
  get_summary_stats("Rdif", type = "mean_sd")

ggboxplot(ArmJvs5, x = "Date", y = "Rdif", add = "point")

ArmJvs5 %>%
  group_by(Date) %>%
  identify_outliers(Rdif)

ArmJvs5 %>%
  group_by(Date) %>%
  shapiro_test(Rdif)

ggqqplot(ArmJvs5, "Rdif", facet.by = "Date")

ArmJvs5$Date <- as.factor(ArmJvs5$Date)
ArmJvs5$Animal <- as.factor(ArmJvs5$Animal)

summary(aov(Rdif ~ Date + Error(Animal/Date), data=ArmJvs5))             
OneRMAnovaJvs5 <- aov(Rdif ~ Date + Error(Animal/Date), data=ArmJvs5)
emmORMAJvs5 <- emmeans(OneRMAnovaJvs5, ~ Date)
pairs(emmORMAJvs5)


### Jvs7 ###

colnames(ArmJvs7)[12] <- "Rdif"

ArmJvs7 %>%
  group_by(Date) %>%
  get_summary_stats("Rdif", type = "mean_sd")

ggboxplot(ArmJvs7, x = "Date", y = "Rdif", add = "point")

ArmJvs7 %>%
  group_by(Date) %>%
  identify_outliers(Rdif)

ArmJvs7 %>%
  group_by(Date) %>%
  shapiro_test(Rdif)

ggqqplot(ArmJvs7, "Rdif", facet.by = "Date")

ArmJvs7$Date <- as.factor(ArmJvs7$Date)
ArmJvs7$Animal <- as.factor(ArmJvs7$Animal)

summary(aov(Rdif ~ Date + Error(Animal/Date), data=ArmJvs7))             
OneRMAnovaJvs7 <- aov(Rdif ~ Date + Error(Animal/Date), data=ArmJvs7)
emmORMAJvs7 <- emmeans(OneRMAnovaJvs7, ~ Date)
pairs(emmORMAJvs7)



### continue checking the effect of the strating group ### 
### Here I am ot considering that it is repeated measures, right? not sure. ###

# 2vs5 #

Start2vs5 <- aov(formula = Rdif ~ Date + StartingG, data = Arm2vs5)
summary(Start2vs5)
TukeyHSD(Start2vs5)

Start2vs5.lm <- lm(formula = Rdif ~ Date + StartingG, data = Arm2vs5)
summary(Start2vs5.lm)

# 2vs7 #

Arm2vs7NoOutlier$Date <- as.factor(Arm2vs7NoOutlier$Date)

Start2vs7 <- aov(formula = Rdif ~ Date + StartingG, data = Arm2vs7NoOutlier)
summary(Start2vs7)
TukeyHSD(Start2vs7)

Start2vs7.lm <- lm(formula = Rdif ~ Date + StartingG, data = Arm2vs7NoOutlier)
summary(Start2vs7.lm)

# 5vs7 #

Start5vs7 <- aov(formula = Rdif ~ Date + StartingG, data = Arm5vs7)
summary(Start5vs7)
TukeyHSD(Start5vs7)

Start5vs7.lm <- lm(formula = Rdif ~ Date + StartingG, data = Arm5vs7)
summary(Start5vs7.lm)

# Jvs2 #

StartJvs2 <- aov(formula = Rdif ~ Date + StartingG, data = ArmJvs2)
summary(StartJvs2)
TukeyHSD(StartJvs2)

StartJvs2.lm <- lm(formula = Rdif ~ Date + StartingG, data = ArmJvs2)
summary(StartJvs2.lm)

# Jvs5 #

StartJvs5 <- aov(formula = Rdif ~ Date + StartingG, data = ArmJvs5)
summary(StartJvs5)
TukeyHSD(StartJvs5)

StartJvs5.lm <- lm(formula = Rdif ~ Date + StartingG, data = ArmJvs5)
summary(StartJvs5.lm)

# Jvs7 #

StartJvs7 <- aov(formula = Rdif ~ Date + StartingG, data = ArmJvs7)
summary(StartJvs7)
TukeyHSD(StartJvs7)

StartJvs7.lm <- lm(formula = Rdif ~ Date + StartingG, data = ArmJvs7)
summary(StartJvs7.lm)



### If starting Group makes no differences, Does Arm Position make it? ###

# 2vs5 #

Start2vs5 <- aov(formula = Rdif ~ Date + Group, data = Arm2vs5)
summary(Start2vs5)
TukeyHSD(Start2vs5)

Start2vs5.lm <- lm(formula = Rdif ~ Date + Group, data = Arm2vs5)
summary(Start2vs5.lm)

# 2vs7 #

Start2vs7 <- aov(formula = Rdif ~ Date + Group, data = Arm2vs7NoOutlier)
summary(Start2vs7)
TukeyHSD(Start2vs7)

Start2vs7.lm <- lm(formula = Rdif ~ Date + Group, data = Arm2vs7NoOutlier)
summary(Start2vs7.lm)

# 5vs7 #

Start5vs7 <- aov(formula = Rdif ~ Date + Group, data = Arm5vs7)
summary(Start5vs7)
TukeyHSD(Start5vs7)

Start5vs7.lm <- lm(formula = Rdif ~ Date + Group, data = Arm5vs7)
summary(Start5vs7.lm)

# Jvs2 #

StartJvs2 <- aov(formula = Rdif ~ Date + Group, data = ArmJvs2)
summary(StartJvs2)
TukeyHSD(StartJvs2)

StartJvs2.lm <- lm(formula = Rdif ~ Date + Group, data = ArmJvs2)
summary(StartJvs2.lm)

# Jvs5 #

StartJvs5 <- aov(formula = Rdif ~ Date + Group, data = ArmJvs5)
summary(StartJvs5)
TukeyHSD(StartJvs5)

StartJvs5.lm <- lm(formula = Rdif ~ Date + Group, data = ArmJvs5)
summary(StartJvs5.lm)

# Jvs7 #

StartJvs7 <- aov(formula = Rdif ~ Date + Group, data = ArmJvs7)
summary(StartJvs7)
TukeyHSD(StartJvs7)

StartJvs7.lm <- lm(formula = Rdif ~ Date + Group, data = ArmJvs7)
summary(StartJvs7.lm)


#### 1)	Is the 5% a turning point?
#### 3)	Is there a difference between DHT and Sham? I-G

