## Uploading data
library(openxlsx)

df_SucroseDiscPre <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/SucroseDiscriminationPre.xlsx")
df_SucroseDiscPost <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/SucroseDiscriminationPost.xlsx")
df_JAll <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/df_JAll.xlsx")

## Changing the name of the sucrose lebels
library(tidyverse)
df_JAll$Sucrose <- factor(df_JAll$Sucrose, 
                                 levels = c(2, 5, 7),
                                 labels = c("S2", "S5", "S7"))


## Checking normality and comparing both rewards
library(dplyr)
R2_2vs5_Pre <- filter(df_SucroseDiscPre, Condition == "C2vs5", Zone == "R2_pct")
R3_2vs5_Pre <- filter(df_SucroseDiscPre, Condition == "C2vs5", Zone == "R3_pct")
shapiro.test(R2_2vs5_Pre$CumDur)              # is it right considering that we have three days per rat?
wilcox.test(R2_2vs5_Pre$CumDur, R3_2vs5_Pre$CumDur, paired = TRUE)

## Plotting
library(ggplot2)
df_SucroseDiscPre[df_SucroseDiscPre$Condition == "C2vs5",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs5_Pre")+
  scale_fill_manual(values = c("lightskyblue", "darkorange2"))+
  scale_y_continuous(breaks = seq(0,100,25))


### Repeating for the other conditions
R2_2vs7_Pre <- filter(df_SucroseDiscPre, Condition == "C2vs7", Zone == "R2_pct")
R4_2vs7_Pre <- filter(df_SucroseDiscPre, Condition == "C2vs7", Zone == "R4_pct")
shapiro.test(R2_2vs7_Pre$CumDur)
wilcox.test(R2_2vs7_Pre$CumDur, R4_2vs7_Pre$CumDur, paired = TRUE)

df_SucroseDiscPre[df_SucroseDiscPre$Condition == "C2vs7",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs7_Pre")+
  scale_fill_manual(values = c("lightskyblue", "darkolivegreen3"))+
  scale_y_continuous(breaks = seq(0,100,25))

R3_5vs7_Pre <- filter(df_SucroseDiscPre, Condition == "C5vs7", Zone == "R3_pct")
R4_5vs7_Pre <- filter(df_SucroseDiscPre, Condition == "C5vs7", Zone == "R4_pct")
shapiro.test(R3_5vs7_Pre$CumDur)
t.test(R3_5vs7_Pre$CumDur, R4_5vs7_Pre$CumDur, paired = TRUE)

df_SucroseDiscPre[df_SucroseDiscPre$Condition == "C5vs7",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "5vs7_Pre")+
  scale_fill_manual(values = c("darkorange2", "darkolivegreen3"))+
  scale_y_continuous(breaks = seq(0,100,25))


### Plotting day by day

library(dplyr)

df_SucroseDiscPre %>%
  filter(Condition == "C2vs5", Zone %in% c("R3_pct")) %>% 
  ggplot(aes(x = Date, y = CumDur, fill = Date))+
  geom_boxplot(width = 0.5, alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs5_Pre_ByDay")+
  ylab("R5_pct")+
  scale_y_continuous(breaks = seq(0,100,25))

df_SucroseDiscPre %>%
  filter(Condition == "C2vs7", Zone %in% c("R4_pct")) %>% 
  ggplot(aes(x = Date, y = CumDur, fill = Date))+
  geom_boxplot(width = 0.5, alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs7_Pre_ByDay")+
  ylab("R7_pct")+
  scale_y_continuous(breaks = seq(0,100,25))

df_SucroseDiscPre %>%
  filter(Condition == "C5vs7", Zone %in% c("R4_pct")) %>% 
  ggplot(aes(x = Date, y = CumDur, fill = Date))+
  geom_boxplot(width = 0.5, alpha = .4, outlier.alpha = 1)+
  labs(title = "5vs7_Pre_ByDay")+
  ylab("R7_pct")+
  scale_y_continuous(breaks = seq(0,100,25))


### Repeating for Post ###

### 2vs5 post ###
R2_2vs5_Post <- filter(df_SucroseDiscPost, Condition == "C2vs5", Zone == "R2_pct")
R3_2vs5_Post <- filter(df_SucroseDiscPost, Condition == "C2vs5", Zone == "R3_pct")
shapiro.test(R2_2vs5_Post$CumDur)
wilcox.test(R2_2vs5_Post$CumDur, R3_2vs5_Post$CumDur, paired = TRUE)
t.test(R2_2vs5_Post$CumDur, R3_2vs5_Post$CumDur, paired = TRUE)

df_SucroseDiscPost[df_SucroseDiscPost$Condition == "C2vs5",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs5_Post")+
  scale_fill_manual(values = c("lightskyblue", "darkorange2"))+
  scale_y_continuous(breaks = seq(0,100,25))

## Same plot but divided by the two groups
df_SucroseDiscPost[df_SucroseDiscPost$Condition == "C2vs5",] %>% 
  ggplot(aes(Zone, CumDur, fill=Group))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs5_Post")+
  scale_fill_manual(values = c("lightskyblue", "darkorange2"))+
  scale_y_continuous(breaks = seq(0,100,25))

var.test(CumDur ~ Group, data = R3_2vs5_Post) ### testing the variance
t.test(CumDur ~ Group, data = R3_2vs5_Post, var.equal = TRUE)
wilcox.test(CumDur ~ Group, data = R3_2vs5_Post, var.equal = TRUE)


### 2vs7 post ###

R2_2vs7_Post <- filter(df_SucroseDiscPost, Condition == "C2vs7", Zone == "R2_pct")
R4_2vs7_Post <- filter(df_SucroseDiscPost, Condition == "C2vs7", Zone == "R4_pct")
shapiro.test(R2_2vs7_Post$CumDur)
wilcox.test(R2_2vs7_Post$CumDur, R4_2vs7_Post$CumDur, paired = TRUE)
t.test(R2_2vs7_Post$CumDur, R4_2vs7_Post$CumDur, paired = TRUE)

df_SucroseDiscPost[df_SucroseDiscPost$Condition == "C2vs7",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs7_Post")+
  scale_fill_manual(values = c("lightskyblue", "darkolivegreen3"))+
  scale_y_continuous(breaks = seq(0,100,25))

## Same plot but divided by the two groups
df_SucroseDiscPost[df_SucroseDiscPost$Condition == "C2vs7",] %>% 
  ggplot(aes(Zone, CumDur, fill=Group))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs7_Post")+
  scale_fill_manual(values = c("lightskyblue", "darkolivegreen3"))+
  scale_y_continuous(breaks = seq(0,100,25))

var.test(CumDur ~ Group, data = R4_2vs7_Post) ### testing the variance
t.test(CumDur ~ Group, data = R4_2vs7_Post, var.equal = TRUE)
wilcox.test(CumDur ~ Group, data = R4_2vs7_Post, var.equal = TRUE)

### 5vs7 post ###

R3_5vs7_Post <- filter(df_SucroseDiscPost, Condition == "C5vs7", Zone == "R3_pct")
R4_5vs7_Post <- filter(df_SucroseDiscPost, Condition == "C5vs7", Zone == "R4_pct")
shapiro.test(R3_5vs7_Post$CumDur)
wilcox.test(R3_5vs7_Post$CumDur, R4_5vs7_Post$CumDur, paired = TRUE)
t.test(R3_5vs7_Post$CumDur, R4_5vs7_Post$CumDur, paired = TRUE)

df_SucroseDiscPost[df_SucroseDiscPost$Condition == "C5vs7",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "5vs7_Post")+
  scale_fill_manual(values = c("darkorange2", "darkolivegreen3"))+
  scale_y_continuous(breaks = seq(0,100,25))

## Same plot but divided by the two groups
df_SucroseDiscPost[df_SucroseDiscPost$Condition == "C5vs7",] %>% 
  ggplot(aes(Zone, CumDur, fill=Group))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "5vs7_Post")+
  scale_fill_manual(values = c("darkorange2", "darkolivegreen3"))+
  scale_y_continuous(breaks = seq(0,100,25))

var.test(CumDur ~ Group, data = R4_5vs7_Post) ### testing the variance
t.test(CumDur ~ Group, data = R4_5vs7_Post, var.equal = TRUE)
wilcox.test(CumDur ~ Group, data = R4_5vs7_Post, var.equal = TRUE)

####### pre and post sucrose discrimination #######

# Add a column to the df to indicate if it is pre or post
library(lessR)
Time <- c("Pre")
df_SucroseDiscPre$Time <- Time
df_SucroseDiscPre$Time <- factor(df_SucroseDiscPre$Time)

Time <- c("Post")
df_SucroseDiscPost$Time <- Time
df_SucroseDiscPost$Time <- factor(df_SucroseDiscPost$Time)

## Merging ###
df_sucDisc <- Merge(df_SucroseDiscPre, df_SucroseDiscPost)

## plotting it ###

df_sucDisc[df_sucDisc$Condition == "C2vs5",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs5")+
  scale_fill_manual(values = c("lightskyblue", "darkorange2"))+
  scale_y_continuous(breaks = seq(0,100,25))+
  facet_grid(~Time)

df_sucDisc[df_sucDisc$Condition == "C2vs7",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "2vs7")+
  scale_fill_manual(values = c("lightskyblue", "darkolivegreen3"))+
  scale_y_continuous(breaks = seq(0,100,25))+
  facet_grid(~Time)

df_sucDisc[df_sucDisc$Condition == "C5vs7",] %>% 
  ggplot(aes(Zone, CumDur, fill=Zone))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "5vs7")+
  scale_fill_manual(values = c("darkorange2", "darkolivegreen3"))+
  scale_y_continuous(breaks = seq(0,100,25))+
  facet_grid(~Time)



#### Create Line Chart ####

df_LP2vs5_R5 <- filter(df_SucroseDiscPre, Condition == "C2vs5", Zone == "R3_pct")
df_LP2vs5_R5$Animal <- factor(df_LP2vs5_R5$Animal, 
                               levels = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12"),
                               labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
df_LP2vs5_R5$Date <- factor(df_LP2vs5_R5$Date, 
                           levels = c("D1","D4", "D7"),
                           labels = c(1, 2, 3))

# convert factor to numeric for convenience
df_LP2vs5_R5$Animal <- as.numeric(df_LP2vs5_R5$Animal)
df_LP2vs5_R5$Date <- as.numeric(df_LP2vs5_R5$Date)
nanimals <- max(df_LP2vs5_R5$Animal)

# get the range for the x and y axis
xrange <- range(df_LP2vs5_R5$Date)
yrange <- range(df_LP2vs5_R5$CumDur)

# set up the plot
par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE, bg = "grey92") # to add place around the plot to put the legend there and make the background grey
plot(xrange, yrange, type="n", xlab="Days",
     ylab="Cumulative Duration R5")
colors <- rainbow(nanimals)
linetype <- c(1:nanimals)
plotchar <- seq(1,1+nanimals,1) # for the symbol of each animal

# add lines
for (i in 1:nanimals) {
  animal <- subset(df_LP2vs5_R5, Animal==i)
  lines(animal$Date, animal$CumDur, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i])
}

# add a title
title("2vs5 day by day")

# add a legend
legend("topright", inset=c(-0.2,0), legend = c(1:nanimals), cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Animal")


#### FOR CONDITION 2vs7 ####

df_LP2vs7_R7 <- filter(df_SucroseDiscPre, Condition == "C2vs7", Zone == "R4_pct")
df_LP2vs7_R7$Animal <- factor(df_LP2vs7_R7$Animal, 
                              levels = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12"),
                              labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
df_LP2vs7_R7$Date <- factor(df_LP2vs7_R7$Date, 
                            levels = c("D2","D5", "D8"),
                            labels = c(1, 2, 3))

# convert factor to numeric for convenience
df_LP2vs7_R7$Animal <- as.numeric(df_LP2vs7_R7$Animal)
df_LP2vs7_R7$Date <- as.numeric(df_LP2vs7_R7$Date)
nanimals <- max(df_LP2vs7_R7$Animal)

# get the range for the x and y axis
xrange <- range(df_LP2vs7_R7$Date)
yrange <- range(df_LP2vs7_R7$CumDur)

# set up the plot
par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE, bg = "grey92") # to add place around the plot to put the legend there and make the background grey
plot(xrange, yrange, type="n", xlab="Days",
     ylab="Cumulative Duration R7")
colors <- rainbow(nanimals)
linetype <- c(1:nanimals)
plotchar <- seq(1,1+nanimals,1) # for the symbol of each animal

# add lines
for (i in 1:nanimals) {
  animal <- subset(df_LP2vs7_R7, Animal==i)
  lines(animal$Date, animal$CumDur, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i])
}

# add a title
title("2vs7 day by day")

# add a legend
legend("topright", inset=c(-0.2,0), legend = c(1:nanimals), cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Animal")


### condition 5vs7 ####

df_LP5vs7_R7 <- filter(df_SucroseDiscPre, Condition == "C5vs7", Zone == "R4_pct")
df_LP5vs7_R7$Animal <- factor(df_LP5vs7_R7$Animal, 
                              levels = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12"),
                              labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
df_LP5vs7_R7$Date <- factor(df_LP5vs7_R7$Date, 
                            levels = c("D3","D6", "D9"),
                            labels = c(1, 2, 3))

# convert factor to numeric for convenience
df_LP5vs7_R7$Animal <- as.numeric(df_LP5vs7_R7$Animal)
df_LP5vs7_R7$Date <- as.numeric(df_LP5vs7_R7$Date)
nanimals <- max(df_LP5vs7_R7$Animal)

# get the range for the x and y axis
xrange <- range(df_LP5vs7_R7$Date)
yrange <- range(df_LP5vs7_R7$CumDur)

# set up the plot
par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE, bg = "grey92") # to add place around the plot to put the legend there and make the background grey
plot(xrange, yrange, type="n", xlab="Days",
     ylab="Cumulative Duration R7")
colors <- rainbow(nanimals)
linetype <- c(1:nanimals)
plotchar <- seq(1,1+nanimals,1) # for the symbol of each animal

# add lines
for (i in 1:nanimals) {
  animal <- subset(df_LP5vs7_R7, Animal==i)
  lines(animal$Date, animal$CumDur, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i])
}

# add a title
title("5vs7 day by day")

# add a legend
legend("topright", inset=c(-0.2,0), legend = c(1:nanimals), cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Animal")




##### We haven't check if there is any difference in Suc Disc for Group A and B #####

ArmPosition <- c("A","A","A","A","A","A","B","B","B","B","B","B","A","A","A","A","A","A","B","B","B","B","B","A","A","A","A","A","B","B","B","B","B","B")
df_LP2vs5_R5$ArmPosition <- ArmPosition
df_LP2vs5_R5$ArmPosition <- factor(df_LP2vs5_R5$ArmPosition)

library(directlabels)
df_LP2vs5_R5 %>% 
  ggplot(aes(x = Date, y = CumDur, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "2vs5") +
  ylab("R5_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

### I shouldn't do a t.test because the sample is very small but I don't know how to do a wilcoxon when #
# A and B have different lenghts ###

df_LP2vs5_R5 %>% filter(Date == 1) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)
df_LP2vs5_R5 %>% filter(Date == 2) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)
df_LP2vs5_R5 %>% filter(Date == 3) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)

## 2vs7 ##

ArmPosition <- c("A","A","A","A","A","A","B","B","B","B","B","A","A","A","A","A","A","B","B","B","B","B","B","A","A","A","A","A","A","B","B","B","B","B","B")
df_LP2vs7_R7$ArmPosition <- ArmPosition
df_LP2vs7_R7$ArmPosition <- factor(df_LP2vs7_R7$ArmPosition)

df_LP2vs7_R7 %>% 
  ggplot(aes(x = Date, y = CumDur, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "2vs7") +
  ylab("R7_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

### I shouldn't do a t.test because the sample is very small but I don't know how to do a wilcoxon when #
# A and B have different lenghts ###

df_LP2vs7_R7 %>% filter(Date == 1) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)
df_LP2vs7_R7 %>% filter(Date == 2) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)
df_LP2vs7_R7 %>% filter(Date == 3) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)

## 5vs7 ##

ArmPosition <- c("A","A","A","A","A","B","B","B","B","B","B","A","A","A","A","A","A","B","B","B","B","B","B","A","A","A","A","A","A","B","B","B","B","B")
df_LP5vs7_R7$ArmPosition <- ArmPosition
df_LP5vs7_R7$ArmPosition <- factor(df_LP5vs7_R7$ArmPosition)

df_LP5vs7_R7 %>% 
  ggplot(aes(x = Date, y = CumDur, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "5vs7") +
  ylab("R7_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

### I shouldn't do a t.test because the sample is very small but I don't know how to do a wilcoxon when #
# A and B have different lenghts ###

df_LP5vs7_R7 %>% filter(Date == 1) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)
df_LP5vs7_R7 %>% filter(Date == 2) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)
df_LP5vs7_R7 %>% filter(Date == 3) %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)


################################ JUVENIL VS SUCROSE ###################

library(dplyr)
Jvs2_Pre <- filter(df_JAll, Sucrose == "S2", Time == "Pre")
shapiro.test(Jvs2_Pre$R1_pct) # I don't think I am doing it right, shoud do a two sample paired test cause I have two days
t.test(Jvs2_Pre$R1_pct)

Jvs2_Post <- filter(df_JAll, Sucrose == "S2", Time == "Post")
shapiro.test(Jvs2_Post$R1_pct)
t.test(Jvs2_Post$R1_pct)

Jvs5_Pre <- filter(df_JAll, Sucrose == "S5", Time == "Pre")
shapiro.test(Jvs5_Pre$R1_pct)
t.test(Jvs5_Pre$R1_pct)

Jvs5_Post <- filter(df_JAll, Sucrose == "S5", Time == "Post")
shapiro.test(Jvs5_Post$R1_pct)
t.test(Jvs5_Post$R1_pct)

Jvs7_Pre <- filter(df_JAll, Sucrose == "S7", Time == "Pre")
shapiro.test(Jvs7_Pre$R1_pct)
t.test(Jvs7_Pre$R1_pct)

Jvs7_Post <- filter(df_JAll, Sucrose == "S7", Time == "Post")
shapiro.test(Jvs7_Post$R1_pct) # not normal
t.test(Jvs7_Post$R1_pct)
wilcox.test(Jvs7_Post$R1_pct)

## Plotting

df_JAll$Date <- factor(df_JAll$Date, 
                              levels = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12"),
                              labels = c("one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve"))

neworderTime <- c("Pre", "Post")
df_JAll <- arrange(transform(df_JAll,
                           Time=factor(Time,levels=neworderTime)),Time)

library(ggplot2)    #### Just Pre
Jvs2_Pre %>% 
  ggplot(aes(Date, R1_pct, fill=Date))+
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "Jvs2_Pre")+
  scale_fill_manual(values = c("lightskyblue", "lightpink2"))+
  scale_y_continuous(breaks = seq(0,100,25))

df_JAll[df_JAll$Sucrose == "S2",] %>%       #### Pre and Post
  ggplot(aes(Date, R1_pct, fill=Date)) +
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "Jvs2")+
  scale_fill_manual(values = c("lightskyblue", "lightpink2","lightskyblue", "lightpink2"))+
  scale_y_continuous(breaks = seq(0,100,25))+
  facet_grid(~Time, scales="free_x")

df_JAll[df_JAll$Sucrose == "S5",] %>%       #### Pre and Post 
  ggplot(aes(Date, R1_pct, fill=Date)) +
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "Jvs5")+
  scale_fill_manual(values = c("darkorange2", "lightpink2","darkorange2", "lightpink2"))+
  scale_y_continuous(breaks = seq(0,100,25))+
  facet_grid(~Time, scales="free_x")

df_JAll[df_JAll$Sucrose == "S7",] %>%       #### Pre and Post 
  ggplot(aes(Date, R1_pct, fill=Date)) +
  geom_boxplot(alpha = .4, outlier.alpha = 1)+
  labs(title = "Jvs7")+
  scale_fill_manual(values = c("darkolivegreen3", "lightpink2","darkolivegreen3", "lightpink2"))+
  scale_y_continuous(breaks = seq(0,100,25))+
  facet_grid(~Time, scales="free_x")

#### plot Individually ####

library(ggplot2)
library(dplyr)
library(directlabels)

Jvs2_Pre %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = Group)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "2vsJ Pre") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

neworderDate <- c("D7", "D10")
Jvs2_Post <- arrange(transform(Jvs2_Post,
                           Date=factor(Date,levels=neworderDate)),Date)

Jvs2_Post %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = Group)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "2vsJ Post") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

Jvs5_Pre %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = Group)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "5vsJ Pre") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

neworderDate <- c("D8", "D11")
Jvs5_Post <- arrange(transform(Jvs5_Post,
                               Date=factor(Date,levels=neworderDate)),Date)

Jvs5_Post %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = Group)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "5vsJ Post") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

Jvs7_Pre %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = Group)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "7vsJ Pre") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

neworderDate <- c("D9", "D12")
Jvs7_Post <- arrange(transform(Jvs7_Post,
                               Date=factor(Date,levels=neworderDate)),Date)

Jvs7_Post %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = Group)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "7vsJ Post") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()


#### plot Individually BY ARM POSITION ####

ArmPosition <- c("A","A","A","A","A","A","B","B","B","B","B","B","A","A","A","A","A","A","B","B","B","B","B")
Jvs2_Pre$ArmPosition <- ArmPosition
Jvs2_Pre$ArmPosition <- factor(Jvs2_Pre$ArmPosition)

Jvs2_Pre %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "2vsJ Pre") +
  ylab("R1_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()
  
Jvs2_Pre %>% filter(Date == "D1") %>% 
    summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)
Jvs2_Pre %>% filter(Date == "D4") %>%
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)

ArmPosition <- c("B","B","B","A","A","A","B","B","B","A","A","A","A","A","A","B","B","B","A","A","A","B","B","B")
Jvs2_Post$ArmPosition <- ArmPosition
Jvs2_Post$ArmPosition <- factor(Jvs2_Post$ArmPosition)

neworderDate <- c("D7", "D10")
Jvs2_Post <- arrange(transform(Jvs2_Post,
                               Date=factor(Date,levels=neworderDate)),Date)

Jvs2_Post %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "2vsJ Post") +
  ylab("R1_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

Jvs2_Post %>% filter(Date == "D7") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)
Jvs2_Post %>% filter(Date == "D10") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)


ArmPosition <- c("A","A","A","A","A","A","B","B","B","B","B","B","A","A","A","A","A","A","B","B","B","B","B","B")
Jvs5_Pre$ArmPosition <- ArmPosition
Jvs5_Pre$ArmPosition <- factor(Jvs5_Pre$ArmPosition)

Jvs5_Pre %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "5vsJ Pre") +
  ylab("R1_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

Jvs5_Pre %>% filter(Date == "D2") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)
Jvs5_Pre %>% filter(Date == "D5") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)


ArmPosition <- c("A","A","A","B","B","B","A","A","A","B","B","B","B","B","B","A","A","B","B","B","A","A","A")
Jvs5_Post$ArmPosition <- ArmPosition
Jvs5_Post$ArmPosition <- factor(Jvs5_Post$ArmPosition)

neworderDate <- c("D8", "D11")
Jvs5_Post <- arrange(transform(Jvs5_Post,
                               Date=factor(Date,levels=neworderDate)),Date)

Jvs5_Post %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "5vsJ Post") +
  ylab("R1_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

Jvs5_Post %>% filter(Date == "D8") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)
Jvs5_Post %>% filter(Date == "D11") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)


ArmPosition <- c("A","A","A","A","A","A","B","B","B","B","B","B","A","A","A","A","A","A","B","B","B","B","B","B")
Jvs7_Pre$ArmPosition <- ArmPosition
Jvs7_Pre$ArmPosition <- factor(Jvs7_Pre$ArmPosition)

Jvs7_Pre %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "7vsJ Pre") +
  ylab("R1_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

Jvs7_Pre %>% filter(Date == "D3") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)
Jvs7_Pre %>% filter(Date == "D6") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)


ArmPosition <- c("B","B","B","A","A","A","B","B","B","A","A","A","A","A","A","B","B","B","A","A","A","B","B","B")
Jvs7_Post$ArmPosition <- ArmPosition
Jvs7_Post$ArmPosition <- factor(Jvs7_Post$ArmPosition)

neworderDate <- c("D9", "D12")
Jvs7_Post <- arrange(transform(Jvs7_Post,
                               Date=factor(Date,levels=neworderDate)),Date)

Jvs7_Post %>% 
  ggplot(aes(x = Date, y = R1_pct, group = Animal, color = ArmPosition)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x - .2), cex = 0.7, "first.bumpup")) +
  geom_point() +
  ylim(0,100)+
  labs(title = "7vsJ Post") +
  ylab("R1_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

Jvs7_Post %>% filter(Date == "D9") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)
Jvs7_Post %>% filter(Date == "D12") %>% 
  summarize(p = t.test(R1_pct[which(ArmPosition == 'A')],R1_pct[which(ArmPosition =='B')])$p.value)





###### Difference in Arm Position for Sucrose Disc after Surgery #####

df_LP2vs7_R7_post <- filter(df_SucroseDiscPost, Condition == "C2vs7", Zone == "R4_pct")

ArmPosition <- c("B","B","B","A","A","A","B","B","B","A","A","A")
df_LP2vs7_R7_post$ArmPosition <- ArmPosition
df_LP2vs7_R7_post$ArmPosition <- factor(df_LP2vs7_R7_post$ArmPosition)

df_LP2vs7_R7_post %>% 
  ggplot(aes(x = ArmPosition, y = CumDur, group = Animal, color = ArmPosition)) +
  geom_point() +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  ylim(0,100)+
  labs(title = "2vs7 Post") +
  ylab("R7_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

df_LP2vs7_R7_post %>%
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)


### 5vs7 post individually ###

df_LP5vs7_R7_post <- filter(df_SucroseDiscPost, Condition == "C5vs7", Zone == "R4_pct")

ArmPosition <- c("A","A","A","B","B","B","A","A","A","B","B","B")
df_LP5vs7_R7_post$ArmPosition <- ArmPosition
df_LP5vs7_R7_post$ArmPosition <- factor(df_LP5vs7_R7_post$ArmPosition)

df_LP5vs7_R7_post %>% 
  ggplot(aes(x = ArmPosition, y = CumDur, group = Animal, color = ArmPosition)) +
  geom_point() +
  geom_dl(aes(label = Animal), method = list(dl.trans(x = x + .2), cex = 0.7, "last.bumpup")) +
  ylim(0,100)+
  labs(title = "5vs7 Post") +
  ylab("R7_pct") +
  theme(plot.title = element_text(hjust = .5)) +
  theme_bw()

df_LP5vs7_R7_post %>% filter(Date == "D12") %>% 
  summarize(p = t.test(CumDur[which(ArmPosition == 'A')],CumDur[which(ArmPosition =='B')])$p.value)


###### Row data: rewards time spent normaliyed by time spent in the arm, A-B (higher reward - lower r) #######
#### PRE ####

library(openxlsx)
Arm2vs5 <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/Arms/2vs5AandR.xlsx")
Arm2vs7 <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/Arms/2vs7AandR.xlsx")
Arm5vs7 <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/Arms/5vs7AandR.xlsx")
ArmJvs2 <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/Arms/Jvs2AandR.xlsx")
ArmJvs5 <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/Arms/Jvs5AandR.xlsx")
ArmJvs7 <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/Arms/Jvs7AandR.xlsx")

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





### One-way repeated measures ANOVA to check the influence of day on RA-RA ###

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

# Computation #

library(ICSNP)
library(emmeans)

Arm2vs5$Date <- as.factor(Arm2vs5$Date)
Arm2vs5$Animal <- as.factor(Arm2vs5$Animal)

summary(aov(Rdif ~ Date + Error(Animal/Date), data=Arm2vs5))             
OneRMAnova2vs5 <- aov(Rdif ~ Date + Error(Animal/Date), data=Arm2vs5)
emmORMA2vs5 <- emmeans(OneRMAnova2vs5, ~ Date)
pairs(emmORMA2vs5)

### Sum 1 to every row of Rdif and make it positive ###

# Done in excel #

Arm2vs5Positive <- read.xlsx("D:/DOCUMENTS/5DHT NAcc/Batch 1/Arms/2vs5AandRPositive.xlsx")

# Remove a columns #

Arm2vs5Positive$X15 <- NULL

Arm2vs5Positive$Date <- as.factor(Arm2vs5Positive$Date)
Arm2vs5Positive$Animal <- as.factor(Arm2vs5Positive$Animal)

# Normality assumption #

Arm2vs5Positive %>%
  group_by(Date) %>%
  shapiro_test(RdifPos)

summary(aov(RdifPos ~ Date + Error(Animal/Date), data=Arm2vs5Positive)) # the same with the positive!
OneRMAnova2vs5P <- aov(RdifPos ~ Date + Error(Animal/Date), data=Arm2vs5Positive)
emmORMA2vs5P <- emmeans(OneRMAnova2vs5P, ~ Date)
pairs(emmORMA2vs5P)


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

# because Date is a fix effect shoulnd't be a random one too #

lmm5vs7 <- lmer(Rdif ~ Date + (1 | Animal), data = Arm5vs7) 
summary(lmm5vs7)
Anova(lmm5vs7)

# because the residual value was so low#

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

# because the residual value was so low#

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



### continue checking the effect of the strating group ### Not considering that it is repeated measures, not sure. ###

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



### If starting Group makes no differences, what about Arm Position ###

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

################## Do it in SPSS and compare resutls ########################set

#### 1)	Is the 5% a turning point?
#### 2)	Is there an influence of days?
#### 3)	Is there a difference between DHT and Sham? I-G
