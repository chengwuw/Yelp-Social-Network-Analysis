

#********************************* 277 Yelp Social Network Analysis ***************************************************

#Filename: Yelp_data_visualization.R
#Date: January 2021
#Author: Team1A

#***************************************************************************************************
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(igraph)
library(sqldf)
library(psych)
library(ggplot2)
library(gridExtra)

setwd("/Users/yuqingwang/Desktop/yelp")

data <- read.csv("user_df.csv",header=T, as.is=T)

data$user_id <-  gsub(" ", "", data$user_id)
##################################################sample
#remove users with no friends
sample <- subset(data, friends != "None")
sample_elite <-subset(sample, elite >=1)
sample_nonelite <-subset(sample, elite =="")
#=========================================2015links
#remove users with friends less than 5
test <- subset(sample,year ==2015 & review_count >=2 &no_of_friends >=2)
#write_csv(test,path ="/Users/yuqingwang/Desktop/yelp/user2015.csv")
test_elite <-subset(test, elite >=1)
test_nonelite <-subset(test, elite =="")
elite_s <- sample_n(test_elite,50)
nonelite_s <- sample_n(test_nonelite,50)
test_sample <- rbind(elite_s,nonelite_s)

#make links
test_friends <- test_sample %>% select(user_id, friends)
test_users <- strsplit(test_friends$friends, split = ",")
test_data <- data.frame(user_id = rep(test_friends$user_id, sapply(test_users, length)), friends = unlist(test_users))
#write.csv(test_dat, file="/Users/yuqingwang/Desktop/links.csv")
#=============================sample summary
n_sample <- sample %>%
  select(elite,review_count,fans,average_stars,no_of_friends,elite_count,year) 

elite0 <- n_sample[n_sample$elite_count==0,] 
elite1 <- n_sample[n_sample$elite_count>=1,] 

#total
n_sample_des<-psych::describe(n_sample)
view(n_sample_des)
#elite/non_elite
elite0_des<-psych::describe(elite0)
view(elite0_des)
elite1_des<-psych::describe(elite1)
view(elite1_des)
################################ data visualization
#n_test distribution
par(mfrow=c(2, 3))
colnames <- dimnames(n_sample)[[2]]
for (i in 2:7) {
  hist(n_sample[,i], main=colnames[i], xlab=NA, probability=TRUE, col="#70DB93", border="white")
}

table(n_sample$elite_count)

#New users per year barplot
new_user_year <- as.data.frame(table(n_sample$year))
ggplot(new_user_year, aes(x=Var1, y=Freq)) + 
  geom_bar(aes(fill = Var1), position = "dodge", stat="identity") + 
  labs(title="New users per year",x="Year",y="Number of new users")

#elite number per year

eliteyear <- unlist(strsplit(sample_elite$elite, split = ","))

elite_year <- as.data.frame(table(eliteyear))
ggplot(elite_year, aes(x=eliteyear, y=Freq)) + 
  geom_bar(aes(fill = eliteyear), position = "dodge", stat="identity") + 
  labs(title="number of elites per year",x="Year",y="Number of elite")
#elite growth rate per year
diff(elite_year$Freq)/elite_year[-1,2]
growth_rate <- as.data.frame(rate<- diff(elite_year$Freq)/elite_year[-1,2])
growth_rate$year <- c(2007:2018)

ggplot(growth_rate, aes(x=year, y=rate)) + geom_line() + geom_point(size=4, shape=20) + 
  labs(title="elite growth rate per year",x="Year",y="rate")

#boxplot
#elite &non_elite
n_sample$Elite <- ifelse(n_sample$elite_count==0,'non_elite','elite')
#number of friends boxplot
b1 <- ggplot(n_sample, aes(x=Elite, y=no_of_friends)) + geom_boxplot(aes(fill=Elite),outlier.colour="blue", outlier.shape=8,outlier.size=0.5)
b1 <- b1 +theme(legend.position="none")+scale_y_continuous(limits = quantile(n_sample$no_of_friends, c(0.1, 0.9)))+theme(axis.title.x=element_blank())

#average star rating of elite and non-elite boxplot
b2 <- ggplot(n_sample, aes(x=Elite, y=average_stars)) + geom_boxplot(aes(fill=Elite),outlier.colour="blue", outlier.shape=8,outlier.size=0.5)
b2 <- b2 + theme(legend.position="none")+theme(axis.title.x=element_blank())

#average number of fans of elite and non-elite boxplot
b3 <- ggplot(n_sample, aes(x=Elite, y=fans)) + geom_boxplot(aes(fill=Elite),outlier.colour="blue", outlier.shape=8,outlier.size=0.5)
b3 <- b3 + theme(legend.position="none")+scale_y_continuous(limits = quantile(n_sample$no_of_friends, c(0.1, 0.9)))+theme(axis.title.x=element_blank())

#average review_count of elite and non-elite boxplot
b4 <- ggplot(n_sample, aes(x=Elite, y=review_count)) + geom_boxplot(aes(fill=Elite),outlier.colour="blue", outlier.shape=8,outlier.size=0.5)
b4 <- b4 + theme(legend.position="none")+scale_y_continuous(limits = quantile(n_sample$no_of_friends, c(0.1, 0.9)))+theme(axis.title.x=element_blank())

grid.arrange(b1,b2,b3,b4,ncol=2)


