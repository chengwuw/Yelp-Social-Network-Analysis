

#******************************** 277 Yelp Social Network Analysis *********************************

#Filename: Yelp_elite.R
#Date: January 2021
#Author: Team1A

#*************************************************************************************************** 
library(dplyr)
library(stringr)
library(tidyverse)
library(igraph)
library(sqldf)
library(psych)

setwd("D:/UCI/277 LEC A CUST & SOCIAL ANLYT/project")

data <- read.csv("user_df.csv",header=T, as.is=T)
data$year_since <- 2020 - data$year
data$elite_rate <- data$elite_count/data$year_since

##### POISSON REGRESSION ##################
data_elite <- subset(data, elite_count>=1 & no_of_friends >= 2 &review_count >= 2 &fans>=2)
data_nonelite <- subset(data, elite_count==0 &  no_of_friends >= 2 & review_count >= 2)

test_elite <- data_elite #sample_n(data_elite, 100)

#make links
test_friends <- test_elite %>% select(user_id, friends)
test_users <- strsplit(test_friends$friends, split = ",")
link_elite <- data.frame(Source = rep(test_friends$user_id, sapply(test_users, length)), Target = unlist(test_users))
link_elite$Source <-  gsub(" ", "", link_elite$Source)
link_elite$Target <-  gsub(" ", "", link_elite$Target)

# make node list
nodelist_source <- as.data.frame(unique(link_elite$Source))
nodelist_target <- as.data.frame(unique(link_elite$Target))
names(nodelist_source) <- "id"
names(nodelist_target) <- "id"
nodelist_elite <- unique(rbind(nodelist_source,nodelist_target))

node_elite <- subset(data, user_id %in% nodelist_elite$id)
node_list <- unique(node_elite$user_id)
link_elite <- subset(link_elite, Source %in% node_list & Target %in% node_list)

ig_elite <- graph_from_data_frame(d = link_elite, vertices = node_elite$user_id, directed=T) 

in_degree_elite <- degree(ig_elite, mode='in')
out_degree_elite <- degree(ig_elite, mode='out')
all_degree_elite <- degree(ig_elite, mode='all')

hub_elite <- hub.score(ig_elite)$vector
authority_elite <- authority.score(ig_elite)$vector

# closeness:
#closeness_elite <- closeness(ig_elite, mode="all", weights=NA) 
# betweenness:
#betweenness_elite <- betweenness(ig_elite, directed=T, weights=NA)

# neighbour mean value
mean_values <- inner_join(link_elite, data, by = c("Target"="user_id")) %>%
  group_by(Source) %>%
  summarise(nghb_mn_fans = mean(fans),
            nghb_mn_friend_cnt = mean(no_of_friends),
            nghb_mn_review_cnt = mean(review_count),
            nghb_mn_avg_stars = mean(average_stars),
            nghb_mn_elite_cnt = mean(elite_count)) %>%
  ungroup() %>%
  arrange(Source)

deg_in2 <- as.data.frame(in_degree_elite)
deg_in2 <- cbind(id = rownames(deg_in2), deg_in2)
deg_out2 <- as.data.frame(out_degree_elite)
deg_out2 <- cbind(id = rownames(deg_out2), deg_out2)

#closeness2 <- as.data.frame(closeness_elite)
#closeness2 <- cbind(id = rownames(closeness2), closeness2)
#betweenness2 <- as.data.frame(betweenness_elite)
#betweenness2 <- cbind(id = rownames(betweenness2), betweenness2)

hub_scores2 <- as.data.frame(hub_elite)
hub_scores2 <- cbind(id = rownames(hub_scores2), hub_scores2)
authority_scores2 <- as.data.frame(authority_elite)
authority_scores2 <- cbind(id = rownames(authority_scores2), authority_scores2)

df_poisson1 <- sqldf(
  "SELECT 
    hub_scores2.id,hub_elite, authority_elite, in_degree_elite, out_degree_elite 
FROM hub_scores2, authority_scores2, deg_in2, deg_out2
WHERE hub_scores2.id = authority_scores2.id
AND hub_scores2.id = deg_in2.id
AND hub_scores2.id = deg_out2.id" )

df_poisson2 <- sqldf(
  "SELECT 
    data.user_id, data.review_count,data.fans, data.no_of_friends, data.average_stars, data.elite_count, 
    hub_elite, authority_elite, in_degree_elite, out_degree_elite, 
    nghb_mn_friend_cnt, nghb_mn_review_cnt, nghb_mn_avg_stars, nghb_mn_fans, nghb_mn_elite_cnt
FROM data, df_poisson1, mean_values 
WHERE data.user_id = df_poisson1.id
AND data.user_id = mean_values.Source")

elite_prediction<- glm(elite_count  ~ review_count + fans + no_of_friends + average_stars
                       + hub_elite + authority_elite 
                       + in_degree_elite + out_degree_elite
                       + nghb_mn_friend_cnt + nghb_mn_review_cnt + nghb_mn_avg_stars
                       + nghb_mn_fans + nghb_mn_elite_cnt, 
                       family="poisson", data=df_poisson2)

summary(elite_prediction)

star_prediction<- lm( average_stars ~ review_count + fans + no_of_friends + elite_count
                       + hub_elite + authority_elite 
                       + in_degree_elite + out_degree_elite
                       + nghb_mn_friend_cnt + nghb_mn_review_cnt + nghb_mn_avg_stars
                       + nghb_mn_fans + nghb_mn_elite_cnt, 
                       data=df_poisson2)

summary(star_prediction)


#### logistic regression ##########

# create a new column: elite vs non-elite 
data1 <- subset(data, no_of_friends >=2)
data1 <- data1 %>%
  mutate(iselite = ifelse(elite_rate >= 0.01,1,0))

data1 <- sample_n(data1,100000)
# create a new column: elite_friend_cnt
data1_elite <- subset(data1, elite_count >= 1)
elite_list <- unique(data1_elite$user_id)
## make target of all data 
test_friends <- data1 %>% select(user_id, friends)
test_users <- strsplit(test_friends$friends, split = ",")
link_data <- data.frame(Source = rep(test_friends$user_id, sapply(test_users, length)), Target = unlist(test_users))
link_data$Source <-  gsub(" ", "", link_data$Source)
link_data$Target <-  gsub(" ", "", link_data$Target)


link_elite_friends <- link_data %>%
  group_by(Source) %>%
  mutate(elite_friend_cnt = sum(ifelse(Target %in% elite_list,1,0)))%>%
  select(Source, elite_friend_cnt) %>%
  distinct(Source, .keep_all = TRUE)


# right join with data on link_elite_friends.source = data.user_id
# set null = 0

data2 <- left_join(data1, link_elite_friends, by = c("user_id" = "Source"))
data2[is.na(data2$elite_friend_cnt),"elite_friend_cnt"] = 0

# create a new column: treatment
data2 <- data2 %>%
  mutate(treatment = ifelse(elite_friend_cnt >= 1,1,0))
#table(data2$elite_friend_cnt)

#psych::describe(data2)

# Taking log for the highly skewed terms, abs(skew value)>=2
# review_count,fans,no_of_friends,elite_count,elite_rate,iselite,elite_friend_cnt
data2$ln_review_count <- log(data2$review_count+1)
data2$ln_fans <- log(data2$fans+1)
data2$ln_no_of_friends <- log(data2$no_of_friends+1)
data2$ln_elite_count <- log(data2$elite_count+1)
data2$ln_elite_rate <- log(data2$elite_rate+1)
data2$ln_iselite <- log(data2$iselite+1)
data2$ln_elite_friend_cnt <- log(data2$elite_friend_cnt+1)


# calculate the mean for each covariates by treatment status

summary_treat <- data2 %>%
  group_by(treatment) %>%
  select(c('review_count','fans','no_of_friends','average_stars', 'year_since',
           'elite_friend_cnt', 'elite_count', 'elite_rate','iselite')) %>%
  summarise_all(funs(mean(., na.rm = T)))
htmlTable::htmlTable(round(summary_treat,2))

cov1 <- c('ln_review_count','ln_fans','ln_no_of_friends','average_stars', 'year_since', 'ln_iselite' ,
          'ln_elite_friend_cnt', 'ln_elite_count', 'ln_elite_rate') 

lapply(cov1, function(n) {
  t.test(data2[, n] ~ data2$treatment)
})


### Propensity score estimation #####

model_ps <- glm(treatment ~  ln_review_count + ln_fans +ln_no_of_friends + average_stars + year_since
                + ln_elite_count + ln_elite_rate + ln_elite_friend_cnt,
                family = binomial(), data = data2) 

# propensity score for each user
propensity_score <- data.frame(pr_score = predict(model_ps, type = "response"),
                               treatment = model_ps$model$treatment )

cov2 <- c('ln_review_count','ln_fans','ln_no_of_friends','average_stars', 'year_since', 'ln_iselite' ,
          'ln_elite_friend_cnt', 'ln_elite_count', 'ln_elite_rate') 

data_nomiss <- data2 %>%  # MatchIt does not allow missing values
  select(elite_count, treatment, one_of(cov2)) %>%
  na.omit()
library(MatchIt)
mod_match <- matchit(formula = treatment ~ ln_review_count + ln_fans + ln_no_of_friends 
                     + average_stars + year_since + ln_elite_count + ln_elite_rate + ln_iselite, 
                     data = data_nomiss, caliper = 0.02, method = "nearest")
summary(mod_match)
plot(mod_match)

## create new data
data_matched <- match.data(mod_match,drop.unmatched = T)
dim(data_matched) #Dimensions of the matched data frame
table(data_matched$treatment) #Treatment distribution in the matched data frame

summary_matched <- data_matched %>%
  group_by(treatment) %>% 
  select(c('ln_review_count','ln_fans','ln_no_of_friends','average_stars', 'year_since',
           'ln_elite_count', 'ln_elite_rate','ln_iselite')) %>%
  summarise_all(funs(mean(., na.rm = T)))
htmlTable::htmlTable(round(summary_matched,2))

# t test after matching 
cov3 <- c('ln_review_count','ln_fans','ln_no_of_friends','average_stars', 'year_since', 'ln_iselite' ,
          'ln_elite_friend_cnt', 'ln_elite_count', 'ln_elite_rate')

lapply(cov3, function(v) {
  t.test(data_matched[, v] ~ data_matched$treatment)
})

# model with all variables of interest
# 'review_count','fans','no_of_friends','average_stars', 'year_since', 'iselite' ,
# 'elite_friend_cnt', 'elite_count', 'treatment','elite_rate'

glm_treat1 <- glm(ln_iselite ~ ln_review_count + ln_fans + ln_no_of_friends + average_stars + year_since
                  + ln_elite_count + treatment + ln_elite_rate +ln_elite_friend_cnt,
                  data = data_matched, family='binomial')
summary(glm_treat1)
exp(coef(glm_treat1))







