

#********************************* 277 Yelp Social Network Analysis ***************************************************

#Filename: Yelp_extraction.R
#Date: January 2021
#Author: Team1A

#***************************************************************************************************
library(jsonlite)
library(tidyr)
library(dplyr)
library(stringr)
library(tidyjson)
library(tidyverse)


#install.packages("tidyjson")

user_out <- lapply(readLines("yelp_academic_dataset_user.json"), fromJSON)

#parse through each user's data and only obtain columns we are interested in

yelp_parse <- function(x) {
  
  parse_list <- list(user_id = x$user_id, 
                     name = x$name, 
                     review_count = x$review_count, 
                     yelping_since = x$yelping_since, 
                     elite = x$elite, 
                     friends = x$friends, 
                     fans = x$fans, 
                     average_stars = x$average_stars)
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- data_frame(user_id=parse_list$user_id,
                   name=parse_list$name, 
                   review_count = parse_list$review_count, 
                   yelping_since=parse_list$yelping_since, 
                   elite = parse_list$elite, 
                   friends = parse_list$friends, 
                   fans = parse_list$fans, 
                   average_stars = parse_list$average_stars)
  df
}

user_list <- lapply(user_out, FUN = yelp_parse) 

#should now have a dictionary with only keys and values we want

#now, make this into a table
user_df <- data.frame(matrix(unlist(user_list), nrow=length(user_list), byrow=TRUE),stringsAsFactors=FALSE)

#set column names
colnames(user_df) <- c("user_id", "name", "review_count", "yelping_since", "elite", "friends", "fans", "average_stars")


#count string of friends (how many characters appear after comma)
num_friends <- count.fields(textConnection(user_df$friends), sep = ",")
user_df$friend_count <- num_friends

#count string of elite count (how many times they earned 'elite' status)
user_df$elite_count <- sapply(strsplit(as.character(user_df$elite), ","), length)

#reformat date and split date into three columns (year, month, day)
user_df <- user_df %>% 
  mutate(year = year(user_df$yelping_since),
         month = month(user_df$yelping_since), 
         day = day(user_df$yelping_since))

#remove columns we don't need
user_df[ ,c('yelping_since', 'yelping_date', 'month', 'day')] <- list(NULL)


#save as csv
write.csv(user_df, file="user_df.csv")

#make subgroups of elite and non-elite
non_elite <- subset(user_df, user_df$elite_count == 0)
elite <- subset(user_df, user_df$elite_count != 0)
write.csv(non_elite, file="non_elite_users.csv")
write.csv(elite, file = "elite_users.csv")
