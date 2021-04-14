
#********************************* 277 Yelp Social Network Analysis ***************************************************

#Filename: Yelp_communities.R
#Date: January 2021
#Author: Team1A

#***************************************************************************************************
library(dplyr)
library(tidyverse)
library(igraph)

#you will get different results everytime you run this code; 
#you can make changes to year, review_count, and no_of_friends requirements

setwd("~/MSBA/BANA277/YelpProject")

dat <- read.csv("user_df.csv",header=T, as.is=T)

sample <- subset(dat, friends != "None")
test <- subset(sample, year ==2015 & review_count >=2 & no_of_friends >=2)

#make links
test_friends <- test %>% select(user_id, friends)
test_users <- strsplit(test_friends$friends, split = ",")
test_dat <- data.frame(user_id = rep(test_friends$user_id, sapply(test_users, length)), friends = unlist(test_users))

#file is still too big, take a sample with 100k nodes
samp_net <- sample_n(test_dat,100000)

#try making network 
test_network <- graph.data.frame(samp_net)
network_s <- simplify(test_network)

net_deg <- degree(network_s)
all_degree <- degree(test_network, mode='all')

#graph user with max degrees
sub_all <- subcomponent(network_s, which(all_degree==max(all_degree)),'all')
g_sub<-induced_subgraph(network_s,sub_all)

#cliques/communities
cliques <- max_cliques(g_sub)
cliqueBP <- matrix(c(rep(paste0("cl", seq_along(cliques)), sapply(cliques, length)), names(unlist(cliques))), ncol=2, )
bp <- graph_from_edgelist(cliqueBP, directed = F) #shows which clique users belong to
V(bp)$type <- grepl("cl", V(bp)$name)

#check clique stats
clique_num(g_sub)
max(cliqueBP)

#merging information of each user found in the network
id_list<-list(V(g_sub)$name)
mx <- max(lengths(id_list))
d1 <- data.frame(lapply(id_list, `length<-`, mx))

#need to change column names; use colnames(d1) then paste it over 
colnames(d1)[1] <- "user_id"

d1_df <- merge(d1, dat[,c("user_id", "name", "review_count", "elite", "fans", "average_stars", 
                             "no_of_friends", "elite_count", "year")])

#this is dataframe that includes info about all users in subgraph
d1_df <- d1_df %>%
  mutate(elite = ifelse(elite_count >= 1,1,0))

top_d1 <- head(arrange(d1_df, desc(no_of_friends)), n = 15)
top_d1 <- subset(top_d1, year == 2015)

#save df as csv
write.csv(top_d1, file="topusers2015.csv")

#network stats
edge_density(bp, loops = FALSE) #0.01104972

#closeness = steps required to reach every vertex
centr_clo(bp, mode ='all', normalized=T) #0.2584915

#betweenness = "bridges"
bridge <- as.data.frame(betweenness(bp, directed=T, normalized=T)) 
max(betweenness(bp, directed=T, normalized=T)) #0.9320919
which(bridge == max(bridge))

#find all non-zero bridges, should have user_id attached
bridge_id <- as.data.frame(which(bridge > 0, arr.ind=TRUE))
bridge_id <- cbind(newColName = rownames(bridge_id), bridge_id)
rownames(bridge_id) <- 1:nrow(bridge_id)
colnames(bridge_id)[1] <- "user_id"

#merge bridges with user_df attributes
id_bridge <-list(bridge_id$user_id)
mx2 <- max(lengths(id_bridge))
bridge_users <- data.frame(lapply(id_bridge, `length<-`, mx2))
colnames(bridge_users)[1] <- "user_id"

bridge_merge <- merge(bridge_users, dat[,c("user_id", "name", "review_count", "elite", "fans", "average_stars", 
                                           "no_of_friends", "elite_count", "year")])

bridge_merge <- bridge_merge %>%
  mutate(elite = ifelse(elite_count >= 1,1,0))

#communities
graph.com <- fastgreedy.community(as.undirected(g_sub))

#find communities and sort by color
V(g_sub)$color <- graph.com$membership + 1

#show largest clique from subgraph, identify bridge
pdf("cliques2015-2f.pdf", 10,10)
plot(bp, vertex.size=all_degree,
     vertex.color = adjustcolor("lightgoldenrod3", alpha.f = 0.6),
     edge.color = adjustcolor("#41424c", alpha.f = 0.3),
     vertex.frame.color=adjustcolor("#41424c", alpha.f = 0.3),
     vertex.label = ifelse(V(bp)$name %in% top_d1$user_id, top_d1$name, NA),
     vertex.label.color = "darkred",
     layout=layout_with_lgl,
     asp = 0.8,
     main = "Largest Clique in Subgraph, Paige has the highest betweenness",
     dpi = 300)
dev.off()

#where are the bridge's friends
#change name based on user_id with highest betweenness value
neigh.nodes <- neighbors(bp, V(bp)[name=="_cpU0VVdQcfN5AnuL6M56A"], mode="out")

# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(bp))
vcol[unlist(bridge.path$vpath)] <- "gold"

#Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"
pdf("Paige-neighbors.pdf", 10,10)
plot(bp, vertex.color=adjustcolor(vcol, alpha.f=0.6), 
     vertex.size=all_degree/1.5,
     edge.width= 1, edge.arrow.mode=0,
     edge.color = adjustcolor("#41424c", alpha.f = 0.3),
     vertex.frame.color=adjustcolor("#41424c", alpha.f = 0.3),
     vertex.label = ifelse(V(bp)$name %in% top_d1$user_id, top_d1$name, NA),
     vertex.label.color = "darkred",
     layout=layout_with_lgl,
     asp = 0.8,
     main = "Largest Clique showing Paige's neighbors (all nodes one step out from Paige)",
     dpi = 300)
dev.off()

#include path
bridge.path <- shortest_paths(bp,
                            from = V(bp)[name=="_cpU0VVdQcfN5AnuL6M56A"],
                            to = V(bp)[name=="meizfKQ8BOZDHl1WDBRZkA"],
                            output = "both") # both path nodes and edges
# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(bp))
ecol[unlist(bridge.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(bp))
ew[unlist(bridge.path$epath)] <- 4

#plot path
pdf("PaigeMilly_path.pdf", 10,10)
plot(bp, vertex.color=adjustcolor("gray40", alpha.f = 0.6), 
     vertex.size=all_degree/1.5,
     edge.color=adjustcolor(ecol,alpha.f = 0.3),
     edge.width=ew, edge.arrow.mode=0,
     vertex.frame.color=adjustcolor("#41424c", alpha.f = 0.3),
     vertex.label = ifelse(V(bp)$name %in% d1_df$user_id, d1_df$name, NA),
     vertex.label.color = "darkred",
     layout=layout_with_lgl,
     asp = 0.8,
     main = "Path from Paige to Milly",
     dpi = 300)
dev.off()

#show all communities found in subgraph
pdf("communities2015-bridges.pdf", 10,10)
plot(g_sub, 
     vertex.color = adjustcolor(V(g_sub)$color, alpha.f=0.6),
     vertex.size=all_degree,
     vertex.label = ifelse(V(g_sub)$name %in% bridge_merge$user_id, bridge_merge$name, NA),
     vertex.label.color = "darkred",
     vertex.frame.color=adjustcolor("#41424c", alpha.f = 0.3),
     vertex.label.size = 0.1,
     edge.arrow.size=0.1,
     edge.color = adjustcolor("#41424c", alpha.f = 0.3),
     edge.width = 1,
     edge.arrow.mode=0,
     layout=layout_with_lgl,
     asp = 0.9,
     dpi=300
) 
dev.off()