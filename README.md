# Yelp-Social-Network-Analysis
Used Social Network Analysis to detect Yelp's users communities, traits and influencers. Look into social impact on elite and non-elite users.

## Background
People with similar interests tend to gravitate towards each other and become associated in communities— clusters or groups of people that share similar traits with them. Since people tend to cluster with others similar to them, we can use community detection to find users with a high number of degrees, i.e. number of connections, and see if they have influence on how their friends in their network review a business. Connections between users and their friends can be represented by a network graph, where the nodes represent users in our data set. Edges represent their “friendship”, i.e. if they are friends on the platform. Therefore, studying Yelp’s social network can let us see if a user’s social influence can affect how others in their network give star ratings. We will also use regression analysis to see what user attributes contribute to a user’s decision to sign up as an “elite” Yelper. Results from community detection can be applied in targeted advertising campaigns so that we can show users more “personalized” advertisements so that they are more likely to visit the business that displayed the ad. Besides, from the result of regression, we can find how social features & "neighbourhood" impact their behaviors on Yelp and how to identify influencers and convert non-elite users to elite to increase revenues.

In this project, we want to address the following questions:
1. Yelp Elite Status
  a. Does having Elite status affect how users give ratings?
  b. Is there a difference in how Elite and Non-elite groups rate?
  c. What attributes affect a user’s decision to become and stay Elite?
2. User’s Friend
  a. How does a Yelp user’s network affect rating?
  b. How does a user’s elite friend influence her/him to become an elite user?
