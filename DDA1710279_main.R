#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#                                   Assignment - Recommendation System
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Goal :Building a recommendation system(collaborative) for the "BeerMart" store ,where customers will be recommended the beer that they are most likely to buy
# Description of the data :  Each record is composed of a beer's ID, the name of the user along with ratings provided by users. All ratings are on a scale from 1 to 5 with 5 being the best.
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------
#Getting started
#--------------------------------------------------------------------


#Packages required for solving
#recommenderlab
#ggplot2
#dplyr
#data.table

#Loading the libraries
library(recommenderlab)
library(ggplot2)
library(dplyr)
library(data.table)


# Importing the dataset
beer_raw_data <- read.csv("beer_data.csv")
str(beer_raw_data)

summary(beer_raw_data)

# beer_beerid         review_profilename review_overall 
# Min.   :    3   northyorksammy:  1846   Min.   :0.000  
# 1st Qu.: 1716   mikesgroove   :  1379   1st Qu.:3.500  
# Median :13892   BuckeyeNation :  1338   Median :4.000  
# Mean   :21661   Thorpe429     :  1072   Mean   :3.815  
# 3rd Qu.:39397   ChainGangGuy  :  1046   3rd Qu.:4.500  
# Max.   :77317   NeroFiddled   :  1031   Max.   :5.000  
# (Other)       :468272     

#------------------------------------------------------------------------------------
# DATA UNDERSTANDING AND DATA PREPARATION
#------------------------------------------------------------------------------------

#There are ratings of 0, which represent missing values,lets remove them

nrow(beer_raw_data[which(beer_raw_data$review_overall==0),]) #There are 6 rows with 0 rating

beer_raw_data <- beer_raw_data[which(beer_raw_data$review_overall!=0),]

#Rearranging the columns such that user column is first and the beer column is second
beer_raw_data <- beer_raw_data[,c(2,1,3)]

#standardising the case of the username
beer_raw_data$review_profilename <- as.factor(tolower(beer_raw_data$review_profilename))


#Understanding the attributes of the data
str(beer_raw_data)  #'data.frame' of	475978  obs. of  3 variables:

#Unique ratings
sort(unique(beer_raw_data$review_overall)) 

#[1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0


#Lets perform some dataquality checks

#Finding if there are any NA's
na_check <- sapply(beer_raw_data,function(x) sum(is.na(x)))
na_check #There are no NA's
# review_profilename        beer_beerid     review_overall 
#           0                  0                  0 

#Finding if there are blanks

blank_check <- function(x){
  check <- sapply(x,function(x) length(which(x =="")))
  return (check)
}

blank_check1 <- blank_check(beer_raw_data)

blank_check1 #There are 100 blank cells for the user, lets remove these rows
# review_profilename        beer_beerid     review_overall 
#         100                  0                  0 

#Removing the rows which has blanks
beer_raw_data[beer_raw_data==""]<-NA
beer_raw_data <- beer_raw_data[complete.cases(beer_raw_data),]

#Checking if blank cells are eliminated
blank_check2 <- blank_check(beer_raw_data)
blank_check2 #There are no blanks
# rating user     review_overall 
# 0                  0                  0 

#lets deduplicate the dataframe to eliminate duplicate entries

beer_raw_data <- beer_raw_data %>% distinct(review_profilename,beer_beerid, .keep_all = TRUE)

#generate a sequence of unique ID's corresponding to each unique user position the first column as the User ID column and the second column as the item column

setDT(beer_raw_data)[, user_id := .GRP, by = review_profilename] # generating Unique user ID's
beer_newdata <- beer_raw_data[,c(4,2,3)] # storing the resultant data in a new dataframe



#-----------------------------------------------------------------------------------------------------------
# Data preparation Part 2
#-----------------------------------------------------------------------------------------------------------
#Now that we have cleaned the data, the next objective is to :

#1.Choose only those beers that have at least N number of reviews,Figure out an appropriate value of N using EDA; this may not have one correct answer, but you shouldn't choose beers having extremely low number of ratings

#Lets check the number of unique beer ID's

length(unique(beer_newdata$beer_beerid)) #40302

avgreview_group <- group_by(beer_newdata,beer_beerid) %>% summarise(number_of_reviews=n())
summary(avgreview_group)

# beer_beerid    number_of_reviews
# Min.   :    3   Min.   :  1.00   
# 1st Qu.:16884   1st Qu.:  1.00   
# Median :37368   Median :  2.00   
# Mean   :36975   Mean   : 11.77   
# 3rd Qu.:56235   3rd Qu.:  5.00   
# Max.   :77317   Max.   :977.00   

#Retaining the beerID's which have an average number of reviews as >12
avgreview_groupredict_UBCF <- avgreview_group[which(avgreview_group$number_of_reviews >12),]
summary(avgreview_groupredict_UBCF)

# beer_beerid    number_of_reviews
# Min.   :    5   Min.   : 13.00   
# 1st Qu.: 3649   1st Qu.: 19.00   
# Median :23390   Median : 33.00   
# Mean   :26084   Mean   : 66.85   
# 3rd Qu.:45152   3rd Qu.: 73.00   
# Max.   :76816   Max.   :977.00 

#Lets aggregate the number of reviews per user and then check the statistics

avg_user_Review <- group_by(beer_newdata,user_id) %>% summarise(no_of_Reviews=n())
summary(avg_user_Review)

# user_id      no_of_Reviews    
# Min.   :    1   Min.   :   1.00  
# 1st Qu.: 5625   1st Qu.:   1.00  
# Median :11249   Median :   3.00  
# Mean   :11249   Mean   :  21.09  
# 3rd Qu.:16873   3rd Qu.:  11.00  
# Max.   :22497   Max.   :1842.00  

#Retaining the User ID's which have an average number of reviews as >21
avg_user_Review1 <- avg_user_Review[which(avg_user_Review$no_of_Reviews >21),]
summary(avg_user_Review1)

# user_id      no_of_Reviews   
# Min.   :    1   Min.   :  22.0  
# 1st Qu.: 1213   1st Qu.:  34.0  
# Median : 2702   Median :  57.0  
# Mean   : 3239   Mean   : 103.6  
# 3rd Qu.: 4797   3rd Qu.: 116.0  
# Max.   :14333   Max.   :1842.0 

#creating a new dataframe which has based on the above thresholds for number of Beer and user reviews

beer_final_data <- beer_newdata[(beer_newdata$beer_beerid %in% avgreview_groupredict_UBCF$beer_beerid)& (beer_newdata$user_id %in% avg_user_Review1$user_id),]

str(beer_final_data)
 beer_final_data$user_id <- as.factor(beer_final_data$user_id)
 beer_final_data$beer_beerid <- as.factor((beer_final_data$beer_beerid))
 beer_final_data$review_overall <- as.integer(beer_final_data$review_overall)
 beer_final_data <- as.data.frame(beer_final_data)

#2. The second objective of the data preparation phase is to Convert this data frame to a "realratingMatrix" before we build the  collaborative filtering models

#lets convert the beer_newdata1 to real rating matrix 

beer_rating_matrix <-  as(beer_final_data, "realRatingMatrix") #converting to real rating matrix

class(beer_rating_matrix)
# [1] "realRatingMatrix"
# attr(,"package")
# [1] "recommenderlab"

#getting some information
# get some informtaion
dimnames(beer_rating_matrix)
rowCounts(beer_rating_matrix)
colCounts(beer_rating_matrix)
rowMeans(beer_rating_matrix)

#------------------------------------------------------------------------------------------
# Data exploration
#------------------------------------------------------------------------------------------

#1.Determine how similar the first ten users are with each other and visualise it

similar_users <- similarity(beer_rating_matrix[1:10, ],
                            method = "cosine",
                            which = "users")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#Users (1,10),(2,3),(3,4),(7,9),(8,10),(9,2),(10,7),(12,9) are similar to each other

#2.Compute and visualise the similarity between the first 10 beers

similar_items <- similarity(beer_rating_matrix[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

#Beer ID's (5,9),(6,7),(7,5),(8,5),(9,7),(10,7),(11,13),(12,11) are similar to each other

#3.What are the unique values of ratings?

vector_ratings <- as.vector(beer_rating_matrix@data)
sort(unique(vector_ratings) )

# 0 1 2 3 4 5  are the unique values of rating

#The ratings are integers in the range 0-5. Let's count the occurrences of each of them. 
table_ratings <- table(vector_ratings) 
table_ratings
# vector_ratings

#      0        1        2        3        4        5 
# 22160533     3840    17729    95007   190807    16780 

# A rating equal to 0 represents a missing value, so we can remove them from vector_ratings:
  
vector_ratings <- vector_ratings[vector_ratings != 0] 

#Now, we can build a frequency plot of the ratings   

vector_ratings <- factor(vector_ratings) 

#Let's visualize the distribution using qplot

qplot(vector_ratings) + ggtitle("Distribution of the ratings")

#Most of the ratings are above 3 and the most common rating is 4

#Now  that we have got a sense of the distribution of ratings, lets proceed to the next question


#4 Visualise the rating values and notice:

#a.The average beer ratings

average_beer_ratings <- colMeans(beer_rating_matrix) 

average_beer_ratings

qplot(average_beer_ratings) + stat_bin(binwidth = 0.05) + ggtitle("Distribution of the average movie rating")

#The highest value lies in the bin of 3.8 to 4

beer_rating <- as.data.frame(group_by(beer_final_data,beer_beerid) %>% summarise(avg_beer_rating=mean(review_overall)))
summary(beer_rating)
# the average beer rating is 3.5


#b: The average user ratings
average_user_ratings <- rowMeans(beer_rating_matrix)
average_user_ratings
qplot(average_user_ratings)+ stat_bin(binwidth = 0.05) + ggtitle("Distribution of the average user rating")

user_rating <- group_by(beer_final_data,user_id) %>% summarise(avg_user_rating=mean(review_overall))

summary(user_rating$avg_user_rating)

#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.571   3.500   3.651   3.630   3.781   4.950 

#the average rating given to by a user is 3.6

hist(user_rating$avg_user_rating, breaks=100, col="red")

#c: The average number of ratings given to the beers

average_num_beer_ratings <- rowCounts(beer_rating_matrix)
qplot(average_num_beer_ratings)+ stat_bin(binwidth = 10) + ggtitle("Distribution of the average number of beer rating")

beer_no_of_ratings <- group_by(beer_final_data,beer_beerid) %>% summarise(no_of_ratings =n())
summary(beer_no_of_ratings)

#The average number of ratings for beers is 55
# beer_beerid   no_of_ratings   
# 5      :   1   Min.   :  7.00  
# 6      :   1   1st Qu.: 17.00  
# 7      :   1   Median : 29.00  
# 8      :   1   Mean   : 55.85  
# 9      :   1   3rd Qu.: 63.00  
# 10     :   1   Max.   :677.00  
# (Other):5798           

##Visualising the above

hist(beer_no_of_ratings$no_of_ratings, breaks=10, col="red") #skewed towards left

#d )The average number of ratings given by the users :

average_num_user_ratings <- colCounts(beer_rating_matrix)
qplot(average_num_user_ratings)+ stat_bin(binwidth = 10) + ggtitle("Distribution of the average number of rating by user")


#lets group by user ID's and summarise by number of ratings

user_no_of_ratings <- group_by(beer_final_data,user_id) %>% summarise(no_of_ratings =n())
summary(user_no_of_ratings)

#Average number of ratings is 83 per user

# user_id     no_of_ratings   
# 1      :   1   Min.   :  4.00  
# 2      :   1   1st Qu.: 30.00  
# 3      :   1   Median : 50.00  
# 4      :   1   Mean   : 83.68  
# 7      :   1   3rd Qu.: 98.00  
# 8      :   1   Max.   :990.00  
# (Other):3868          

hist(user_no_of_ratings$no_of_ratings, breaks=10, col="red") #skewed towards left

#---------------------------------------------------------------------------------------------
# Recommendation models
#---------------------------------------------------------------------------------------------

#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# names(recommender_models)# 9 types of models
# [1] "ALS_realRatingMatrix"          "ALS_implicit_realRatingMatrix"
# [3] "IBCF_realRatingMatrix"         "POPULAR_realRatingMatrix"     
# [5] "RANDOM_realRatingMatrix"       "RERECOMMEND_realRatingMatrix" 
# [7] "SVD_realRatingMatrix"          "SVDF_realRatingMatrix"        
# [9] "UBCF_realRatingMatrix"

#description of recommendation system algorithms/models used
lapply(recommender_models, "[[", "description")

#This gives you different types of recommendation models
#In this case study , let's compare user based and item based
#collaborative filtering

#checking the parameters of these two models
recommender_models$IBCF_realRatingMatrix$parameters

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#To further optimize the run time of the algorithm, lets filter by beer ratings >3, Users who have rated at least 25 beers and beers that has got 30 ratings  

beer_final_data <- beer_final_data[which(beer_final_data$review_overall >3),]
beer_rating_matrix <-  as(beer_final_data, "realRatingMatrix") #converting to real rating matrix
beer_rating_matrix <- beer_rating_matrix[rowCounts(beer_rating_matrix) >25,colCounts(beer_rating_matrix) >30 ]

#--------------------------------------------------------------------------------------------------------
#Objective is to find the best value of K and split to pick a model which has least RMSE value, as lower RMSE indicates a better model
#Lets iterate from K=1 to K=5 and split the data for training from 0.6 to 0.8 at intervals of 0.05 


#Initilizing blank DF for storing  accuracy measures for UBCF
error_UBCF <- data.frame()

#Initilizing blank DF for storing  accuracy measures for IBCF
error_IBCF <- data.frame()

#generating sequence for train split
train_split <- seq(0.6,0.80,by=0.05)

#generating sequnce for K 
k_split <- seq(1,5)



#Takes approximately 24.75466 mins to complete.
for (i in train_split){
  for (j in k_split){
    
    #Declaring the scheme
    set.seed(100)
    scheme <- evaluationScheme(beer_rating_matrix, method = "split", train = i,
                               k = j, given=12 , goodRating = 4)
 
    
    #--arguments
    #train and test
    #Here we create an evaluation scheme which splits the users 
    #into a training set and test set for values indicated by K
    
    #given 
    #For the test set items(given argument) will be given to the
    #recommender algorithm and the other items will be held out for computing the error
    
    #With goodRating=4 all items with actual user rating of greater or equal 4 are considered 
    #positives in the evaluation process
    
    
    #Building the recommendation models
    recom_UBCF<-Recommender(getData(scheme, "train"), "UBCF") 
    recom_IBCF <- Recommender(getData(scheme, "train"), "IBCF") 
    
    # Making the Predictions
    predict_UBCF <- predict(recom_UBCF, getData(scheme, "known"), type="ratings") 
    predict_IBCF<- predict(recom_IBCF, getData(scheme, "known"), type="ratings")
    
    #Evaluating the performance parameters
    error_recom_UBCF<-calcPredictionAccuracy(predict_UBCF, getData(scheme, "unknown"))
    error_recom_IBCF<-calcPredictionAccuracy(predict_IBCF, getData(scheme, "unknown"))
    
    #Storing the result in a dataframe
    error_UBCF1 <- cbind(data.frame(iteration=paste("split_",i,"k_",j)),t(data.frame(error_recom_UBCF)))
    error_UBCF <- rbind(error_UBCF1,error_UBCF)
    
    error_IBCF1 <- cbind(data.frame(iteration=paste("split_",i,"k_",j)),t(data.frame(error_recom_IBCF)))
    error_IBCF <- rbind(error_IBCF1,error_IBCF)
    
  }
}


#Resetting rownames
rownames(error_IBCF) <- NULL
rownames(error_UBCF) <- NULL

#Viewing the performance measures for both UBCF and IBCF
View(error_UBCF)
View(error_IBCF)

#Lets find whats the minimum RMSE value out of the 25 iterations and pick the
#iteration values which got the min value of RMSE
min(error_UBCF$RMSE)  # 0.2853344
error_UBCF$iteration[error_UBCF$RMSE==min(error_UBCF$RMSE)]  # split_ 0.6 k_ 3

min(error_IBCF$RMSE) # 0.4365818
error_IBCF$iteration[error_IBCF$RMSE==min(error_IBCF$RMSE)]  # split_ 0.6 k_5

#----------------------------------------------------------------------------------------
# Plotting ROC for both UBCF and IBCF
#-------------------------------------------------------------------------------------
#Now that we have got the thresholds for K and split values for both UBCF and IBCF,
#lets visualize the ROC plot by generating the algorithms for both UBCF and IBCF

#Lets define the scheme as below, i.e split=0.6 and k=3
set.seed(100)
scheme <- evaluationScheme(beer_rating_matrix, method = "split", train = .6,
                           k = 3, given=12 , goodRating = 4)
scheme

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=4)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

# run algorithms, predict next n beers

#Approximately takes 3.842059 mins for completion

results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))

class(results)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")

#Inference : Based on both the RMSE values yielded above and the ROC plot,UBCF is a better model than the IBCF for this dataset

#------------------------------------------------------------------------------------------------------------
#Identifying the user ID's for user names , as we have generated unique ID for each unique profile name
#The objective of the assignment is to recommend top 5 beers to users "cokes", "genog" & "giblet"

profilename_vec <- c("cokes","genog","giblet") #vector containing the 3 users to whom we are suggesting

reviewer_id <- c()
for (i in 1:length(profilename_vec)){
  
  reviewer_id<- c(reviewer_id,beer_raw_data$user_id[beer_raw_data$review_profilename == profilename_vec[i]][1])
}

reviewer_id
#[1]  440 1092  747, i.e cokes=440,genog=1092,giblet=747


#-----------------------------------------------------------------------------------------------------------------------
#Prediction phase
#-----------------------------------------------------------------------------------------------------------------------

#1. Lets build individual recommendation models for UBCF and IBCF based on the best cut off values obtained above
#2. Predict top 5 beer's to the users using both UBCF and IBCF

#--------------------------------------------------------------------------------------------------------
#Predicting using the UBCF
#--------------------------------------------------------------------------------------------------------
#Lets build the UBCF with split=0.6 and  k= 3

#Lets make use of the same scheme used for generating ROC as it as the same parameters, i.e split=0.75 and k=4

recom_UBCF<-Recommender(getData(scheme, "train"), "UBCF")

#Making predictions using UBCF
# recommending top 5 items for for cokes , user ID=440
recommended.items.user440 <- predict(recom_UBCF, beer_rating_matrix["440",], n=5)

# to display them
as(recommended.items.user440, "list")
#$`440`
#[1] "8787"  "44531" "27804" "857"   "2204" 

# recommending top 5 items for for genog , user ID =1092
recommended.items.user1092 <- predict(recom_UBCF, beer_rating_matrix["1092",], n=5)

# to display them
as(recommended.items.user1092, "list")
#$`1092`
#[1]  "19960" "29127" "645"   "21690" "16814"

# recommending top 5 items for for giblet , user ID =747
recommended.items.user747 <- predict(recom_UBCF, beer_rating_matrix["747",], n=5)

# to display them
as(recommended.items.user747, "list")
# $`747`
# [1] "5"  "6"  "7"  "10" "14"

#Now that we have recommended the top 5 beers to the 3 specified users using UBCF using the best threshold parameters
# of k and split, lets proceed to see what the recommendations look like using the IBCF
#We will do a similar exercise as the above

#--------------------------------------------------------------------------------------------------------
#Predicting using the IBCF
#--------------------------------------------------------------------------------------------------------

#Lets build the IBCF with split=0.6 and  k= 5

#Lets generate a scheme for the above parameters
set.seed(100)
scheme <- evaluationScheme(beer_rating_matrix, method = "split", train = .6,
                           k = 5, given=12 , goodRating = 4)

#Building recommender model using IBCF

recom_IBCF <- Recommender(getData(scheme, "train"), "IBCF") 

#Making predictions using UBCF
# recommending top 5 items for for cokes , user ID=440
recommended.items.user440_ibcf <- predict(recom_IBCF, beer_rating_matrix["440",], n=5)

# to display them
as(recommended.items.user440_ibcf, "list")
#$`440`
#[1] "298"  "705"  "1011" "1397" "1745"

# recommending top 5 items for for genog , user ID =1092
recommended.items.user1092_ibcf <- predict(recom_IBCF, beer_rating_matrix["1092",], n=5)

# to display them
as(recommended.items.user1092_ibcf, "list")
#$`1092`
#[1] "74"  "225" "634" "650" "691"


# recommending top 5 items for for giblet , user ID =747
recommended.items.user747_ibcf <- predict(recom_IBCF, beer_rating_matrix["747",], n=5)

# to display them
as(recommended.items.user747_ibcf,"list")
# $`747`
# character(0) implying user is not found in matrix


#----------------------- R ANALYSIS ENDS HERE------------------------------------------------------

#--------------------------------------------------------------------------------------------------
#SUMMARY
#--------------------------------------------------------------------------------------------------

#Overall methodology of solving this assignment can be summarised in following steps:
#1. Importing the dataset
#2. Data cleaning : checking for blanks values, NA's 
#3. Aggregating the data and filtering based on "N", the threshold for beerID's and Users with "N" number of reviews
#4. Answering the data exploration questions after converting the dataframe to realratingmatrix
#5. Filtering on beer ID' with ratings < 3 and rowCount and colCount in matrix
#6. Experimenting with different values of k and split and building the performance metrix for bothe UBCF and IBCF
#7. Visualizing IBCF and UBCF based ROC
#8. Building individual recommender models based on UBCF and IBCF based on the optimal thresholds of k and split based schemes
#9. Predicting top 5 items for specified users using both UBCG and IBCF

#Conclusion:
#Both visually and based on the RMSE metric the UBCF model outperforms the IBCF model
#Based on the evidence presented through the analysis , kindly recommend the top 5 beers to the users based on UBCF model, as a first priority. There are also recommendations provided using IBCF.

#for user coke: recommend the beer with following ID's
# "8787"  "44531" "27804" "857"   "2204" 

#for user genog : recommend the beer with following ID's
#  "19960" "29127" "645"   "21690" "16814" 

# for user giblet : recommend the beer with following ID's
# "5"  "6"  "7"  "10" "14"

########################################################################################################################################################################