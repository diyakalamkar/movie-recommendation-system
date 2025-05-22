#movie recommendation system using r

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
setwd("C:/Users/dell/Downloads/IMDB-Dataset") #folder containing data is set as current working directory
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data) #displays info abt movie_data dataframe
summary(movie_data)
head(movie_data)
summary(rating_data)
head(rating_data) #userid and movieid are integers

movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE) 
#extracts the genres column from movie_data and converts it into a data frame
#stringsAsFactors=FALSE ensures that genres are treated as strings, not factors

movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
#splits the genre strings in each row of movie_genre into multiple columns wherever there's a |
#"Action|Adventure|Fantasy" becomes "Action", "Adventure", "Fantasy" in separate columns
#tstrsplit() does this row-wise; type.convert=TRUE converts character columns into numeric or logical where applicable

colnames(movie_genre2) <- c(1:10)
#renames the columns of movie_genre2 to numbers 1 through 10, assuming that no movie has more than 10 genres

list_genre <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", 
                "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical",
                "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
#lists all possible genres as a reference for one-hot encoding
#one-hot encoding is a method for converting categorical data into a format suitable for machine learning models 
#it transforms each category into a unique binary column, where 1 is presence of that category and 0 is absence

genre_mat1 <- matrix(0, 10330, 18) #initializes a matrix of size 10330 x 18 with all values set to 0
#10330 is the no of movies in the dataset and 18 is the number of unique genres

genre_mat1[1,] <- list_genre #stores the genre names as the first row of the matrix (temporary)

colnames(genre_mat1) <- list_genre #sets the column names of genre_mat1 to the genre names

for(index in 1:nrow(movie_genre2)){
  for(col in 1:ncol(movie_genre2)){
    gen_col = which(genre_mat1[1,]==movie_genre2[index,col])
    genre_mat1[index+1,gen_col] <- 1
  }
} #one hot encoding loop
#loops through each genre value in movie_genre2
#which(genre_mat1[1,] == movie_genre2[index,col]): finds the column index where the genre matches
#genre_mat1[index+1, gen_col] <- 1: sets 1 in the matching genre column for that movie (offset by 1 due to the header in row 1)

genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE)
#Removes the first row (which was genre names) to keep only the data
#Converts the matrix into a data frame

for (col in 1:ncol(genre_mat2)){
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}
#converts all columns from characters to integers (i.e., "0"/"1" as string → 0/1 as integer)

str(genre_mat2)
#displays the structure of genre_mat2, confirming it is a data frame with integer columns for each genre

SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])
#combines the first two columns of movie_data (MovieID and Title) with the genre information stored in genre_mat2
#cbind() binds columns together to form a new matrix/dataframe.
#genre_mat2[] retrieves all columns and rows from genre_mat2.

head(SearchMatrix)
#search matrix allows us to perform an easy search of the films by specifying the genre present in our list

#for movie recommendation system to make sense of our ratings through recommenderlabs-convert matrix into a sparse matrix one
#this new matrix is of the class ‘realRatingMatrix’
ratingMatrix <- dcast(rating_data, userId~movieId, value.var="rating", na.rm=FALSE)
#converts rating_data (likely in long format: userId, movieId, rating) into a wide format where:
#Rows = users, Columns = movies, Cells = rating values.
#dcast(): From the reshape2 package, reshapes the data.
#rana.rm=FALSE: Keeps NAs for missing ratings, important for sparse matrix use.

ratingMatrix <- as.matrix(ratingMatrix[,-1]) #removes userIds

#convert rating matrix into a recommenderlab sparse matrix
ratingMatrix<-as(ratingMatrix,"realRatingMatrix")
ratingMatrix #cli

#retrieves all registered recommendation models that can work with a realRatingMatrix
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
#lists the names of all available recommendation algorithms like UBCF, IBCF
names(recommendation_model) #cli

#prints a description of each recommendation method
lapply(recommendation_model, "[[", "description")

#displays the parameters specific to the Item-Based Collaborative Filtering (IBCF) model for a realRatingMatrix
recommendation_model$IBCF_realRatingMatrix$parameters #cli

#Collaborative Filtering involves suggesting movies to the users that are based on collecting preferences from many other users. 
# if a user A likes to watch action films and so does user B, then the movies that the user B will watch in the future will be recommended to A and vice-versa
# therefore, recommending movies is dependent on creating a relationship of similarity between the two users 
# with the help of recommenderlab, we can compute similarities using various operators like cosine, pearson as well as jaccard
#here each row and column is a user, so we have 4 users and their similarity
similarity_mat <- similarity(ratingMatrix[1:4,], method="cosine", which="users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main="user's similarities")

#same for movies
movie_similarity <- similarity(ratingMatrix[,1:4], method="cosine", which="items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main="movies' similarities")

#extract the most unique ratings
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) #cli

#create a table of ratings that will display the most unique ratings
Table_Of_Ratings <- table(rating_values)
Table_Of_Ratings #cli

#calculates how many ratings (non-zero values) each movie has received from users
movie_views <- colCounts(ratingMatrix) #count the views for each
table_views <- data.frame(movie=names(movie_views), views=movie_views) #create a dataframe of views
table_views <- table_views[order(table_views$views, decreasing=TRUE),] #sort by no of views descending order
#adds a new column title to table_views, initializing it with NA (missing values)
#this will be filled in the next step with actual movie titles from the movie_data dataframe
table_views$title <- NA
for (index in 1:10325){
  table_views[index,3] <- as.character(subset(movie_data, movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,] #cli

#visualizing the total no of views of the top films
print(ggplot(table_views[1:6,] , aes(x=title,y=views)) + 
        geom_bar(stat="identity", fill='purple') + 
        geom_text(aes(label=views), vjust=-0.3, size=3.5) +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        ggtitle("Total Views of the Top Films"))

#heatmap of movie ratings (first 25 rows and 25 columns)
print(image(ratingMatrix[1:25, 1:25], axes=FALSE, main="Heatmap of Movie Ratings (first 25 rows and 25 columns)"))

#performing data preparation
#filtering most-watched films from least-watched ones (selecting the data)
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix)>50, colCounts(ratingMatrix)>50]
print(movie_ratings)

#delineating the matrix
minimum_movies <- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
print(image(movie_ratings[ rowCounts(movie_ratings) > minimum_movies,
                           colCounts(movie_ratings) > minimum_users],
            main="Heatmap of the top movies and users"))

#visualising the average ratings per user
average_ratings <- rowMeans(movie_ratings)
print(qplot(average_ratings, fill=I("pink"), col=I("maroon")) + ggtitle("Distribution of Average Rating per User"))

#normalisation of data to remove bias
normalised_ratings = normalize(movie_ratings)
sum(rowMeans(normalised_ratings) > 0.00001)
print(image(normalised_ratings[ rowCounts(normalised_ratings) > minimum_movies,
                                colCounts(normalised_ratings) > minimum_users],
            main="Normalised ratings of top movies and users"))

#performing data binarization
#means that we have two discrete values 1 and 0, which will allow our recommendation systems to work more efficiently
#we will define a matrix that will consist of 1 if the rating is above 3 and otherwise it will be 0
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95) 
#determines a threshold to filter the top 5% most-rated movies
#rowCounts(movie_ratings) -> gets the number of ratings each movie has received
#quantile(..., 0.95) -> gets the 95th percentile — i.e., the value above which the top 5% of movies lie
#so binary_minimum_movies stores the number of ratings that only the top 5% of movies have or exceed

binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
#same as above, but for users instead of movies
#colCounts(movie_ratings) gets the number of ratings made by each user
#quantile(..., 0.95) calculates the threshold for the top 5% most active users
#result is stored in binary_minimum_users

good_rated_films <- binarize(movie_ratings, minRating=3)
#converts the rating matrix into a binary matrix where ratings ≥ 3 become 1, and the rest become 0
#binarize(...) is from the recommenderlab package (used for recommendation systems)
#minRating = 3 means ratings greater than or equal to 3 are marked as 1 (liked), others as 0 (not liked).
#makes the data simpler and more efficient for collaborative filtering and recommendation algorithms

print(image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies, 
                       colCounts(movie_ratings) > binary_minimum_users], 
                        main="Heatmap of the top users and movies"))
#creates a heatmap showing which top users liked which top movies

#creating item based collaborative filtering system
#finds similarity in the items based on the people’s ratings of them
#if two movies tend to be liked or disliked by the same users, they’re considered similar
#algorithm first builds a similar-items table of the customers who have purchased them into a combination of similar items 
#this is then fed into the rec system
sampled_data <- sample(x=c(TRUE, FALSE), 
                       size=nrow(movie_ratings), 
                       replace=TRUE, 
                       prob=c(0.8, 0.2))
#randomly splits the data into training (80%) and testing (20%) sets
#sample(...) randomly chooses TRUE (80%) or FALSE (20%) for each row (i.e., each user)
#nrow(movie_ratings) means we do this for every user in the dataset
#replace=TRUE allows sampling with replacement
#prob=c(0.8, 0.2) sets the probabilities

training_data <- movie_ratings[sampled_data,]
#selects the 80% of users where sampled_data is TRUE
#trains the rec model

testing_data <- movie_ratings[!sampled_data,]
#selects the remaining 20% of users for testing (where sampled_data is FALSE)
#evaluates the rec system's accuracy

#building the rec system
recommendation_system <- recommenderRegistry$get_entries(dataType="realRatingMatrix")
#retrieves all available recommendation algorithms that work with realRatingMatrix data
#recommenderRegistry is a list of registered recommendation algorithms
#dataType="realRatingMatrix" filters it to models that use actual rating values (not binary)

print(recommendation_system$IBCF_realRatingMatrix$parameters)
#displays the default parameters for the IBCF (Item-Based Collaborative Filtering) model
#IBCF_realRatingMatrix refers to the item-based model for real-valued ratings

recommend_model <- Recommender(data=training_data, method="IBCF", parameter=list(k=30))
#builds the item-based recommender using training data
#method="IBCF" specifies the item-based collaborative filtering algorithm.
#parameter=list(k=30) -> sets k = 30, meaning it considers the top 30 most similar items when generating recommendations

print(recommend_model)
print(class(recommend_model))

model_info <- getModel(recommend_model)
#extracts the internal model data (like the similarity matrix) from the trained recommender
#getModel() gives access to more technical info inside the model

print(class(model_info$sim))
print(dim(model_info$sim))
#item-item similarity matrix

top_items <- 20
#defines how many top items to visualize in the heatmap

print(image(model_info$sim[1:top_items, 1:top_items], main="Heatmap of the first rows and columns"))
#visualizes the similarity between the top 20 items using a heatmap
#each square in the heatmap represents how similar two items are
#white = more similar
#black = less or no similarity

#analysing similarity matrix
sum_rows <- rowSums(model_info$sim > 0)
#counts how many non-zero similarities each item has with other items
#model_info$sim > 0 gives a logical matrix where TRUE means there's some similarity
#rowSums(...) adds up TRUE values for each row (i.e., item)
#so sum_rows[i] = number of other items that item i is similar to

table(sum_rows)
#shows a frequency distribution of how many items have a given number of similar items

sum_cols <- colSums(model_info$sim > 0)
#same idea, but now counting how many items consider each item as similar
#this is the column-wise sum of non-zero similarities

print(qplot(sum_cols, fill=I("lightblue"), col=I("purple"))+ ggtitle("Distribution of the column count"))
#plots a histogram showing the distribution of sum_cols (how many times each item is similar to others)

#building the system
#create a top_recommendations variable which will be initialized to 10, specifying the number of films to each user
#use the predict() function that will identify similar items and will rank them appropriately
#each rating is used as a weight
#each weight is multiplied with related similarities
#everything is added in the end
top_recommendations <- 10
predicted_recommendations <- predict(object=recommend_model, newdata=testing_data, n=top_recommendations)
print(predicted_recommendations)

user1 <- predicted_recommendations@items[[1]] #recommendation for the first user
#gets the indices of the recommended movies for the first user
#@items gives the raw item indices (not movie titles yet)

movies_user1 <- predicted_recommendations@itemLabels[user1]
#converts those indices to the actual item labels (movie IDs or column names)

movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data, movie_data$movieId == movies_user1[index])$title)
}
#converts movie IDs to movie titles
#subset(...) finds the movie title where movieId matches
#this loop replaces the IDs with readable movie titles for display

print(movies_user2)
#prints the final list of 10 recommended movie titles for the first user

recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) 
#matrix with the recommendations for each user
print(recommendation_matrix[,1:4])
#prints the recommended movie IDs for the first 4 users
