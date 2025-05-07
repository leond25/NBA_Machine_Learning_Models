#########################################################
#
# Welcome to our CS438 big data project!
#
# We will be working with player statistics in the 
# modern era of the NBA
#
# Authors: Santiago, Bryan, and David
#
#########################################################
install.packages("dplyr")  # Install if you haven't already
library(dplyr)

nba_data = read.csv("player_stats.csv", header=T)    # importing the dataset
nba_data = na.omit(nba_data)    # removes all rows with missing data (NA)
attach(nba_data)

TOV <- TOV / GP
FGA <- FGA / GP
FTA <- FTA / GP
STL <- STL / GP
BLK <- BLK / GP


nba_data$POS <- sub("-.*", "", nba_data$POS) # deletes the second position if a player has 2 positions
# code to test single position
#unique_positions <- unique(nba_data$POS)
#num_positions <- length(unique_positions)
#print(num_positions)


############################################################
#
# Prediction Model #1
# 
# This model will predict the number of fouls a player commits
# based on turnovers and the number of shot attempts
#
# Author: Bryan
#
############################################################

# set up the training and test sets
set.seed(1)

index = sample(1:nrow(nba_data), size = 0.7 * nrow(nba_data))
nba_train = nba_data[index, ]
nba_test = nba_data[-index, ]

# create the linear model based on the training set
lm.fouls = lm(PF~ TOV + STL + BLK + GP, data = nba_train)
summary(lm.fouls)

# create a prediction
pred.fouls = predict(lm.fouls, newdata = nba_test)

comp = data.frame(
  pred.fouls,
  nba_test$PF
)
print(comp)

# MSE calculation
true.fouls <- nba_test$PF
mse <- mean((pred.fouls - true.fouls)^2)
print(paste("Mean Squared Error:", round(mse, 2)))

res = residuals(lm.fouls)
hist(res, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", breaks = 20)

############################################################
#
# Prediction Model #2
# 
# This model will predict an NBA player's field goal percentage 
#
# Author: David 
#
############################################################
library(tree)

# Set a random seed
set.seed(7)

# Create training and test sets
index_train <- sample(1:nrow(nba_data), size = 0.7 * nrow(nba_data))
nba_train2 <- nba_data[index_train, ]
nba_test2 <- nba_data[-index_train, ]

# Remove unwanted columns
nba_train2 <- nba_train2 %>% select(-POS, -TEAM, -EXP, -FGM, -FGA)
nba_test2 <- nba_test2 %>% select(-POS, -TEAM, -EXP, -FGM, -FGA)

# Train the tree model
tree.FG_Perc <- tree(FG_PERCENT ~ ., data = nba_train2)

# make predictions using the tree
yhat = predict(tree.FG_Perc,newdata=nba_test)

# take in the real results 
tree.FG_Perc_test=nba_test$FG_PERCENT

# calculate the mean squared error
tree_mse <- mean((yhat-tree.FG_Perc_test)^2)

