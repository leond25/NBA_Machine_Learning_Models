#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tree) # do not remove this. this is needed for printing out my tree
library(dplyr)
library(ggplot2)

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
#install.packages("dplyr")  # Install if you haven't already


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

##############################################################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dream Team"),

    tabsetPanel(
      tabPanel(
        ##
        ##  Tab #1: Predicting Fouls
        ##  UI
        ##
        ##
        ##
        
        "Predict Fouls",
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            numericInput("tov", "Average Turnovers (TOV):", value = 10, min = 0),
            numericInput("stl", "Average Steals (STL):", value = 2, min = 0),
            numericInput("blk", "Average Blocks (BLK):", value = 1, min = 0),
            numericInput("gp", "Total Number of Games Played", value = 1, min = 0),
            actionButton("predBtn", "Predict Fouls")
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            h3("Draft Kings: +/- Personal Fouls"),
            verbatimTextOutput("prediction"),
            plotOutput("resultPlot")
          )
        ) 
      ),
      
      ##
      ##  Tab #2: 
      ##
      ##
      ##
      ##
      
      tabPanel(
        "NBA Player's FG% Predictor",
        # Sidebar layout with user inputs
        sidebarLayout(
          sidebarPanel(
            numericInput("Age", "Player Age:", value = 25, min = 18),
            numericInput("GamesPlayed", "Total Games Played:", value = 30, min = 0),
            numericInput("Minutes", "Total Minutes Played:", value = 30, min = 0),
            numericInput("threeMA", "Total Three-Pointers Made:", value = 50, min = 0),
            numericInput("threePA", "Total Three-Pointers Attempts:", value = 100, min = 0),
            numericInput("FreeThrowMade", "Total Free Throws Made:", value = 80, min = 0),
            numericInput("FreeThrowAttempted", "Total Free Throws Attempted:", value = 100, min = 0),
            numericInput("Assists", "Total Assists:", value = 30, min = 0),
            numericInput("Steals", "Total Steals:", value = 30, min = 0),
            numericInput("Blocks", "Total Blocks:", value = 30, min = 0),
            numericInput("Turnovers", "Total Turnovers:", value = 50, min = 0),
            numericInput("PersonalFouls", "Total Personal Fouls:", value = 20, min = 0),
            numericInput("TotalPoints", "Total Points:", value = 300, min = 0),
            actionButton("predictBtn", "Predict Player's FG%")
          ),
          mainPanel(
            h3("FG % Tree"),
            plotOutput("treePlot"),
            uiOutput("predictedFG"),
            plotOutput("PredictedVSActual"),
            uiOutput("MeanSquaredError")
          )
        )
      ),
      
      tabPanel(
        ##
        ##  Tab #3: "Type of Player" Archetype Clustering
        ##  UI
        ##
        ##
        ##
        
        "'Type of Player' Archetype Clustering",
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            sliderInput("k", "Number of Clusters:", 
                        min = 5, max = 15, value = 10, step = 1)
          ),
          mainPanel(
            plotOutput("clusterPlot"),
            verbatimTextOutput("metrics"),
            verbatimTextOutput("centers")
          )
        ) 
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ##
    ##  Tab #1: Predicting Fouls
    ##  Logic, Buttons, and Results
    ##
    ##
    ##
    observeEvent(input$predBtn, {
      user_data <- data.frame(
        TOV = input$tov,
        STL = input$stl,
        BLK = input$blk,
        GP = input$gp
      )
      
      predicted_fouls = predict(lm.fouls, newdata = user_data)
      
      # Output the prediction
      output$prediction <- renderPrint({
        paste("Predicted number of your player personal fouls:", round(predicted_fouls, 2))
      })
      
      output$resultPlot <- renderPlot({
        # Plot the distribution of actual fouls
        plot(nba_train$GP, nba_train$PF,
             xlab = "Games Played",
             ylab = "Personal Fouls",
             main = "How does you prediction compare?",
             pch = 16, col = "blue")
        
        # add the prediction
        points(user_data$GP, predicted_fouls,
               col = "red", pch = 19, cex = 1.5)
      })
    })
  
  ##
  ##  Tab #2: Predicting a player's FG perc
  ##  Logic, Buttons, and Results
  ##
  ##
  ##
  
  # Render the tree plot
  output$treePlot <- renderPlot({
    #  plot(tree.FG_Perc)
    #  text(tree.FG_Perc, pretty = 0)
    plot(tree.FG_Perc, col = "orange", main = "Decision Tree for a player's FG%", cex = 1.2)
    text(tree.FG_Perc, pretty = 0, col = "black", cex = 1)
    
  })
  
  # Plot feature importance
  output$PredictedVSActual <- renderPlot({
    plot(yhat,tree.FG_Perc_test, main = "Predicted vs Actual Values", xlab = "Predicted", ylab = "Actual")
    abline(0,1)
  })
  
  # Output the mean squared error
  output$MeanSquaredError <- renderUI({
    HTML(paste("<div style='font-size:24px; font-weight:bold;'>Mean Squared Error:", tree_mse, "</div>"))
  })
  
  # Observe event for prediction
  observeEvent(input$predictBtn, {
    # Create a new data frame based on user inputs
    user_data <- data.frame(
      AGE = input$Age,
      GP = input$GamesPlayed,
      MIN = input$Minutes,
      x3PM = input$threeMA,
      x3PA = input$threePA,
      x3P_PERCENT = ((input$threeMA / input$threePA) * 100),
      FTM = input$FreeThrowMade,
      FTA = input$FreeThrowAttempted,  
      FT_PERCENT = ((input$FreeThrowMade / input$FreeThrowAttempted) * 100),
      AST = input$Assists,
      STL = input$Steals,
      BLK = input$Blocks,
      TOV = input$Turnovers,
      PF = input$PersonalFouls,
      PTS = input$TotalPoints
    )
    
    
    # Predict FG% using the trained tree model
    predicted_fg <- predict(tree.FG_Perc, newdata = user_data)
    
    # Output the predicted FG%
    #output$predictedFG <- renderText({
    # paste("Predicted FG%:", round(predicted_fg, 2))
    #  })
    
    # Output the predicted FG% with increased size
    output$predictedFG <- renderUI({
      HTML(paste("<div style='font-size:24px; font-weight:bold;'>Predicted FG%:", round(predicted_fg, 2), "</div>"))
    })
    
  })
  
  ##
  ##  Tab #3: Predicting a player's FG perc
  ##  Logic, Buttons, and Results
  ##
  ##
  ##
  nba_data = read.csv("player_stats.csv", header=T)    # importing the dataset
  nba_data = na.omit(nba_data)    # removes all rows with missing data (NA)
  attach(nba_data)
  
  # Reactive expression to run k-means whenever input$k changes
  clusters <- reactive({
    set.seed(123) # reproducibility
    kmeans(
      nba_data[, c("PTS", "AST", "BLK", "FGA", "FGM")],
      centers = input$k
    )
  })
  
  # Reactive data frame that adds the cluster assignments and reorder them by size
  reactive_data <- reactive({
    df <- nba_data
    clust <- clusters()
    df$cluster <- as.factor(clust$cluster)
    
    # Reorder factor levels by cluster size (smallest to largest)
    cluster_sizes <- table(df$cluster)
    ordered_levels <- names(sort(cluster_sizes))
    df$cluster <- factor(df$cluster, levels = ordered_levels)
    
    df
  })
  
  # MSE and RMSE
  output$metrics <- renderPrint({
    clust <- clusters()
    mse <- mean(clust$withinss) / length(reactive_data()$cluster)
    rmse <- sqrt(mse)
    cat("Mean Squared Error (MSE):", mse, "\n")
    cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  })
  
  # Print cluster centers
  output$centers <- renderPrint({
    clust <- clusters()
    cat("Cluster Centers:\n")
    print(clust$centers)
  })
  
  output$clusterPlot <- renderPlot({
    df <- reactive_data()
    clust <- clusters()
    
    # Reorder the centers to match the cluster order
    cluster_sizes <- table(df$cluster)
    ordered_levels <- names(sort(cluster_sizes))
    ordered_levels_num <- as.numeric(ordered_levels)
    
    centers <- as.data.frame(clust$centers)
    # Reorder rows of centers by cluster size
    centers <- centers[ordered_levels_num, , drop = FALSE]
    centers$cluster <- ordered_levels
    
    ggplot(df, aes(x = PTS, y = AST, color = cluster)) +
      geom_point() +
      # Add X marks at cluster centers
      geom_point(data = centers, aes(x = PTS, y = AST), 
                 shape = 4, size = 5, color = "black", stroke = 2) +
      labs(
        title = "Player Clusters based on Performance Metrics",
        x = "Points per Season",
        y = "Assists per Season"
      ) +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
