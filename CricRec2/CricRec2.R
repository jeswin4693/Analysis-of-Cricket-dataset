# Load required libraries
library(rpart)
library(rpart.plot)

# Read the data
cricket_data <- read.csv("FieldingMostCatches.csv", stringsAsFactors = FALSE)



View(cricket_data)
# Function to extract team from player string
get_team <- function(player_string) {
  team <- gsub(".*\\((.*)\\).*", "\\1", player_string)
  return(team)
}

# Data preprocessing
cricket_data$Team <- sapply(cricket_data$Player, get_team)
cricket_data$Catching_Efficiency <- cricket_data$Ct/cricket_data$Inns
cricket_data$Catch_Category <- cut(cricket_data$Catching_Efficiency,
                                   breaks = c(0, 0.3, 0.6, 0.9, Inf),
                                   labels = c("Low", "Medium", "High", "Exceptional"))

# Create separate datasets for each team
ind_data <- subset(cricket_data, Team == "IND")
aus_data <- subset(cricket_data, Team == "AUS")
wi_data <- subset(cricket_data, Team == "WI")
nz_data <- subset(cricket_data, Team == "NZ")
sa_data <- subset(cricket_data, Team == "SA")
pak_data <- subset(cricket_data, Team == "PAK")
ban_data <- subset(cricket_data, Team == "BAN")

# Function to create and plot decision tree
create_team_tree <- function(data, team_name) {
  # Create tree model
  tree_model <- rpart(Catch_Category ~ Mat + Inns + Ct + Max,
                      data = data,
                      method = "class",
                      control = rpart.control(minbucket = 2))
  
  # Plot tree
  rpart.plot(tree_model,
             box.palette = "Blues",
             shadow.col = "gray",
             main = paste("Decision Tree -", team_name),
             extra = 101,
             fallen.leaves = TRUE,
             branch = 0.3,
             digits = 3)
  
  return(tree_model)
}

# Set up plotting layout for all trees
par(mfrow = c(3, 3), mar = c(1, 1, 2, 1))

# Create trees for each team
# India
print("India Team Analysis")
ind_tree <- create_team_tree(ind_data, "India")
ind_stats <- ind_data[order(ind_data$Catching_Efficiency, decreasing = TRUE), ]
print(head(ind_stats[, c("Player", "Mat", "Ct", "Catching_Efficiency")]))

# Australia
print("Australia Team Analysis")
aus_tree <- create_team_tree(aus_data, "Australia")
aus_stats <- aus_data[order(aus_data$Catching_Efficiency, decreasing = TRUE), ]
print(head(aus_stats[, c("Player", "Mat", "Ct", "Catching_Efficiency")]))

# West Indies
print("West Indies Team Analysis")
wi_tree <- create_team_tree(wi_data, "West Indies")
wi_stats <- wi_data[order(wi_data$Catching_Efficiency, decreasing = TRUE), ]
print(head(wi_stats[, c("Player", "Mat", "Ct", "Catching_Efficiency")]))

# New Zealand
print("New Zealand Team Analysis")
nz_tree <- create_team_tree(nz_data, "New Zealand")
nz_stats <- nz_data[order(nz_data$Catching_Efficiency, decreasing = TRUE), ]
print(head(nz_stats[, c("Player", "Mat", "Ct", "Catching_Efficiency")]))

# South Africa
print("South Africa Team Analysis")
sa_tree <- create_team_tree(sa_data, "South Africa")
sa_stats <- sa_data[order(sa_data$Catching_Efficiency, decreasing = TRUE), ]
print(head(sa_stats[, c("Player", "Mat", "Ct", "Catching_Efficiency")]))

# Pakistan
print("Pakistan Team Analysis")
pak_tree <- create_team_tree(pak_data, "Pakistan")
pak_stats <- pak_data[order(pak_data$Catching_Efficiency, decreasing = TRUE), ]
print(head(pak_stats[, c("Player", "Mat", "Ct", "Catching_Efficiency")]))

# Bangladesh
print("Bangladesh Team Analysis")
ban_tree <- create_team_tree(ban_data, "Bangladesh")
ban_stats <- ban_data[order(ban_data$Catching_Efficiency, decreasing = TRUE), ]
print(head(ban_stats[, c("Player", "Mat", "Ct", "Catching_Efficiency")]))

# Create summary statistics for all teams
team_summary <- data.frame(
  Team = c("IND", "AUS", "WI", "NZ", "SA", "PAK", "BAN"),
  Avg_Efficiency = c(
    mean(ind_data$Catching_Efficiency),
    mean(aus_data$Catching_Efficiency),
    mean(wi_data$Catching_Efficiency),
    mean(nz_data$Catching_Efficiency),
    mean(sa_data$Catching_Efficiency),
    mean(pak_data$Catching_Efficiency),
    mean(ban_data$Catching_Efficiency)
  ),
  Max_Efficiency = c(
    max(ind_data$Catching_Efficiency),
    max(aus_data$Catching_Efficiency),
    max(wi_data$Catching_Efficiency),
    max(nz_data$Catching_Efficiency),
    max(sa_data$Catching_Efficiency),
    max(pak_data$Catching_Efficiency),
    max(ban_data$Catching_Efficiency)
  ),
  Total_Players = c(
    nrow(ind_data),
    nrow(aus_data),
    nrow(wi_data),
    nrow(nz_data),
    nrow(sa_data),
    nrow(pak_data),
    nrow(ban_data)
  )
)

# Print team summary
print("Team Summary Statistics:")
print(team_summary)


# Print key findings
cat("\nKey Findings:\n")
cat("1. Team with highest average catching efficiency:", 
    team_summary$Team[which.max(team_summary$Avg_Efficiency)], "\n")
cat("2. Team with highest maximum catching efficiency:", 
    team_summary$Team[which.max(team_summary$Max_Efficiency)], "\n")
cat("3. Team with most players analyzed:", 
    team_summary$Team[which.max(team_summary$Total_Players)], "\n")




------------------------------------------------------------------------------------------------------------
  
  # Load necessary libraries
  library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("FieldingMostCatches.csv")

# Data Preprocessing
# Filter the data for players from 1996 to 2016 and select the top 10 based on catches
top_players <- data %>%
  filter(Span >= 1996 & Span <= 2016) %>%  # Filter for years 1996 to 2016
  arrange(desc(Ct)) %>%                    # Sort by number of catches
  slice(1:10)                              # Select top 10 players

# Normalize numerical columns for K-means clustering
numerical_data <- top_players[, c("Mat", "Inns", "Ct", "Max", "Ct.Inn")] # Adjust this based on your actual columns
numerical_data <- scale(numerical_data)  # Normalize the data

# Apply K-means clustering
set.seed(123) # Set seed for reproducibility
k <- 3 # Choose the number of clusters (you can adjust this)
kmeans_result <- kmeans(numerical_data, centers = k)

# Add cluster assignment to the top_players data frame
top_players$Cluster <- as.factor(kmeans_result$cluster)

# Create a data frame for plotting with player names and team information
plot_data <- as.data.frame(numerical_data)
plot_data$Player <- top_players$Player
plot_data$Cluster <- top_players$Cluster

# Use meaningful names for the plot
colnames(plot_data) <- c("Mat", "Inns", "Ct", "Max", "Ct.Inn", "Player", "Cluster")

# Plot the top 10 players with ggplot2, coloring by cluster
ggplot(plot_data, aes(x = Ct, y = Mat, color = Cluster, label = Player)) +
  geom_point(size = 4) +
  geom_text(hjust = 0.5, vjust = -1.5, size = 3, check_overlap = TRUE) +
  labs(title = "K-Means Clustering of Top 10 Players with Most Catches (1996-2016)",
       x = "Catches (Ct)",
       y = "Matches Played (Mat)") +
  theme_minimal() +
  theme(legend.position = "bottom")
