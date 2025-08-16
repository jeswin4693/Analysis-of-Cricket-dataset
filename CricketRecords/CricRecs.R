# Load necessary libraries
library(dplyr)
library(psych)

# Load the data
data <- read.csv("BowlingMostWicket.csv", header = TRUE, stringsAsFactors = FALSE)

# Rename the columns for clarity
colnames(data) <- c("Player", "Span", "Matches", "Innings", "Balls", "Overs", "Maidens", "Runs", "Wickets", 
                    "BBI", "Average", "Economy", "StrikeRate", "4Wickets", "5Wickets")

# Convert necessary columns to numeric
data$Matches <- as.numeric(data$Matches)
data$Innings <- as.numeric(data$Innings)
data$Balls <- as.numeric(data$Balls)
data$Overs <- as.numeric(data$Overs)
data$Maidens <- as.numeric(data$Maidens)
data$Runs <- as.numeric(data$Runs)
data$Wickets <- as.numeric(data$Wickets)
data$Average <- as.numeric(data$Average)
data$Economy <- as.numeric(data$Economy)
data$StrikeRate <- as.numeric(data$StrikeRate)
data$'4Wickets' <- as.numeric(data$'4Wickets')
data$'5Wickets' <- as.numeric(data$'5Wickets')

# Summary Statistics and Descriptive Analysis
summary(data)
describe(data)


# Hypothesis Testing

# Hypothesis 1: Is the average number of wickets greater than 50?
t_test_wickets <- t.test(data$Wickets, mu = 50, alternative = "greater", na.rm = TRUE)
print(t_test_wickets)

# Hypothesis 2: Is the mean economy rate different from 4?
t_test_economy <- t.test(data$Economy, mu = 4, alternative = "two.sided", na.rm = TRUE)
print(t_test_economy)

# Hypothesis 3: Is the average strike rate less than 30?
t_test_strike_rate <- t.test(data$StrikeRate, mu = 30, alternative = "less", na.rm = TRUE)
print(t_test_strike_rate)
