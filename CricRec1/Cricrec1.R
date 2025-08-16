# Import required libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Read the CSV data
bowling_data <- read.csv("BowlingBestInnings.csv", stringsAsFactors = FALSE)

# Data preprocessing steps
bowling_clean <- bowling_data %>%
  # Convert Match Date to proper date format
  mutate(Match_Date = as.Date(Match.Date, format="%d-%m-%Y")) %>%
  
  # Replace missing values (-) in Mdns column with 0
  mutate(Mdns = ifelse(Mdns == "-", 0, as.numeric(Mdns))) %>%
  
  # Convert numeric columns to appropriate type
  mutate(
    Overs = as.numeric(Overs),
    Runs = as.numeric(Runs),
    Wkts = as.numeric(Wkts),
    Econ = as.numeric(Econ)
  ) %>%
  
  # Extract year from Match_Date
  mutate(Year = year(Match_Date)) %>%
  
  # Clean team names (remove 'v' prefix)
  mutate(Opposition = gsub("v ", "", Opposition)) %>%
  
  # Create a column for complete overs (floor value of Overs)
  mutate(Complete_Overs = floor(Overs)) %>%
  
  # Create a column for partial overs (decimal part * 6 for balls)
  mutate(Partial_Balls = round((Overs - Complete_Overs) * 10)) %>%
  
  # Calculate total balls bowled
  mutate(Total_Balls = (Complete_Overs * 6) + Partial_Balls) %>%
  
  # Calculate strike rate (balls per wicket)
  mutate(Strike_Rate = round(Total_Balls/Wkts, 2))

# Add some useful summary statistics
bowling_stats <- bowling_clean %>%
  group_by(Player) %>%
  summarize(
    Total_Wickets = sum(Wkts),
    Average_Economy = mean(Econ),
    Matches = n(),
    Best_Figures = paste0(max(Wkts), "/", Runs[which.max(Wkts)]),
    Avg_Strike_Rate = mean(Strike_Rate)
  )

# Print first few rows of cleaned data
print("First few rows of cleaned data:")
head(bowling_clean)

# Print summary statistics
print("\nBowling statistics summary:")
head(bowling_stats)

# Save processed data
write.csv(bowling_clean, "bowling_data_processed.csv", row.names = FALSE)
write.csv(bowling_stats, "bowling_statistics_summary.csv", row.names = FALSE)

# Basic data analysis
print("\nSummary statistics:")
summary(bowling_clean[c("Overs", "Runs", "Wkts", "Econ", "Strike_Rate")])

# Count of performances by year
yearly_performances <- bowling_clean %>%
  group_by(Year) %>%
  summarize(Count = n())
print("\nPerformances by year:")
print(yearly_performances)

# Top 5 best bowling figures (by wickets and runs)
top_performances <- bowling_clean %>%
  arrange(desc(Wkts), Runs) %>%
  select(Player, Wkts, Runs, Opposition, Match_Date) %>%
  head(5)
print("\nTop 5 bowling performances:")
print(top_performances)





# Install and load required packages
install.packages(c("arules", "arulesViz"))
library(arules)
library(arulesViz)

# Read the data
bowling_data <- read.csv("BowlingBestInnings.csv", stringsAsFactors = FALSE)

# Data Preprocessing
preprocess_data <- function(data) {
  # Convert numeric columns
  data$Econ <- as.numeric(as.character(data$Econ))
  data$Wkts <- as.numeric(as.character(data$Wkts))
  
  # Create categories
  data$Economy_Category <- cut(data$Econ, 
                               breaks = c(0, 3, 4.5, 6, Inf),
                               labels = c("Excellent", "Good", "Average", "Poor"))
  
  data$Wickets_Category <- cut(data$Wkts,
                               breaks = c(-1, 2, 4, Inf),
                               labels = c("Low", "Medium", "High"))
  
  return(data)
}

# Create transaction data
create_transactions <- function(data) {
  # Select relevant columns for transactions
  trans_data <- data.frame(
    Economy = as.factor(data$Economy_Category),
    Wickets = as.factor(data$Wickets_Category),
    Team = as.factor(data$Team),
    Opposition = as.factor(data$Opposition)
  )
  
  # Convert to transactions
  trans <- as(trans_data, "transactions")
  return(trans)
}

# Mine association rules
mine_rules <- function(trans, min_support = 0.01, min_confidence = 0.5) {
  rules <- apriori(trans,
                   parameter = list(support = min_support,
                                    confidence = min_confidence,
                                    minlen = 2))
  
  rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
  return(rules_sorted)
}

# Apply the functions
processed_data <- preprocess_data(bowling_data)
trans <- create_transactions(processed_data)
rules <- mine_rules(trans)

# View top rules
inspect(head(rules, 10))

# Plot rules with enhanced visibility
plot(rules,
     measure = c("support", "confidence"),
     shading = "lift",
     main = "Association Rules: Support vs Confidence",
     col = rainbow(20),  # Vibrant colors
     cex = 1.5,         # Larger points
     pch = 19)         # Solid circles

# Network visualization of top rules
plot(rules[1:20],
     method = "graph",
     control = list(
       type = "items",
       nodeCol = "#0066CC",    # Bright blue nodes
       edgeCol = "#FF3300",    # Bright orange edges
       alpha = 0.8,            # Less transparency
       cex = 1.2              # Larger text
     ))

# Print summary of rules
summary(rules)