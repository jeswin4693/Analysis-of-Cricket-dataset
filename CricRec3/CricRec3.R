# First, let's import the required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(RColorBrewer)

# Read the CSV file
cricket_data <- read.csv("BattingRunsCareer.csv")

View(cricket_data)
# 1. HISTOGRAMS

# Plot 1: Distribution of Batting Averages
ggplot(cricket_data, aes(x=Ave)) +
  geom_histogram(fill="skyblue", color="black", bins=30) +
  theme_minimal() +
  labs(title="Distribution of Batting Averages",
       x="Average",
       y="Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 2: Distribution of Strike Rates
ggplot(cricket_data, aes(x=SR)) +
  geom_histogram(fill="lightgreen", color="black", bins=25) +
  theme_minimal() +
  labs(title="Distribution of Strike Rates",
       x="Strike Rate",
       y="Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# 2. BAR PLOTS

# Plot 1: Top 10 Run Scorers
cricket_data %>%
  top_n(10, Runs) %>%
  ggplot(aes(x=reorder(Player, Runs), y=Runs)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title="Top 10 Run Scorers",
       x="Player",
       y="Runs")

# Plot 2: Top Century Makers
cricket_data %>%
  top_n(10, X100s) %>%
  ggplot(aes(x=reorder(Player, X100s), y=X100s)) +
  geom_bar(stat="identity", fill="orange") +
  coord_flip() +
  theme_minimal() +
  labs(title="Top 10 Century Makers",
       x="Player",
       y="Number of Centuries")

# 3. BOX PLOTS

# Plot 1: Runs Distribution
ggplot(cricket_data, aes(y=Runs)) +
  geom_boxplot(fill="lightblue") +
  theme_minimal() +
  labs(title="Distribution of Runs",
       y="Runs")

# Plot 2: Strike Rate Distribution by Batting Position
ggplot(cricket_data, aes(y=SR)) +
  geom_boxplot(fill="lightgreen") +
  theme_minimal() +
  labs(title="Distribution of Strike Rates",
       y="Strike Rate")

# 4. HEATMAPS

# Plot 1: Correlation Heatmap
numeric_cols <- cricket_data %>%
  select_if(is.numeric)
correlation <- cor(numeric_cols, use="complete.obs")
corrplot(correlation, method="color", type="upper", 
         order="hclust", addCoef.col="black",
         tl.col="black", tl.srt=45)

# First, let's check the structure of your data
str(cricket_data)

#  modify the code to ensure numeric conversion
top_players <- cricket_data %>%
  top_n(15, Runs) %>%
  select(Player, Runs, Ave, SR, X100s, X50s) %>%
  mutate(
    Runs = as.numeric(Runs),
    Ave = as.numeric(Ave),
    SR = as.numeric(SR),
    X100s = as.numeric(X100s),
    X50s = as.numeric(X50s)
  )

# Verify the data is numeric
print(sapply(top_players[,-1], class))  # Should all show "numeric"

# Create matrix and continue with visualization
matrix_data <- as.matrix(scale(top_players[,-1]))  # Remove Player column for scaling
rownames(matrix_data) <- top_players$Player

# Create custom color palette
my_colors <- colorRampPalette(c("#4575B4", "#FFFFBF", "#D73027"))(100)

# Create the heatmap
heatmap(matrix_data,
        Colv = NA,
        scale = "none",
        col = my_colors,
        margins = c(10, 10),
        main = "\n Cricket Performance Metrics Heatmap\n(Top 15 Run Scorers)",
        xlab = "Metrics",
        ylab = "Players",
        cexRow = 0.8,
        cexCol = 0.8)

# Add legend
legend_colors <- as.matrix(seq(-2, 2, length=5))
legend_labels <- c("Very Low", "Low", "Average", "High", "Very High")

legend("bottomright",
       legend = legend_labels,
       fill = my_colors[c(1, 25, 50, 75, 100)],
       title = "Performance Scale",
       cex = 0.7)

# 5. SCATTER PLOTS

# Plot 1: Runs vs Average
ggplot(cricket_data, aes(x=Runs, y=Ave)) +
  geom_point(alpha=0.6, color="blue") +
  geom_smooth(method="lm", se=FALSE, color="red") +
  theme_minimal() +
  labs(title="Runs vs Batting Average",
       x="Runs",
       y="Average")

# Plot 2: Strike Rate vs Average with Century Count as size
ggplot(cricket_data, aes(x=SR, y=Ave, size=X100s)) +
  geom_point(alpha=0.6, color="purple") +
  theme_minimal() +
  labs(title="Strike Rate vs Average (Size: Number of Centuries)",
       x="Strike Rate",
       y="Average",
       size="Centuries")

# Additional: Jitter Plot
# Plot 1: Runs distribution with jitter
ggplot(cricket_data, aes(x=factor(1), y=Runs)) +
  geom_jitter(width=0.2, alpha=0.6, color="blue") +
  theme_minimal() +
  labs(title="Runs Distribution (Jittered)",
       x="",
       y="Runs")

# Plot 2: Strike Rate distribution with jitter
ggplot(cricket_data, aes(x=factor(1), y=SR)) +
  geom_jitter(width=0.2, alpha=0.6, color="green") +
  theme_minimal() +
  labs(title="Strike Rate Distribution (Jittered)",
       x="",
       y="Strike Rate")

