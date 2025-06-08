# Load required libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(corrplot)
library(reshape2)
library(scales)

# Read dataset
data <- read.csv("E:/Downloads/R/user_behavior_dataset.csv")


# Clean column names to avoid errors with spaces/special characters
names(data) <- make.names(names(data))  # Safe column names

# View structure and summary
str(data)
summary(data)
sapply(data, function(x) sum(is.na(x)))

# Convert relevant columns to factors
data$Gender <- as.factor(data$Gender)
data$Operating.System <- as.factor(data$Operating.System)
data$Device.Model <- as.factor(data$Device.Model)
data$User.Behavior.Class <- as.factor(data$User.Behavior.Class)

# Get numeric columns
num_cols <- names(data)[sapply(data, is.numeric)]

# Histograms for all numeric columns
for (col in num_cols) {
  p <- ggplot(data, aes(x = !!sym(col))) +
    geom_histogram(fill = "steelblue", color = "black", bins = 30) +
    ggtitle(paste("Histogram of", col)) +
    xlab(col) +
    theme_minimal()
  print(p)
}

# Specific histogram for Screen On Time
ggplot(data, aes(x = Screen.On.Time..hours.day.)) +
  geom_histogram(fill = "green", color = "black", bins = 30, alpha = 0.9) +
  labs(
    title = "Screen Time Distribution",
    x = "Screen On Time (hours/day)",
    y = "Number of Users"
  ) +
  theme_minimal()

# 1) Gender distribution
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  ggtitle("Histogram of Gender") +
  ylab("Count") +
  theme_minimal()

# 2) Operating System distribution
ggplot(data, aes(x = Operating.System)) +
  geom_bar(fill = "lightgreen", color = "black") +
  ggtitle("Bar Chart of Operating System") +
  ylab("Count") +
  theme_minimal()

# 3) Pie chart of phone models
phone_model_counts <- data %>%
  count(Device.Model) %>%
  mutate(percentage = round(n / sum(n) * 100, 2),
         label = paste0(percentage, "%"))

ggplot(phone_model_counts, aes(x = "", y = n, fill = Device.Model)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), size = 4) +
  ggtitle("Pie Chart of Phone Models")

# 4) Scatter Plot: Number of Apps vs Battery Drain
ggplot(data, aes(x = Number.of.Apps.Installed, y = Battery.Drain..mAh.day.)) +
  geom_point(color = "hotpink") +
  ggtitle("Scatter Plot: Number of Apps vs Battery Drain") +
  theme_minimal()

# 5) App Usage vs Screen Time
ggplot(data, aes(x = App.Usage.Time..min.day., y = Screen.On.Time..hours.day.)) +
  geom_point(color = "navyblue") +
  ggtitle("Scatter Plot: App Usage vs Screen Time") +
  theme_minimal()

# 6) Boxplot: Screen Time Group vs Battery Drain
data <- data %>%
  mutate(ScreenTimeGroup = cut(Screen.On.Time..hours.day.,
                               breaks = c(0, 2, 4, 6, 8, Inf),
                               labels = c("0–2", "2–4", "4–6", "6–8", "8+")))

ggplot(data, aes(x = ScreenTimeGroup, y = Battery.Drain..mAh.day.)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Box Plot: Screen Time Groups vs Battery Drain") +
  xlab("Screen Time Group (hours/day)") +
  ylab("Battery Drain (mAh/day)") +
  theme_minimal()

# 7) Heat Map: Screen Time vs User Behavior Class
heat_data <- data %>%
  group_by(User.Behavior.Class, Screen.On.Time..hours.day.) %>%
  summarise(count = n(), .groups = "drop")

ggplot(heat_data, aes(x = Screen.On.Time..hours.day., y = User.Behavior.Class, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "green", high = "red") +
  ggtitle("Heat Map: Screen Time vs User Behavior Class") +
  theme_minimal()

# 8) Battery Drain Distribution by Operating System
ggplot(data, aes(x = Battery.Drain..mAh.day., color = Operating.System)) +
  geom_freqpoly(binwidth = 100) +
  ggtitle("Battery Drain Distribution by Operating System") +
  xlab("Battery Drain (mAh/day)") +
  ylab("Number of Users") +
  scale_color_manual(values = c("Android" = "blue", "iOS" = "red")) +
  theme_minimal()

# Scatterplot: Gender vs User Behavior by OS
ggplot(data, aes(x = Gender, y = User.Behavior.Class, color = Operating.System)) +
  geom_jitter(width = 0.2, height = 0.2, size = 2) +
  scale_color_manual(values = c("Android" = "green", "iOS" = "blue")) +
  ggtitle("Scatterplot: Gender vs User Behavior by OS") +
  xlab("Gender") +
  ylab("User Behavior Class") +
  theme_minimal()

# Boxplots of numeric vars by Gender
for (col in num_cols) {
  p <- ggplot(data, aes(x = Gender, y = .data[[col]], fill = Gender)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col, "by Gender")) +
    xlab("Gender") +
    ylab(col) +
    theme_minimal()
  print(p)
}

# Correlation Matrix
num_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(num_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)

# Bar Plot: User Behavior Class by Gender
ggplot(data, aes(x = User.Behavior.Class, fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("User Behavior Class by Gender") +
  xlab("User Behavior Class") +
  ylab("Count") +
  theme_minimal()

# Mean of numeric features by User Behavior Class
data %>%
  group_by(User.Behavior.Class) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  print()
