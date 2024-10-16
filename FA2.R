# Load libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)
library(purrr)
library(tibble)



dataset <- read_excel("dataset.xls", sheet = 1)  

head(dataset)

# Check the structure of the data
str(dataset)

# Remove unnecessary columns (Address, Sr. No., Roll No., etc.)
dataset_clean <- dataset %>%
  select(-c("Sr. No.", "Roll No.", "Address Line 1", "Address Line 2", "Address Line 3"))

# Check for missing values in the dataset
colSums(is.na(dataset_clean))


# Summary of the dataset
summary(dataset_clean)

colnames(dataset_clean)

head(dataset_clean)

# Calculate the mean of CET Score, excluding NA values
mean_cet_score <- mean(dataset_clean$`CET Score`, na.rm = TRUE)

# Fill NA values in CET Score with the calculated mean
dataset_clean$`CET Score`[is.na(dataset_clean$`CET Score`)] <- mean_cet_score

# Export the dataframe as a CSV file
write.csv(dataset_clean, "dataset_clean.csv", row.names = FALSE)


# Function to calculate mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Define a function to calculate measures for a numeric vector
calculate_measures <- function(column) {
  mean_value <- mean(column, na.rm = TRUE)
  median_value <- median(column, na.rm = TRUE)
  mode_value <- get_mode(column)
  range_value <- range(column, na.rm = TRUE)
  variance_value <- var(column, na.rm = TRUE)
  sd_value <- sd(column, na.rm = TRUE)
  iqr_value <- IQR(column, na.rm = TRUE)
  
  # Return a named vector
  return(c(Mean = mean_value,
           Median = median_value,
           Mode = mode_value,
           Range = paste(range_value[1], "to", range_value[2]),
           Variance = variance_value,
           `Standard Deviation` = sd_value,
           IQR = iqr_value))
}

# Apply the function to all numeric columns in the dataset
numeric_measures <- dataset %>%
  select(where(is.numeric)) %>%  # Select only numeric columns
  map_df(~ calculate_measures(.x), .id = "Column")  # Apply the measure function and include column names

# Print the summary table for all numeric columns
print(numeric_measures)



# Count the number of students by Gender
gender_count <- dataset_clean %>%
  count(Gender)

# Pie chart of Male vs. Female Students
ggplot(gender_count, aes(x = "", y = n, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  # Converts bar chart to pie chart
  labs(title = "Distribution of Students by Gender") +
  theme_void() +
  theme(legend.position = "right")  # Adjust legend position


# Bar plot to show count of students by Region
ggplot(dataset_clean, aes(x = Region)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Number of Students by Region", x = "Region", y = "Count of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

mother_tongue_count <- dataset_clean %>%
  count(`Mother Tongue`)  # Count number of students for each mother tongue

# Donut chart for Mother Tongue distribution
ggplot(mother_tongue_count, aes(x = 2, y = n, fill = `Mother Tongue`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  xlim(0.5, 2.5) +  # Creates the hole in the center
  labs(title = "Distribution of Students by Mother Tongue", x = NULL, y = NULL) +
  theme_void()

# Bar chart to show the number of students per state
ggplot(dataset_clean, aes(x = State)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Number of Students by State", x = "State", y = "Count of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Bar chart of HSC Passing Year distribution
ggplot(dataset_clean, aes(x = `HSC Passing Year`)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Number of Students by HSC Passing Year", x = "HSC Passing Year", y = "Count of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Boxplot of HSC Total Percentage by Passing Year
ggplot(dataset_clean, aes(x = factor(`HSC Passing Year`), y = `HSC Total Percentage`, fill = factor(`HSC Passing Year`))) +
  geom_boxplot() +
  labs(title = "HSC Total Percentage Distribution by Passing Year", x = "HSC Passing Year", y = "HSC Total Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Histogram of CET Scores
ggplot(dataset_clean, aes(x = `CET Score`)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of CET Scores", x = "CET Score", y = "Frequency") +
  theme_minimal()


# Average CET Scores by State
average_cet_by_state <- dataset_clean %>%
  group_by(State) %>%
  summarize(Avg_CET_Score = mean(`CET Score`, na.rm = TRUE))

# Bar chart of average CET Scores by State
ggplot(average_cet_by_state, aes(x = reorder(State, -Avg_CET_Score), y = Avg_CET_Score, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Average CET Scores by State", x = "State", y = "Average CET Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Density plot of CET Scores
ggplot(dataset_clean, aes(x = `CET Score`)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "Density Plot of CET Scores", x = "CET Score", y = "Density") +
  theme_minimal()

# Stacked bar chart of Seat Types by Gender
ggplot(dataset_clean, aes(x = `Seat Type`, fill = Gender)) +
  geom_bar(position = "stack") +
  labs(title = "Seat Type Distribution by Gender", x = "Seat Type", y = "Count of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Count the number of students per Religion
student_count_by_religion <- dataset_clean %>%
  count(Religion)

# Bar chart of Student Count by Religion
ggplot(student_count_by_religion, aes(x = reorder(Religion, -n), y = n, fill = Religion)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Students by Religion", x = "Religion", y = "Count of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme_minimal()

# Count the number of students per Home University
student_count_by_university <- dataset_clean %>%
  count(`Home University`)

# Bar chart of Student Count by Home University
ggplot(student_count_by_university, aes(x = reorder(`Home University`, -n), y = n, fill = `Home University`)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Students by Home University", x = "Home University", y = "Count of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme_minimal()

# Count the number of students per Category
student_count_by_category <- dataset_clean %>%
  count(Category)

# Bar chart of Student Count by Category
ggplot(student_count_by_category, aes(x = reorder(Category, -n), y = n, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Students by Category", x = "Category", y = "Count of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme_minimal()

# Stacked bar chart of Students by Category and Gender
ggplot(dataset_clean, aes(x = Category, fill = Gender)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Students by Category and Gender", x = "Category", y = "Count of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  theme_minimal()

# Density plot of SSC Total Percentage
ggplot(dataset_clean, aes(x = `SSC Total Percentage`)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot of SSC Total Percentage", x = "SSC Total Percentage", y = "Density") +
  theme_minimal()


# Histogram of HSC Percentage
ggplot(dataset_clean, aes(x = `HSC Total Percentage`)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of HSC Total Percentage", x = "HSC Total Percentage", y = "Count of Students") +
  theme_minimal()

# Density plot of HSC Total Percentage
ggplot(dataset_clean, aes(x = `HSC Total Percentage`)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Density Plot of HSC Total Percentage", x = "HSC Total Percentage", y = "Density") +
  theme_minimal()

