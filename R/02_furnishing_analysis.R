#Load necessary libraries
library(ggplot2)
library(dplyr)


#Read the dataset of cleaned file
data <- filter_df

#Function to validate numeric columns
validate_numeric <- function(column) {
  return(!any(is.na(column)) && all(column >= 0))
}

#Validate dataset
if (!validate_numeric(data$`Size(in sq ft)`) || !validate_numeric(data$`Price(in RM)`)) {
  stop("Numeric columns contain NA or negative values.")
}

#Remove duplicate entries
data <- distinct(data)

#Filter data for properties between 5000 to 10000 sq ft
filtered_data <- data %>%
  filter(`Size(in sq ft)` >= 5000 & `Size(in sq ft)` <= 10000)

#Analyzing impact of Furnishing Status regardless of other variables
furnishing_analysis <- filtered_data %>%
  group_by(Furnishing_Status) %>%
  summarise(Average_Price = mean(`Price(in RM)`))

#Visualizes the impact of furnishing status on average price
ggplot(furnishing_analysis, aes(x=Furnishing_Status, y=Average_Price, fill=Furnishing_Status)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#ffcc00","#ADD8E6","#cc33ff")) + 
  labs(title="Impact of Furnishing Status on Average Property Price",
       x="Furnishing Status",
       y="Average Price (in RM)")

#Display the original averages for reference
furnishing_analysis

#Calculate and display the difference
differences <- combn(furnishing_analysis$Average_Price, 2, FUN = function(x) abs(x[1] - x[2]))
names(differences) <- combn(furnishing_analysis$Furnishing_Status, 2, FUN = function(x) paste(x[1], "vs", x[2]))
differences

#Calculating the difference in percentage
percentage_differences <- combn(furnishing_analysis$Average_Price, 2, FUN = function(x) (abs(x[1] - x[2]) / mean(x)) * 100)
names(percentage_differences) <- combn(furnishing_analysis$Furnishing_Status, 2, FUN = function(x) paste(x[1], "vs", x[2]))
percentage_differences

#Comparing prices in KLCC vs other regions
klcc_vs_others <- filtered_data %>%
  mutate(Region = ifelse(grepl("Klcc", Property_Location, ignore.case = TRUE), "KLCC", "Others")) %>%
  group_by(Region, Furnishing_Status) %>%
  summarise(Average_Price = mean(`Price(in RM)`))

klcc_vs_others

#Visualizes the comparison
ggplot(klcc_vs_others, aes(x=Furnishing_Status, y=Average_Price, fill=Region)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="Comparison of Property Prices in KLCC vs Other Regions",
       x="Furnishing Status",
       y="Average Price (in RM)",
       fill="Region")

#Reshaping data manually
klcc_prices <- klcc_vs_others %>% filter(Region == "KLCC") %>% select(-Region)
other_prices <- klcc_vs_others %>% filter(Region == "Others") %>% select(-Region)

#Merging data frames by Furnishing_Status
merged_data <- merge(klcc_prices, other_prices, by = "Furnishing_Status", suffixes = c("_KLCC", "_Others"))

#Calculating the percentage difference
merged_data$Percentage_Difference <- (merged_data$Average_Price_KLCC - merged_data$Average_Price_Others) / merged_data$Average_Price_Others * 100
merged_data

#Calculating the overall average percentage difference
overall_avg_percentage_diff <- mean(merged_data$Percentage_Difference, na.rm = TRUE)
print(paste("Overall Average Percentage Difference: ", format(overall_avg_percentage_diff, nsmall = 2), "%", sep=""))

#Filter data for KLCC properties
klcc_data <- filtered_data %>%
  filter(grepl("Klcc", Property_Location, ignore.case = TRUE))

#Scatter plot diagram with regression lines
ggplot(klcc_data, aes(x = `Price(in RM)`, y = `Size(in sq ft)`, color = Furnishing_Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regression Analysis of KLCC Property Prices",
       x = "Price (in RM)",
       y = "Size (in sq ft)",
       color = "Furnishing Status") +
  theme_minimal()

#Making price a continuous variable by
#creating bins for the price range
klcc_data <- klcc_data %>%
  mutate(Price_Range = cut(`Price(in RM)`, breaks = pretty(`Price(in RM)`, n = 10), include.lowest = TRUE))

#Count the number of properties for each furnishing status within each price range
count_data <- klcc_data %>%
  group_by(Price_Range, Furnishing_Status) %>%
  summarise(Count = n()) %>%
  ungroup()

#Plotting the line graph
ggplot(count_data, aes(x = Price_Range, y = Count, group = Furnishing_Status, color = Furnishing_Status)) +
  geom_line(linewidth = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Trend of Properties by Furnishing Status Across Price Ranges in KLCC",
       x = "Price Range",
       y = "Count of Properties",
       color = "Furnishing Status")



