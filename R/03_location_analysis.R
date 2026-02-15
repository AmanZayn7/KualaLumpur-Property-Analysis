
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

filter_df <- read.csv("C:\\Users\\ameer\\Desktop\\APU\\YEAR2\\PFDA\\5. kl_property_data_copy.csv",
                      header=TRUE)

#Excluding duplicate values
filter_df <- unique(filter_df)

#Eliminating irrelevant columns
filter_df <- subset(filter_df, select = -c(Rooms, Bathrooms, Car.Parks))

#Changing column names
names(filter_df)=c("Property_Location", "Price(in RM)", "Property_Type", "Size(in sq ft)", "Furnishing_Status")

#Converting Price(in RM) data type
filter_df$`Price(in RM)` <- as.numeric(gsub("RM|,", "", filter_df$`Price(in RM)`))

#Checking for null values
sum(is.na(filter_df$`Price(in RM)`))
sum(is.na(filter_df$Property_Location))
sum(is.na(filter_df$`Size(in sq ft)`))
sum(is.na(filter_df$Furnishing_Status))
sum(is.na(filter_df$Property_Type))

#Checking for whitespaces 
sum(filter_df$`Size(in sq ft)` == "")
sum(filter_df$`Price(in RM)`== "")
sum(filter_df$Property_Location == "")
sum(filter_df$Furnishing_Status == "")
sum(filter_df$Property_Type == "")

#Removing null values
filter_df <- filter_df[!is.na(filter_df$`Price(in RM)`), ]
filter_df <- filter_df[!is.na(filter_df$Furnishing_Status), ]
filter_df <- filter_df[!is.na(filter_df$`Size(in sq ft)`), ]

#Remove whitespaces
filter_df <- filter_df[filter_df$`Size(in sq ft)` != "", ]
filter_df <- filter_df[filter_df$Furnishing_Status != "", ]

#Capitalize the first letter of each word in the columns Proprty_Location and Furnishing_Status
filter_df$Property_Location <- str_to_title(filter_df$Property_Location)
filter_df$Furnishing_Status <- str_to_title(filter_df$Furnishing_Status)

#Splitting Size(in sq ft)
filter_df = separate(filter_df, col = `Size(in sq ft)`, into = c("Type", "Size(in sq ft)"), sep = ": ")

#Changing Size(in sq ft) column to numeric
convert_to_sqft <- function(size_str) {
  #Remove commas and extra spaces
  size_str <- gsub(",", "", size_str)
  size_str <- gsub("\\s+", " ", size_str)
  
  #Convert acres to sq ft
  if (grepl("acre", size_str, ignore.case = TRUE)) {
    acres <- as.numeric(str_extract(size_str, "\\d+\\.?\\d*"))
    return(acres * 43560)
  }
  
  #Convert dimensions to sq ft
  if (grepl("[xX]", size_str)) {
    # Extract the dimension part and then split by 'x' or 'X', allowing for spaces
    dimensions <- as.numeric(unlist(strsplit(str_extract(size_str, "\\d+\\s*[xX]\\s*\\d+"), "\\s*[xX]\\s*")))
    return(prod(dimensions))
  }
  
  #Extract sq ft value
  numeric_sqft <- as.numeric(str_extract(size_str, "\\d+\\.?\\d*"))
  return(numeric_sqft)
}

# Apply the function to the Size(in sq ft) column
filter_df$`Size(in sq ft)` <- sapply(filter_df$`Size(in sq ft)`, convert_to_sqft)

#Round the Size_in_sqft column to two decimal places
filter_df$`Size(in sq ft)` <- round(filter_df$`Size(in sq ft)`, 2)

#Removing values of 0.00 in Size(in sq ft) column
filter_df <- filter_df %>% 
  filter(`Size(in sq ft)` != 0.00)

#Removing 'Unknown' from Furnishing_Status
filter_df <- filter_df %>% 
  filter(Furnishing_Status != "Unknown")

View(filter_df)

#creating subset based on range
range_subset <- subset(filter_df, filter_df$`Size(in sq ft)` >= 5000 & filter_df$`Size(in sq ft)` <= 10000)

View(range_subset)

#adjusting index numbers
rownames(range_subset) = 1:nrow(range_subset)


#analysis 1
#creating column based on sq ft range
filter_df$range <- cut(filter_df$`Size(in sq ft)`, breaks = c(-Inf, 1000, 10000, Inf),
                          labels = c("Less than 1000", "1000-10000", "More than 10000"))

#creating table based on range
range_data <- table(filter_df$range)

#changing table into a dataframe
range_df <- as.data.frame(range_data)

#changing column names
colnames(count_df) <- c("category", "range_count")

#plotting bar graph
library(ggplot2)
ggplot(count_df, aes(category, range_count, fill = range_count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = range_count), vjust = 0) +
  scale_fill_gradient(low = "#ffd4e5", high = "#feffa3")+
  labs(title = "Number of Properties in Different Square Foot Ranges",
       x = "Square Foot Range",
       y = "Number of Properties") + 
  theme_bw()


#analysis 2
# Define the selected location
selected_location <- "Klcc, Kuala Lumpur"
filtered_data <- filter_df
# Create a new column 'location_category' based on the condition
filtered_data <- transform(filter_df, Property_Location = ifelse(Property_Location != selected_location,
                                                                 "KLCC", "Other Locations"))

filtered_data$range <- cut(filtered_data$Size.in.sq.ft., 
                                   breaks = c(-Inf, 5000, 10000, Inf),
                                   labels = c("Less than 5000 sqft", "5000-10000 sqft", "More than 10000 sqft"))

range_data <- table(filtered_data$Property_Location,filtered_data$range)
#changing table into a dataframe
range_df <- as.data.frame(range_data)
View(range_df)
#changing column names
colnames(range_df) <- c("Location", "range", "range_count")

#plotting the bar graph
ggplot(range_df, aes(x = range, y = range_count, fill = Location)) +
  geom_bar(stat = "Identity", position = "dodge")+
  geom_text(aes(label = range_count), position = position_dodge(width = 0.5), vjust = -0.5, size = 5)+
  scale_fill_manual(values = c("#ffd9d9", "#d9d2e9"))+
  labs(x = "square foot range", y = "number of properties",
       title = "property distribution based on square foot range\n in KLCC and other locations")+
    theme_minimal()
  
#analysis 3

#creating variable for required location
selected_location <- "Klcc, Kuala Lumpur"
#filtering data based on sq ft range
filtered_data <- filter_df[filter_df$`Size.in.sq.ft.)` >= 1000 & filter_df$`Size.in.sq.ft.` <= 10000]
# Create a new column 'location_category' based on the condition
filtered_data <- transform(filter_df, Property_Location = ifelse(Property_Location != selected_location,
                                                                 "KLCC", "Other Locations"))
#creating categories based on price ranges
filtered_data$range <- cut(filtered_data$Price.in.RM., 
                           breaks = c(-Inf, 5000000, 10000000, Inf),
                           labels = c("Less than 5 Million", "5-10 million", "More than 10 million"))
#creating a table
range_data <- table(filtered_data$Property_Location,filtered_data$range)
#changing table into dataset
range_df <- as.data.frame(range_data)
#changing column names
colnames(range_df) <- c("Location", "Price_range", "range_count")

#plotting the graph

ggplot(range_df, aes(x = `Price_range`, y = `range_count`, fill = Location))+
  geom_bar(stat = "Identity", position = "dodge")+
  geom_text(aes(label = range_count), position = position_dodge(width = 0.5), vjust = -0.5, size = 3)+
  scale_fill_manual(values = c("lightblue", "lightpink"))+
  labs(x = "square foot range", y = "number of properties",
       title = "property distribution based on price range\n in KLCC and other locations")+
  theme_minimal()




