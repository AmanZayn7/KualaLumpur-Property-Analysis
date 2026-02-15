library(dplyr)
library(stringr)
library(tidyr)
library(scales)
filter_df <- read.csv("5. kl_property_data 2.csv",                      header=TRUE)
View(filter_df)
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
unique(filter_df$Property_Location)




# Install and load necessary packages
install.packages("ggplot2")
library(ggplot2)

# Assuming filter_df is your dataset
# Assuming "Price(in RM)" is the column name for property prices
# If not, replace these names with the actual ones from your dataset

# Group by Property_Location and calculate average price
average_prices <- aggregate(`Price(in RM)` ~ Property_Location, data = filter_df, FUN = mean)

# Order locations by average price in descending order
average_prices <- average_prices[order(average_prices$`Price(in RM)`, decreasing = TRUE), ]

# Select the top 5 locations
top5_locations <- head(average_prices, 5)

# Create a bar plot
bar_plot <- ggplot(top5_locations, aes(x = Property_Location, y = `Price(in RM)`, fill = `Price(in RM)`)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = scales::comma) +
  labs(title = "Top 5 Most Expensive Locations",
       x = "Property Location",
       y = "Average Price (in RM)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),               # Adjust font size of y-axis labels
        plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))

# Display the plot
print(bar_plot)

#creating column 
klcc_data$range <- cut(klcc_data$`Price(in RM)`, breaks = c(-Inf, 5000000, 20000000, Inf),
                       labels = c("Less than 5000000", "5000000-20000000", "More than 20000000"))

#creating table based on range
range_data <- table(klcc_data$range)

#changing table into a dataframe
range_df <- as.data.frame(range_data)

#changing column names
colnames(range_df) <- c("category", "range_count")

ggplot(range_df, aes(category, range_count, fill = range_count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = range_count), vjust = 0) +
  scale_fill_gradient(low = "#ffd4e5", high = "#feffa3")+
  labs(title = "Price range of properties in KLCC, Kuala Lumpur",
       x = "Price (in RM)",
       y = "Number of Properties IN klcc") + 
         theme_bw()


# Assuming klcc_data is your dataset and 'Price(in RM)' is the column for property prices

library(ggplot2)

# Convert 'Price(in RM)' to numeric
klcc_data$Price_in_RM <- as.numeric(gsub(",", "", klcc_data$`Price(in RM)`))

# Filter properties with price above 20,000,000
high_price_data <- klcc_data[klcc_data$Price_in_RM > 20000000, ]

  # Assuming klcc_data is your dataset and 'Price(in RM)' is the column for property prices
  
  # Convert 'Price(in RM)' to numeric
  klcc_data$Price_in_RM <- as.numeric(gsub(",", "", klcc_data$`Price(in RM)`))
  
  # Filter properties with price above 20,000,000
  high_price_properties <- klcc_data[klcc_data$Price_in_RM > 20000000, ]
  
  # Display the list of properties
  print(high_price_properties)
  

  
    install.packages("plotly")
    
  
   
    # Load required libraries
    library(ggplot2)
    library(plotly)
    library(scales)
    
    
    
    # Convert 'Price(in RM)' to numeric
    klcc_data$Price_in_RM <- as.numeric(gsub(",", "", klcc_data$`Price(in RM)`))
    
    # Filter properties in the range of 20,000,000 to 50,000,000
    mid_range_properties <- klcc_data[klcc_data$Price_in_RM >= 20000000 & klcc_data$Price_in_RM <= 50000000, ]
    
    # Create a mesmerizing ggplot
    gg_plot <- ggplot(mid_range_properties, aes(x = Price_in_RM)) +
      geom_histogram(aes(y = ..count.., fill = ..count..), bins = 30, color = "white", alpha = 0.7) +
      scale_fill_gradient(low = "#FFEEEE", high = "#8B0000") +  # Shades of red
      labs(title = "Histogram for Properties in the Range 20,000,000 to 50,000,000",
           x = "Price (in RM)",
           y = "Number of Properties") +
      theme_minimal() +
      scale_x_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))
    
    # Convert ggplot to plotly for interactivity
    plotly_plot <- ggplotly(gg_plot)
    
    # Print the plot
    print(plotly_plot)
  
      
      # Load necessary libraries
      library(ggplot2)
    
      # Convert 'Size(in sq ft)' and 'Price(in RM)' to numeric
      filter_df$Size_in_sq_ft <- as.numeric(gsub(",", "", filter_df$`Size(in sq ft)`))
      filter_df$Price_in_RM <- as.numeric(gsub(",", "", filter_df$`Price(in RM)`))
      
      # Filter data for the specified size range
      filtered_data <- subset(filter_df, Size_in_sq_ft >= 5000 & Size_in_sq_ft <= 10000)
      
      # Group by 'Property_Location' and calculate the average price
      average_prices <- aggregate(Price_in_RM ~ Property_Location, data = filtered_data, FUN = mean, na.rm = TRUE)
      
      # Order the data by Average_Price in descending order
      average_prices <- average_prices[order(-average_prices$Price_in_RM),]
      
      # Select top 20 areas
      top_20_areas <- head(average_prices, 20)
      
      # Remove ', Kuala Lumpur' from each region
      top_20_areas$Property_Location <- gsub(", Kuala Lumpur", "", top_20_areas$Property_Location)
      
      # Create a bar graph using ggplot2 with 20 shades of blue
      ggplot(top_20_areas, aes(x = reorder(Property_Location, Price_in_RM), y = Price_in_RM, fill = Price_in_RM)) +
        geom_bar(stat = "identity", color = "white") +
        scale_fill_gradient(low = "#cce5ff", high = "#001a66") +
        labs(title = "Top 20 Most Expensive Areas (5000-10000 sq ft) in KLCC",
             x = "Property Location",
             y = "Average Price (in RM)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = scales::comma)  # This line ensures numerical labels without scientific notation
        guides(fill = guide_legend(title = "Average Price (in RM)"))
      
    
        
        # Convert 'Price(in RM)' to numeric
        klcc_data$Price_in_RM <- as.numeric(gsub(",", "", klcc_data$`Price(in RM)`))
        
        # Filter properties above 20 million
        above_20m_properties <- klcc_data[klcc_data$Price_in_RM > 20000000, ]
        
        # Display the properties
        print(above_20m_properties)
        
        # Assuming klcc_data is your dataset and 'Price(in RM)' is the column for property prices
        
        # Convert 'Price(in RM)' to numeric
        klcc_data$Price_in_RM <- as.numeric(gsub(",", "", klcc_data$`Price(in RM)`))
        
        # Create a new column 'Price_Range' based on property prices
        klcc_data$Price_Range <- cut(klcc_data$Price_in_RM,
                                     breaks = c(-Inf, 5000000, 20000000, Inf),
                                     labels = c("< 5 Million", "5 Million - 20 Million", "> 20 Million"),
                                     include.lowest = TRUE)
        
        # Create a bar graph using ggplot2
        library(ggplot2)
        ggplot(klcc_data, aes(x = Price_Range, fill = Price_Range)) +
          geom_bar(stat = "count") +
          labs(title = "Property Price Distribution in KLCC, Kuala Lumpur",
               x = "Price Range",
               y = "Number of Properties") +
          theme_minimal()
        
          
        
          # Convert 'Price(in RM)' to numeric
          klcc_data$Price_in_RM <- as.numeric(gsub(",", "", klcc_data$`Price(in RM)`))
          
          # Filter properties above 20 million
          above_20m_properties <- klcc_data[klcc_data$Price_in_RM > 20000000, ]
          
          # Create a scatterplot using ggplot2
          library(ggplot2)
          ggplot(above_20m_properties, aes(x = Price_in_RM, y = Price_in_RM, color = Price_in_RM)) +
            geom_point(size = 5, alpha = 0.8) +
            scale_color_gradient(low = "#3498db", high = "#2980b9") +
            labs(title = "Properties Above 20 Million in KLCC",
                 x = "Price (in RM)",
                 y = "Price (in RM)") +
            scale_x_continuous(labels = scales::comma) +
            scale_y_continuous(labels = scales::comma) +
            theme_minimal() +
            theme(legend.position = "none")
          