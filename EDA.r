library(tidyverse)
library(tidyr)
library(data.table)   #fread
library(dplyr)
library(ggplot2)
library(readxl)
library(modelsummary) # for neat tables
library(zoo) # for yearmon
library(fixest) # for feols etc.
library(sandwich) # for vcovHC
library(car) # for linearHypothesis
library(here)
library(haven)
library(modelsummary)
library(ggcorrplot)

#dataDir <- paste0(setwd('/Users/bystrov/Desktop/IO/EPs/final_ep'), "/data/products.csv")
#dataDir
#data <- read_csv(dataDir, col_names = TRUE)

print(head(data))
spec(data)

##### Feature engineering:

# Filtering the data
mushy <- data %>% filter(mushy == 1)
firm <- data %>% filter(mushy == 0)

# Extract city and quarter from 'market'
data <- data %>%
  mutate(city = as.numeric(gsub("C(\\d+)Q\\d+", "\\1", market)),
         quarter = as.numeric(gsub("C\\d+Q(\\d+)", "\\1", market))) %>%
  # Extract firm from 'product'
  mutate(firm = as.numeric(gsub("F(\\d+)B\\d+", "\\1", product)))



##########
### Summary stat: 
##########

# Define output file
output_tex <- "/Users/bystrov/Desktop/IO/EPs/final_ep/summary_stats.tex"

# Save as LaTeX
datasummary_skim(data, output = output_tex)

print(paste("Summary saved to:", output_tex))

datasummary_skim(data, fmt = 3) 
names(data)
?datasummary_skim
length(unique(data$market))

##########
### Correlations ####


# Select relevant columns
cor_data <- data[, c("servings_sold", "city_population", "price_per_serving", "price_instrument")]

# Compute the correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")  # Use complete observations only

# Print the correlation matrix
print(cor_matrix)


# Base R plot: Consumption vs. Price (original)
par(mfrow = c(1, 2))  # Arrange 2 plots side by side
par(mgp = c(2, 1, 0)) 

# Plot 1: Original plot
plot(data$price_per_serving,log(data$servings_sold), xlab =  "Price per Serving",
     ylab =  "Servings Sold", col = "blue", pch = 16)
title("Consumption and Price of Cereals (Unaggregated)", line= 0.3)

# Plot 2: Mushy vs Firm in the same plot using ggplot2
ggplot(data, aes(x = price_per_serving, y = log(servings_sold), color = factor(mushy))) +
  geom_point() +
  labs(title = "Mushy vs Firm Cereals: Price vs Servings Sold",
       x = "Price per Serving",
       y = "Servings Sold",
       color = "Cereal Type") +
  scale_color_manual(values = c("blue", "red"), labels = c("Firm", "Mushy")) +
  theme_minimal()

# Plot 3: Mushy vs Crunchy in the same plot using ggplot2
ggplot_object <- ggplot(data, aes(x = log(servings_sold), y = price_per_serving, color = factor(mushy))) +
  geom_point(alpha = 0.4) +  # Scatter plot with transparency
  geom_smooth(method = "lm", se = FALSE, aes(linetype = factor(mushy))) +  # Regression lines
  labs(
       x = "Log(Servings Sold)",
       y = "Price per Serving",
       color = "Cereal Type",
       linetype = "Cereal Type") +  # Legend for line types
  scale_color_manual(values = c("blue", "red"), labels = c("Crunchy", "Mushy")) +
  scale_linetype_manual(values = c("solid", "solid"), labels = c("Crunchy", "Mushy")) +  # Different line types
  theme(
    plot.title = element_text(size = 15, face = "bold", family = "sans"),  # Title font settings
    axis.title = element_text(size = 15, family = "sans"))+  # Adjust axis labels 
  labs(title = "Demand: Mushy vs Crunchy Cereals")
ggsave("demand_plot_entire.png", plot = ggplot_object, width = 10, height = 4, dpi = 100)



#### Distribution plots of Continious variables: 

# Open graphics device (optional, remove if running interactively)
png("/Users/bystrov/Desktop/IO/EPs/final_ep/plots/distribution_plots.png", width = 1000, height = 350)

# Set layout: 2 rows, 2 columns
par(mfrow = c(1,4), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))  # oma reserves space for the title

# 1st plot: Distribution of Servings Sold
hist(data$servings_sold, breaks = 30, col = "lightblue", border = "black",
     main = "Servings Sold", xlab = "Servings Sold")

# 2nd plot: Distribution of City Population
hist(data$city_population, breaks = 30, col = "lightcoral", border = "black",
     main = "City Population", xlab = "City Population")

# 3rd plot: Distribution of Price per Serving
hist(data$price_per_serving, breaks = 30, col = "lightgreen", border = "black",
     main = "Price per Serving", xlab = "Price per Serving",cex = 1.5, font = 2)

# 4th plot: Distribution of Price Instrument
hist(data$price_instrument, breaks = 30, col = "lightgoldenrod", border = "black",
     main = "Price Instrument", xlab = "Price Instrument")

# Add main title to the entire panel
mtext("Distributions of Continuous Variables", outer = TRUE, cex = 1.5, font = 2)

# Close graphics device (optional, remove if running interactively)
dev.off()

# Print confirmation
print("Distribution plots saved as 'distribution_plots.png'")



####### Outliars

Q1 <- quantile(data$servings_sold, 0.25, na.rm = TRUE)
Q3 <- quantile(data$servings_sold, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

iqr_filtered <- data |> filter(servings_sold >= lower_bound & servings_sold <= upper_bound)

(dim(data)[1]-dim(iqr_filtered)[1])/dim(data)[1] # % of outliars 

### Outliers? Distribution of the Quantities 

min(data$servings_sold)
max(data$servings_sold)
mean(data$servings_sold)
median(data$servings_sold)

#Distributions of main variables: 

ggplot(data, aes(x = servings_sold)) + 
  geom_histogram(bins = 1000, fill = "blue", alpha = 0.7) +
  theme_minimal()

ggplot(data, aes(x = log(servings_sold))) + 
  geom_histogram(bins = 1000, fill = "blue", alpha = 0.7) +
  theme_minimal()

ggplot(data, aes(x = price_per_serving)) + 
  geom_histogram(bins = 1000, fill = "blue", alpha = 0.7) +
  theme_minimal()

data <- data |> mutate(z_score = scale(servings_sold))

names(data)

plot(data$price_instrument,data$servings_sold, xlab =  "Price instrument",
     ylab =  "Servings Sold", col = "blue", pch = 16)
title("Consumption and Price Instrument", line= 0.3)

## 
length(unique(data$market))



## Demand plots by firm and product 


# Define unique firms
unique_firms <- unique(data$firm)

# Open graphics device (optional, remove if running interactively)
png("/Users/bystrov/Desktop/IO/EPs/final_ep/plots/demand_plots.png", width = 1200, height = 800)

# Set up 2x3 layout with outer margins for a title
par(mfrow = c(2,3), mar = c(5, 5, 4, 2), oma = c(2, 2, 5, 2))  # `oma` for outer margin (pillar title)

# Track the number of plots per page
plot_count <- 0  

for (firm in unique_firms) {
  subset_data <- data %>% filter(firm == !!firm)
  
  # Ensure there are enough observations for plotting
  if (nrow(subset_data) > 1) {  
      
    # Extract unique products for the firm
    unique_products <- unique(subset_data$product)
    
    # Create a color palette for products
    product_colors <- setNames(rainbow(length(unique_products)), unique_products)  
    
    # Plot demand with product-based colors
    plot(log(subset_data$servings_sold), subset_data$price_per_serving,
         xlab = "Log(Servings Sold)", ylab = "Price per Serving",
         col = product_colors[subset_data$product],  
         pch = 16,
         main = paste("Firm", firm),
         cex.lab = 2.5,  # Increase font size of X and Y axis labels
         cex.main = 2.5)  # Increase title font size
    
    # Add legend
    legend("topright", legend = unique_products, col = product_colors, pch = 16, title = "Product")
    
    # Track the number of plots
    plot_count <- plot_count + 1
    
    # If first plot on the page, add a pillar title
    if (plot_count == 1) {
      mtext("Demand plots by firm and product", outer = TRUE, cex = 2, font = 2, line = 2)
    }
    
    # If 6 plots are completed, reset the layout for a new page
    if (plot_count == 6) {
      dev.off()  # Save the current set of plots
      plot_count <- 0  # Reset counter
      
      # Open a new file for the next set of plots
      png(paste0("/Users/bystrov/Desktop/IO/EPs/final_ep/plots/demand_plots_", Sys.time(), ".png"), width = 1200, height = 800)
      par(mfrow = c(2,3), mar = c(5, 5, 4, 2), oma = c(2, 2, 5, 2))  # Reset layout with space for title
    }
  }
}

# Close graphics device
dev.off()

# Print confirmation
print("Plots saved in a 2x3 pillar layout with titles and larger font.")

unique(data$product)
## By unique city and firm

# Unique cities
unique_cities <- unique(data$city)

# Define output folder and create if not existing
output_folder_cities <- "/Users/bystrov/Desktop/IO/EPs/final_ep/plots/cities"

for (city in unique_cities) {
  subset_data <- data %>% filter(city == !!city)
  unique_firms <- unique(subset_data$firm)  # Ensure correct column name
  firm_colors <- setNames(rainbow(length(unique_firms)), unique_firms)  # Assign colors to firms

  if (nrow(subset_data) > 1) {  # Ensure there are enough points to plot
    
    # Define file name dynamically
    file_name <- paste0(output_folder_cities, "/demand_plot_city_", city, ".png")
    
    # Open graphics device
    png(file_name, width = 800, height = 600)
    
    # Plot demand with firm-based color
    plot(subset_data$price_per_serving, log(subset_data$servings_sold),
         xlab = "Price per Serving",
         ylab = "Log(Servings Sold)",
         col = product_colors[subset_data$product],  # Assign colors based on product
         pch = 16)

    # Add title
    title(paste("Demand Curve for City", city), line = 0.3)
    
    # Add legend to show firm colors
    legend("topright", legend = unique_firms, col = firm_colors, pch = 16, title = "Firm")

    # Close the graphics device
    dev.off()
    
    # Print confirmation
    print(paste("Plot saved for City", city, "at", file_name))
  }
}


####
unique_products <- unique(data$product)

plot(data$price_per_serving,log(data$servings_sold), xlab =  "Price per serving",
     ylab =  "Servings Sold", col = "blue", col = firm_colors[subset_data$firm],  pch = 16)
title("Consumption and Price Instrument", line= 0.3)


unique_firms <- unique(data$firm)
firm_colors <- setNames(rainbow(length(unique_firms)), unique_firms)  # Assign colors to products

plot(log(data$servings_sold),data$price_per_serving, 
         xlab = "Log(Servings Sold)",
         ylab = "Price per Serving",
         col = firm_colors[data$firm],  # Assign colors based on firm
         pch = 16)


### Also make sence to differ by quarter, because quarter is time - 90 days 

unique_quartes <- unique(data$quarter)
quarter_colors <- setNames(rainbow(length(unique_quartes)), unique_quartes)  # Assign colors to products

plot(data$price_per_serving, log(data$servings_sold),
         xlab = "Price per Serving",
         ylab = "Log(Servings Sold)",
         col = quarter_colors[data$quarter],  # Assign colors based on firm
         pch = 16)


# city quarter

# Define output folder and create if not existing
output_folder_firms_q <- "/Users/bystrov/Desktop/IO/EPs/final_ep/plots/firm_quarter"

for (firm in unique_firms) {
  subset_data <- data %>% filter(firm == !!firm)
  unique_quartes <- unique(subset_data$quarter)
  quarter_colors <- setNames(rainbow(length(unique_quartes)), unique_quartes)  # Assign colors to products

  if (nrow(subset_data) > 1) {  # Ensure there are enough points to plot
    
    # Define file name dynamically
    file_name <- paste0(output_folder_firms_q, "/demand_plot_firm_", firm, ".png")
    
    # Open graphics device
    png(file_name, width = 800, height = 600)
    
    # Plot demand with product-based color
    plot(log(subset_data$servings_sold),subset_data$price_per_serving,
         xlab = "Log(Servings Sold)",
         ylab = "Price per Serving", 
         col = quarter_colors[subset_data$quarter],  # Assign colors based on product
         pch = 16)
    
    # Add title
    title(paste("Demand Curve for Firm", firm), line = 0.3)
    
    # Add legend to show product colors
    legend("topright", legend = unique_quartes, col = quarter_colors, pch = 16, title = "Product")

    # Close the graphics device
    dev.off()
    
    # Print confirmation
    print(paste("Plot saved for Firm", firm, "at", file_name))
  }
}



### the % of mushy products by firm 

mushy_percentage_by_firm <- data %>%
  group_by(firm) %>%
  summarise(Mushy_Percentage = mean(mushy) * 100)

# Print the result in R console
print(mushy_percentage_by_firm)

## Which products are mushy:
mushy_products <- data %>%
  filter(mushy==1)%>%
  group_by(mushy) %>%
  count(product)

mushy_products

crunchy_products <- data %>%
  filter(mushy==0)%>%
  group_by(mushy) %>%
  count(product)

crunchy_products


####  Number of mushy and crunchy producte 

mushy_percentage_by_firm <- data %>%
  group_by(firm) %>%
  summarise(Mushy_Percentage = mean(mushy) * 100)

# Print the result in R console
print(mushy_percentage_by_firm)

## Which products are mushy:
mushy_products <- data %>%
  filter(mushy==1)%>%
  count(product)

num_unique_mushy_products <- nrow(mushy_products)
num_unique_mushy_products

crunchy_products <- data %>%
  filter(mushy==0)%>%
  count(product)

num_unique_crunchy_products <- nrow(crunchy_products)
num_unique_crunchy_products


####



f3_data  <- data %>%
  filter(firm==3)


ggplot(f3_data, aes(x = log(servings_sold), y = price_per_serving, color = factor(mushy))) +
  geom_point(alpha = 0.6) +  # Scatter plot with transparency
  geom_smooth(method = "lm", se = FALSE, aes(linetype = factor(mushy))) +  # Regression lines
  labs(
       x = "Log(Servings Sold)",
       y = "Price per Serving",
       color = "Cereal Type",
       linetype = "Cereal Type") +  # Legend for line types
  scale_color_manual(values = c("#00e5ff", "red"), labels = c("Crunchy", "Mushy")) +
  scale_linetype_manual(values = c("solid", "solid"), labels = c("Crunchy", "Mushy")) +  # Different line types
  theme(
    plot.title = element_text(size = 30, face = "bold", family = "sans"),  # Title font settings
    axis.title = element_text(size = 30, family = "sans"))+  # Adjust axis labels 
  labs(title = "Demand: Mushy vs Crunchy Cereals")


ggplot_object <- ggplot(f3_data, aes(x = log(servings_sold), y = price_per_serving, color = factor(mushy))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = factor(mushy))) +
  labs(
       x = "Log(Servings Sold)",
       y = "Price per Serving",
       color = "Cereal Type",
       linetype = "Cereal Type") +
  scale_color_manual(values = c("#00e5ff", "red"), labels = c("Crunchy", "Mushy")) +
  scale_linetype_manual(values = c("solid", "solid"), labels = c("Crunchy", "Mushy")) +
  theme(
    plot.title = element_text(size = 13, face = "bold", family = "sans"),  
    axis.title = element_text(size = 10, family = "sans"),
    legend.title = element_text(size = 10, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 6),  # Increase legend item size
    legend.key.size = unit(1.5, "cm")  # Adjust legend key size (box around items)
  ) +
  labs(title = "Demand for firm 3: Mushy vs Crunchy product")

# Save with custom width and height (in inches)
ggsave("demand_plot.png", plot = ggplot_object, width = 5, height = 2.5, dpi = 100)


### Whuch segment is more competitive? 

# Calculate Market Shares for each firm in Segment 

data_ms <- data %>%
  group_by(quarter, city, mushy, firm) %>%  # Group by segment (mushy), quarter, city, and firm
  mutate(firm_sales = sum(servings_sold)) %>%  # Total servings sold by firm in that segment
  group_by(quarter, city, mushy) %>%  # Group by segment (mushy), quarter, city
  mutate(segment_market_size = sum(servings_sold),  # Total servings in segment
         firm_market_share = firm_sales / segment_market_size) %>%  # Firm's market share in segment
  ungroup()

#Compute HHI for Each Segment (City-Quarter Level)

data_ms <- data_ms %>%
  group_by(quarter, city, mushy) %>%
  summarise(HHI = sum((firm_market_share * 100)^2)) %>%  # Sum of squared market shares
  ungroup()

unique(data_ms$HHI)

#Highly concentrated market (low competition).


# Compute the Difference in HHI Between Segments
# Reshape data to compare mushy vs non-mushy
hhi_comparison <- data_ms %>%
  pivot_wider(names_from = mushy, values_from = HHI, names_prefix = "HHI_") %>%
  mutate(HHI_Difference = HHI_1 - HHI_0)  # Difference between mushy and crunchy segments

summary_hhi <- hhi_comparison %>%
  summarise(
    avg_HHI_Mushy = mean(HHI_1, na.rm = TRUE),
    avg_HHI_Crunchy = mean(HHI_0, na.rm = TRUE),
    avg_HHI_Difference = mean(HHI_Difference, na.rm = TRUE)
  )

summary_hhi

# HHI_Mushy > HHI_NonMushy, mushy cereals are less competitive.


##### Q2

# Extract city and quarter from 'market'
data <- data %>%
  group_by(quarter, city) %>%
  mutate(market_size = city_population * 90) %>%
  ungroup()  # Important to ungroup after mutating

mushy_products <- data %>%
  filter(mushy==1)%>%
  group_by(mushy) %>%
  count(product)

crunchy_products <- data %>%
  filter(mushy==0)%>%
  group_by(mushy) %>%
  count(product)

crunchy_products

data
