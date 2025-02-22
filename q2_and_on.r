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

dataDir <- paste0(setwd('/Users/bystrov/Desktop/IO/EPs/final_ep'), "/data/products.csv")
dataDir
data <- read_csv(dataDir, col_names = TRUE)

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


names(data)

#Q2

data <- data %>%
  group_by(quarter, city) %>%  
  mutate(market_size = city_population * 90) %>%  
  ungroup() %>%
  mutate(market_share = servings_sold / market_size) %>% 
  group_by(quarter, city) %>%
  mutate(outside_share = 1 - sum(market_share)) %>%  
  ungroup()

#Q3 
