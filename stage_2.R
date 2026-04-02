# =========================================================

# Script: Segment 2.r

# Author: Alexander Jacob

# Created:31-03-2026

# Purpose: Compare regression models & create tree. 
# =========================================================

library("readr")
library(flextable)
library(officer)
library(dplyr)
library(gt)
library(ggplot2)
library(tidyverse)
library()

# ////////////////////////////////////////
# // Grabbing data and cleaning dataset
# ////////////////////////////////////////
data <- read.csv("https://raw.githubusercontent.com/dat-analytics/assess1_t1_2026/refs/heads/main/z5745186_z5745186-Assessment1Data.csv", encoding="UTF-8")

data_clean <- data %>% select(price, bathrooms, bedrooms, room_type, host_response_time, host_is_superhost, number_of_reviews_l30d, maximum_nights, instant_bookable)
data_clean <- data_clean %>% drop_na()
View(data_clean)

data_clean$price <- as.numeric(gsub("[$,]", "", data_clean$price))

Q1 <- quantile(data_clean$price, 0.25, na.rm = TRUE)
Q3 <- quantile(data_clean$price, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
fence <- Q3 + 1.5 * IQR_val

cat("Upper fence:", fence, "\n")
cat("Rows retained:", sum(data_clean$price <= fence, na.rm = TRUE), "\n")
cat("Rows removed:", sum(data_clean$price > fence, na.rm = TRUE), "\n")

lower_fence <- Q1 - 1.5 * IQR_val
cat("Lower fence:", lower_fence, "\n")
cat("Rows removed below fence:", sum(data_clean$price < lower_fence, na.rm = TRUE), "\n")

data_clean <- data_clean %>% filter(price <= fence & price >= lower_fence)
data_clean <- data_clean %>%
  filter(host_response_time != "N/A",
         host_response_time != "",
         host_is_superhost != "")

# ////////////////////////////////////////
# //Linear Regression Models
# ////////////////////////////////////////

## Base Model
base_model <- lm(price ~ bathrooms + bedrooms + room_type, data = data_clean)
summary(base_model)

## Model D
model_d <- lm(price ~ bathrooms + bedrooms + room_type + instant_bookable + host_is_superhost, data = data_clean)
summary(model_d)
