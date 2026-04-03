# =========================================================

# Script: Segment 2.r

# Author: Alexander Jacob

# Created:31-03-2026

# Purpose: Compare regression models & create tree. 
# =========================================================

library(tidyverse)
library(naniar)

# ////////////////////////////////////////
# // Grabbing data and cleaning dataset
# ////////////////////////////////////////
dat <- read.csv("https://raw.githubusercontent.com/dat-analytics/assess1_t1_2026/refs/heads/main/z5745186_z5745186-Assessment1Data.csv", encoding="UTF-8")

# ============================================================
# STEP 1: Remove unwanted columns
# ============================================================
dat_clean <- dat %>%
  select(-Property_ID, -host_since, -host_identity_verified, 
         -latitude, -longitude, -minimum_nights)

cat("Original rows:", nrow(dat), "\n")
cat("Columns after removal:", ncol(dat_clean), "\n")
cat("Remaining columns:", paste(colnames(dat_clean), collapse=", "), "\n\n")

# ============================================================
# STEP 2: Clean strings and remove NA/"N/A"/empty values
# ============================================================

# Clean price column (remove $ and commas)
dat_clean$price <- as.numeric(gsub("[$,]", "", dat_clean$price))

# Replace "N/A" strings and empty strings with real NAs across all columns
dat_clean <- dat_clean %>%
  replace_with_na_all(condition = ~.x %in% c("N/A", "", " "))

rows_before_drop <- nrow(dat_clean)

# Listwise delete all rows with any NA
dat_clean <- dat_clean %>% drop_na()

rows_after_drop <- nrow(dat_clean)

cat("Rows before listwise deletion:", rows_before_drop, "\n")
cat("Rows removed:", rows_before_drop - rows_after_drop, "\n")
cat("Rows remaining:", rows_after_drop, "\n\n")

# ============================================================
# STEP 3: Remove price outliers using IQR method
# ============================================================
Q1 <- quantile(dat_clean$price, 0.25)
Q3 <- quantile(dat_clean$price, 0.75)
IQR_val <- Q3 - Q1
upper_fence <- Q3 + 1.5 * IQR_val
lower_fence <- Q1 - 1.5 * IQR_val

rows_before_outlier <- nrow(dat_clean)
dat_clean <- dat_clean %>% filter(price <= upper_fence & price >= lower_fence)

cat("Price outliers removed:", rows_before_outlier - nrow(dat_clean), "\n")
cat("Final rows remaining:", nrow(dat_clean), "\n\n")

# ============================================================
# STEP 4: Fit linear regression with all remaining variables
# ============================================================
full_model <- lm(price ~ ., data = dat_clean)
summary(full_model)

# ============================================================
# BACKWARD ELIMINATION
# ============================================================

