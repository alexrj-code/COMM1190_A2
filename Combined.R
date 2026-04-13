# =========================================================
# =========================================================
# =============== COMBINED GROUP R SCRIPT =================
# =========================================================
# =========================================================
# =========================================================

# ==============================================================================
# ==============================================================================
# Author: Lokisha Phuyal 
# Purpose: Create base LRM model and extension A. Utilised other group members'
# stage 1 notes to create comparisons. 
# ==============================================================================
# ==============================================================================
install.packages("readr") install.packages("dplyr") library("readr") commdata2 <- read.csv("https://raw.githubusercontent.com/dat-analytics/assess1_t1_2026/refs/heads/main/z5745186_z5745186-Assessment1Data.csv", encoding="UTF-8") dim(commdata2) library(dplyr) model_data <- commdata2 %>% select(price, bathrooms, bedrooms, room_type, host_response_time, host_is_superhost)
# Cleaning Data
library(dplyr)
# Keeping relevant variables for base model and model A
model_data <- commdata2 %>% select(price, bathrooms, bedrooms, room_type, host_response_time, host_is_superhost)
# Clean price
model_data <- model_data %>% mutate( price = gsub("\$", "", price), price = gsub(",", "", price), price = as.numeric(price) )
# Clean categorical variables
model_data <- model_data %>% mutate( room_type = na_if(room_type, ""), room_type = na_if(room_type, "N/A"), host_response_time = na_if(host_response_time, ""), host_response_time = na_if(host_response_time, "N/A"), host_is_superhost = na_if(host_is_superhost, ""), host_is_superhost = na_if(host_is_superhost, "N/A"), room_type = as.factor(room_type), host_response_time = as.factor(host_response_time), host_is_superhost = as.factor(host_is_superhost) )
# Listwise deletion
model_data <- model_data %>% filter( !is.na(price), !is.na(bathrooms), !is.na(bedrooms), !is.na(room_type), !is.na(host_response_time), !is.na(host_is_superhost) )
# Removing price outliers
model_data <- model_data %>% filter(price <= 613)
nrow(model_data)
base_model <- lm(price ~ bathrooms + bedrooms + room_type, data = model_data) summary(base_model)
model_A <- lm(price ~ bathrooms + bedrooms + room_type + host_response_time + host_is_superhost, data = model_data) summary(model_A)
summary(base_model)$adj.r.squared summary(model_A)$adj.r.squared 
# Calculating RMSE
predicted <- predict(model_A, model_data) actual <- model_data$price rmseA <- sqrt(mean((actual-predicted)^2)) rmseA
predicted_base <- predict(base_model, model_data) actual <- model_data$price rmse_base <- sqrt(mean((actual-predicted_base)^2)) rmse_base


# ==============================================================================
# ==============================================================================
# Author: Alexander Jacob
# Purpose: Create 'best' Linear Regression Model & Evaluate
# ==============================================================================
# ==============================================================================

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

dat_clean <- dat_clean %>%
  mutate(price = parse_number(price)) %>%
  replace_with_na_all(condition = ~.x %in% c("N/A", "", " ")) %>%
  drop_na()

rows_before <- nrow(dat_clean)
dat_clean <- dat_clean %>% drop_na()
cat(sprintf("Removed %d rows, %d remaining\n", rows_before - nrow(dat_clean), nrow(dat_clean)))

# ============================================================
# STEP 3: Remove price outliers using IQR method (WITH OUTLIER BOUNDARIES FROM STAGE 1)
# ============================================================

dat_price_bound <- dat %>% 
  select(price, bathrooms, bedrooms, room_type, host_response_time, 
         host_is_superhost, number_of_reviews_l30d, maximum_nights, instant_bookable) %>%
  drop_na() %>%
  mutate(price = as.numeric(gsub("[$,]", "", price)))

Q1 <- quantile(dat_price_bound$price, 0.25, na.rm = TRUE)
Q3 <- quantile(dat_price_bound$price, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
upper_fence <- Q3 + 1.5 * IQR_val
lower_fence <- Q1 - 1.5 * IQR_val

rows_before_outlier <- nrow(dat_clean)
dat_clean <- dat_clean %>% filter(price >= lower_fence & price <= upper_fence)
cat("Price outliers removed:", rows_before_outlier - nrow(dat_clean), "\n")
cat("Final rows remaining:", nrow(dat_clean), "\n\n")

# ============================================================
# CREATE SYDNEY REGION COLUMN
# ============================================================

region_lookup <- c(
  # CBD / Inner City
  "Sydney" = "CBD / Inner City",
  
  # Inner West
  "Leichhardt" = "Inner West",
  "Marrickville" = "Inner West",
  "Canada Bay" = "Inner West",
  "Burwood" = "Inner West",
  "Strathfield" = "Inner West",
  "Ashfield" = "Inner West",
  
  # Eastern Suburbs
  "Waverley" = "Eastern Suburbs",
  "Woollahra" = "Eastern Suburbs",
  "Randwick" = "Eastern Suburbs",
  
  # North Shore (merged)
  "Mosman" = "North Shore",
  "North Sydney" = "North Shore",
  "Lane Cove" = "North Shore",
  "Hunters Hill" = "North Shore",
  "Ku-Ring-Gai" = "North Shore",
  "Hornsby" = "North Shore",
  "Ryde" = "North Shore",
  "Willoughby" = "North Shore",
  
  # Northern Beaches
  "Manly" = "Northern Beaches",
  "Pittwater" = "Northern Beaches",
  "Warringah" = "Northern Beaches",
  
  # Western Sydney
  "Parramatta" = "Western Sydney",
  "Blacktown" = "Western Sydney",
  "Auburn" = "Western Sydney",
  "Holroyd" = "Western Sydney",
  "The Hills Shire" = "Western Sydney",
  "Penrith" = "Western Sydney",
  
  # South West Sydney
  "Liverpool" = "South West Sydney",
  "Fairfield" = "South West Sydney",
  "Bankstown" = "South West Sydney",
  "Camden" = "South West Sydney",
  "Campbelltown" = "South West Sydney",
  "Canterbury" = "South West Sydney",
  
  # South / Sutherland & St George
  "Sutherland Shire" = "South / Sutherland & St George",
  "Hurstville" = "South / Sutherland & St George",
  "Rockdale" = "South / Sutherland & St George",
  "City Of Kogarah" = "South / Sutherland & St George",
  "Botany Bay" = "South / Sutherland & St George"
)

# Create the new column
dat_clean$sydney_region <- region_lookup[dat_clean$neighbourhood_cleansed]

# ============================================================
# CHECK EVERY ENTRY HAS A REGION
# ============================================================
unmatched <- dat_clean %>%
  filter(is.na(sydney_region)) %>%
  select(neighbourhood_cleansed) %>%
  distinct()

if (nrow(unmatched) == 0) {
  cat("All entries successfully mapped to a Sydney region.\n")
} else {
  cat("WARNING:", nrow(unmatched), "neighbourhood(s) could not be matched:\n")
  print(unmatched)
}

# Summary of listings per region
cat("\nListings per region:\n")
print(table(dat_clean$sydney_region))

# Drop original neighbourhood column
dat_clean <- dat_clean %>% select(-neighbourhood_cleansed)


# ============================================================
# TRAIN/TEST SPLIT
# ============================================================
set.seed(123)
split <- sample(c(TRUE, FALSE), nrow(dat_clean), replace=TRUE, prob=c(0.8,0.2))
train_data <- dat_clean[split, ]
test_data  <- dat_clean[!split, ]

cat("Training rows:", nrow(train_data), "\n")
cat("Test rows:", nrow(test_data), "\n\n")

# ============================================================
# STEP 4: Fit linear regression with all remaining variables
# ============================================================
full_model <- lm(price ~ ., data = train_data)  
summary(full_model)

# ============================================================
# BACKWARD ELIMINATION TO FIND BEST LINEAR REGRESSION MODEL
# ============================================================
current_data <- train_data  # <-- changed
current_model <- lm(price ~ ., data = current_data)
current_adj_r2 <- summary(current_model)$adj.r.squared
iteration <- 0

cat("Starting Adjusted R²:", round(current_adj_r2, 6), "\n")
cat("Starting variables:", paste(names(current_data)[names(current_data) != "price"], collapse=", "), "\n\n")

repeat {
  current_model <- lm(price ~ ., data = current_data)
  
  ## DELETE THIS
  summary(current_model)
  ##
  coef_table <- summary(current_model)$coefficients
  coef_table <- coef_table[rownames(coef_table) != "(Intercept)", , drop = FALSE]
  coef_table <- coef_table[order(-coef_table[, "Pr(>|t|)"]), , drop = FALSE]
  
  candidate_vars <- names(current_data)[names(current_data) != "price"]
  removed <- FALSE
  
  for (i in 1:nrow(coef_table)) {
    max_pval <- coef_table[i, "Pr(>|t|)"]
    
    if (max_pval < 0.05) {
      cat("All remaining variables significant (p < 0.05). Stopping.\n")
      removed <- NA
      break
    }
    
    worst_coef <- rownames(coef_table)[i]
    
    matched_var <- candidate_vars[sapply(candidate_vars, function(v) startsWith(worst_coef, v))]
    if (length(matched_var) > 1) matched_var <- matched_var[which.max(nchar(matched_var))]
    if (length(matched_var) == 0) next
    
    all_dummies_for_var <- rownames(coef_table)[sapply(rownames(coef_table), function(r) startsWith(r, matched_var))]
    
    if (length(all_dummies_for_var) > 1) {
      any_significant <- any(coef_table[all_dummies_for_var, "Pr(>|t|)"] < 0.05)
      if (any_significant) {
        cat(sprintf("Skipping %s — has insignificant dummy (%s) but other dummies are significant\n",
                    matched_var, worst_coef))
        next
      }
    }
    
    new_data <- current_data %>% select(-all_of(matched_var))
    new_model <- lm(price ~ ., data = new_data)
    new_adj_r2 <- summary(new_model)$adj.r.squared
    
    iteration <- iteration + 1
    cat(sprintf("Iteration %d | Removed: %-35s | p-value: %.5f | Adj R²: %.6f %s\n",
                iteration, matched_var, max_pval, new_adj_r2,
                ifelse(new_adj_r2 >= current_adj_r2, "", "(decreased — reverting)")))
    
    # Stop if Adjusted R² decreases at all
    if (new_adj_r2 < current_adj_r2) {
      cat("\nRemoving", matched_var, "worsens the model. Keeping it. Best model found.\n")
      removed <- NA
      break
    }
    
    current_data <- new_data
    current_adj_r2 <- new_adj_r2
    removed <- TRUE
    break
  }
  
  if (is.na(removed) || !removed) break
}

best_model <- lm(price ~ ., data = current_data)
cat("\n--- FINAL MODEL ---\n")
cat("Variables kept:", paste(names(current_data)[names(current_data) != "price"], collapse=", "), "\n\n")
summary(best_model)

## I don't trust the loop - Manually checking 
#test_model <- lm(price ~ . - host_response_time - instant_bookable, data = current_data)
#summary(test_model)

## ok the loop seems to work

# ============================================================
# Evaluate the model
# ============================================================

## Eval 1: MSE
predictions <- predict(best_model, newdata = test_data)
test_mse <- mean((test_data$price - predictions)^2)
test_rmse <- sqrt(test_mse)
cat("Test RMSE: $", round(test_rmse, 2), "\n")

## Eval2: R^2 (on unseen data)
ss_res <- sum((test_data$price - predictions)^2)
ss_tot <- sum((test_data$price - mean(test_data$price))^2)
test_r2 <- 1 - ss_res/ss_tot
cat("Test R²:", round(test_r2, 4), "\n")

## Eval3: Actual vs predicted plot
ggplot(data.frame(actual=test_data$price, predicted=predictions), 
       aes(x=actual, y=predicted)) +
  geom_point(alpha=0.3) +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  labs(title="Actual vs Predicted Price",
       x="Actual Price ($)", y="Predicted Price ($)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


