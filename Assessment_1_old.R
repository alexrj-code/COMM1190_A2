# =========================================================

# Script: z5587614_eda.r

# Author: Alexander Jacob

# Created:07-03-2026

# Purpose: Clean dataset and perform exploratory data 
#          analysis on Airbnb dataset

# =========================================================

library("readr")
library(flextable)
library(officer)
library(dplyr)
library(gt)
library(ggplot2)
library()

data <- read.csv("https://raw.githubusercontent.com/dat-analytics/
                 assess1_t1_2026/refs/heads/main/z5587614_z5587614-Assessment1
                 Data.csv", encoding="UTF-8")

dim(data)
View(data)

# ================ Global Data Cleaning ==================

### Remove Duplicates
dupes_removed <- sum(duplicated(data$Property_ID[order(rowSums(is.na(data)))]))
data <- data[order(rowSums(is.na(data))), ]
data <- data[!duplicated(data$Property_ID), ]
cat("Duplicate entries removed:", dupes_removed, "\n")
# Note: there was no duplicates lol 


### Remove hotel rooms and shared rooms
hotel_removed <- sum(data$room_type == "Hotel room", na.rm = TRUE)
shared_removed <- sum(data$room_type == "Shared room", na.rm = TRUE)
data <- data[!data$room_type %in% c("Hotel room", "Shared room"), ]
cat("Hotel room entries removed:", hotel_removed, "\n")
cat("Shared room entries removed:", shared_removed, "\n")
# Note: 48 hotels removed, 22 shared rooms removed. Only private & entire remain

### Remove $ sign in price
data$price <- as.numeric(gsub("[$,]", "", data$price))

# ================ Summary Cont. Stats Table ==================

calc_stats <- function(x, label) {
  x <- x[!is.na(x)]
  tibble(
    Variable = label,
    N        = length(x),
    Mean     = mean(x),
    Median   = median(x),
    SD       = sd(x),
    Min      = min(x),
    Max      = max(x),
    Q1       = quantile(x, 0.25),
    Q3       = quantile(x, 0.75)
  )
}

price_data        <- data$price[!is.na(data$price) & 
                                  data$price > 0 & 
                                  data$price <= 1000]

accommodates_data <- data$accommodates[!is.na(data$accommodates) & 
                                         data$accommodates > 0 & 
                                         data$accommodates <= 12]

bedrooms_data     <- data$bedrooms[!is.na(data$bedrooms) & 
                                     data$bedrooms > 0]

bathrooms_data    <- data$bathrooms[!is.na(data$bathrooms) & 
                                      data$bathrooms > 0]

min_nights_data   <- data$minimum_nights[!is.na(data$minimum_nights) & 
                                           data$minimum_nights > 0 & 
                                           data$minimum_nights <= 90]

avail_data        <- data$availability_30[!is.na(data$availability_30) & 
                                            data$availability_30 >= 0 & 
                                            data$availability_30 <= 30]

review_data       <- data$review_scores_rating[!is.na(data$review_scores_rating) & 
                                                 data$review_scores_rating > 0 & 
                                                 data$review_scores_rating <= 5]

rpm_data          <- data$reviews_per_month[!is.na(data$reviews_per_month) & 
                                              data$reviews_per_month > 0 & 
                                              data$reviews_per_month <= 31]

host_data         <- data$host_listings_count[!is.na(data$host_listings_count) & 
                                                data$host_listings_count > 0 & 
                                                data$host_listings_count <= 100]

summary_table <- bind_rows(
  calc_stats(price_data,        "Nightly Price ($)"),
  calc_stats(accommodates_data, "Accommodates (Guests)"),
  calc_stats(bedrooms_data,     "Bedrooms"),
  calc_stats(bathrooms_data,    "Bathrooms"),
  calc_stats(min_nights_data,   "Minimum Nights"),
  calc_stats(avail_data,        "Availability (Next 30 Days)"),
  calc_stats(review_data,       "Review Score"),
  calc_stats(rpm_data,          "Reviews Per Month"),
  calc_stats(host_data,         "Host Listings Count")
)

summary_table <- summary_table %>%
  mutate(across(c(Mean, Median, SD, Min, Max, Q1, Q3), ~ round(., 2)))

summary_table %>%
  gt() %>%
  tab_header(title = "Table 1: Summary Statistics For Continuous Variables") %>%
  cols_label(
    Variable = "Variable",
    N        = "Num. Datapoints",
    Mean     = "Mean",
    Median   = "Median",
    SD       = "Std. Dev.",
    Min      = "Minimum",
    Max      = "Maximum",
    Q1       = "1st Quartile",
    Q3       = "3rd Quartile"
  ) %>%
  fmt_number(columns = c(Mean, Median, SD, Min, Max, Q1, Q3), decimals = 2) %>%
  fmt_integer(columns = N) %>%
  tab_style(
    style = cell_fill(color = "#F2F2F2"),
    locations = cells_body(rows = seq(2, nrow(summary_table), by = 2))
  ) %>%
  tab_style(
    style = cell_fill(color = "#FFFFFF"),
    locations = cells_body(rows = seq(1, nrow(summary_table), by = 2))
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#2C3E50"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title()
  ) %>%
  tab_options(
    table.width = pct(100),
    heading.align = "left",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.width = px(2),
    table.border.bottom.width = px(2)
  )

ft <- flextable(summary_table)
doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = "table1.docx")

# ================ Summary Categorical Stats Table ==================
clean_cat <- function(x) x[!is.na(x) & trimws(x) != ""]

room_vals   <- clean_cat(data$room_type)
room_n      <- length(room_vals)
room_counts <- table(room_vals)
room_cats   <- c("Entire home/apt", "Private room")

super_vals   <- clean_cat(data$host_is_superhost)
super_n      <- length(super_vals)
super_counts <- table(super_vals)

book_vals   <- clean_cat(data$instant_bookable)
book_n      <- length(book_vals)
book_counts <- table(book_vals)

resp_vals   <- clean_cat(data$host_response_time)
resp_n      <- length(resp_vals)
resp_counts <- table(resp_vals)
resp_cats   <- c("within an hour", "within a few hours", "within a day", "a few days or more")

neigh_vals   <- clean_cat(data$neighbourhood_cleansed)
neigh_n      <- length(neigh_vals)
neigh_counts <- sort(table(neigh_vals), decreasing = TRUE)[1:10]

make_header_row <- function(label, n) {
  tibble(Variable = paste0(label, " (", n, ")"), N = NA_integer_, Pct = NA_real_, is_header = TRUE)
}

make_cat_row <- function(label, count, total) {
  tibble(Variable = paste0("    ", label), N = as.integer(count), 
         Pct = round(100 * count / total, 1), is_header = FALSE)
}

cat_table <- bind_rows(
  make_header_row("Room Type", room_n),
  make_cat_row("Entire home / apartment", 
               sum(room_vals %in% c("Entire home/apt", "Entire home/apartment")), room_n),
  make_cat_row("Private room", 
               sum(room_vals == "Private room"), room_n),
  
  make_header_row("Superhost Status", super_n),
  make_cat_row("Yes", sum(super_vals %in% c("t", "TRUE", "true", "True", "Yes", "yes", "1")), super_n),
  make_cat_row("No",  sum(super_vals %in% c("f", "FALSE", "false", "False", "No",  "no",  "0")), super_n),
  
  make_header_row("Instant Bookable", book_n),
  make_cat_row("Yes", sum(book_vals %in% c("t", "TRUE", "true", "True", "Yes", "yes", "1")), book_n),
  make_cat_row("No",  sum(book_vals %in% c("f", "FALSE", "false", "False", "No",  "no",  "0")), book_n),
  
  make_header_row("Host Response Time", resp_n),
  make_cat_row("Within an hour",        sum(tolower(resp_vals) == "within an hour"),        resp_n),
  make_cat_row("Within a few hours",    sum(tolower(resp_vals) == "within a few hours"),    resp_n),
  make_cat_row("Within a day",          sum(tolower(resp_vals) == "within a day"),          resp_n),
  make_cat_row("A few days or more",    sum(tolower(resp_vals) == "a few days or more"),    resp_n),
  
  make_header_row("Top 10 Suburbs (by Listing Count)", neigh_n),
  bind_rows(lapply(seq_along(neigh_counts), function(i) {
    make_cat_row(names(neigh_counts)[i], as.integer(neigh_counts[i]), neigh_n)
  }))
)

cat_display <- cat_table %>%
  mutate(
    N_display   = ifelse(is_header, "", as.character(N)),
    Pct_display = ifelse(is_header, "", paste0(Pct, "%"))
  ) %>%
  select(Variable, N_display, Pct_display, is_header)

ft2 <- flextable(cat_display %>% select(Variable, N_display, Pct_display)) %>%
  set_header_labels(
    Variable    = "Variable",
    N_display   = "Num. Datapoints",
    Pct_display = "% of Eligible Datapoints"
  ) %>%
  
  bg(i = which(cat_display$is_header), bg = "#2C3E50", part = "body") %>%
  color(i = which(cat_display$is_header), color = "white", part = "body") %>%
  bold(i = which(cat_display$is_header), part = "body") %>%
  
  bg(i = which(!cat_display$is_header & (seq_along(cat_display$is_header) %% 2 == 0)),
     bg = "#F2F2F2", part = "body") %>%
  bg(i = which(!cat_display$is_header & (seq_along(cat_display$is_header) %% 2 != 0)),
     bg = "#FFFFFF", part = "body") %>%
  
  bg(part = "header", bg = "#2C3E50") %>%
  color(part = "header", color = "white") %>%
  bold(part = "header") %>%
  
  width(j = 1, width = 3.2) %>%
  width(j = 2, width = 1.4) %>%
  width(j = 3, width = 2.0) %>%
  
  border_outer(part = "all", border = fp_border(color = "#CCCCCC", width = 1)) %>%
  border_inner_h(border = fp_border(color = "#CCCCCC", width = 0.5), part = "body") %>%
  
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  
  set_caption("Table 2: Summary Statistics For Categorical Variables") %>%
  align(j = 2:3, align = "center", part = "all")


doc2 <- read_docx() %>% body_add_flextable(ft2)
print(doc2, target = "table2.docx")

# ================ Correlation Table ==================
library(dplyr)
library(flextable)
library(officer)

CBD_LAT <- -33.86785
CBD_LON <- 151.20732

# --- Row 1: Distance from CBD vs Price Per Guest Per Night ---
r1_data <- data %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(price), !is.na(accommodates),
         latitude != 0, longitude != 0, price > 0, accommodates > 0) %>%
  mutate(
    delta_lat  = latitude - CBD_LAT,
    delta_lon  = longitude - CBD_LON,
    distance   = sqrt(delta_lat^2 + delta_lon^2),
    price_per_guest = price / accommodates
  )
cor1 <- round(cor(r1_data$distance, r1_data$price_per_guest, method = "pearson"), 3)

# --- Row 2: Bedrooms vs Total Price Per Night ---
r2_data <- data %>%
  filter(!is.na(bedrooms), !is.na(price), bedrooms > 0, price > 0)
cor2 <- round(cor(r2_data$bedrooms, r2_data$price, method = "pearson"), 3)

# --- Row 3: Bathrooms vs Total Price Per Night ---
r3_data <- data %>%
  filter(!is.na(bathrooms), !is.na(price), bathrooms > 0, price > 0)
cor3 <- round(cor(r3_data$bathrooms, r3_data$price, method = "pearson"), 3)

# --- Row 4: Price Per Guest Per Night vs Review Score ---
r4_data <- data %>%
  filter(!is.na(price), !is.na(accommodates), !is.na(review_scores_rating),
         price > 0, accommodates > 0, review_scores_rating > 0) %>%
  mutate(price_per_guest = price / accommodates)
cor4 <- round(cor(r4_data$price_per_guest, r4_data$review_scores_rating, method = "pearson"), 3)

# --- Row 5: Host Listings Count vs Review Score ---
r5_data <- data %>%
  filter(!is.na(host_listings_count), !is.na(review_scores_rating),
         host_listings_count > 0, review_scores_rating > 0)
cor5 <- round(cor(r5_data$host_listings_count, r5_data$review_scores_rating, method = "pearson"), 3)

# --- Build table ---
cor_table <- tibble(
  Comparison = c(
    "Distance from CBD vs. Price Per Guest Per Night",
    "Number of Bedrooms vs. Total Price Per Night",
    "Number of Bathrooms vs. Total Price Per Night",
    "Price Per Guest Per Night vs. Review Score",
    "Host Listings Count vs. Review Score"
  ),
  Pearson_r = c(cor1, cor2, cor3, cor4, cor5)
)

# --- Build flextable ---
ft3 <- flextable(cor_table) %>%
  set_header_labels(
    Comparison = "Variables Compared",
    Pearson_r  = "Pearson Correlation Coefficient (r)"
  ) %>%
  # Alternating row shading
  bg(i = seq(1, nrow(cor_table), by = 2), bg = "#FFFFFF", part = "body") %>%
  bg(i = seq(2, nrow(cor_table), by = 2), bg = "#F2F2F2", part = "body") %>%
  # Column header styling
  bg(part = "header", bg = "#2C3E50") %>%
  color(part = "header", color = "white") %>%
  bold(part = "header") %>%
  # Column widths
  width(j = 1, width = 3.8) %>%
  width(j = 2, width = 2.4) %>%
  # Borders
  border_outer(part = "all", border = fp_border(color = "#CCCCCC", width = 1)) %>%
  border_inner_h(border = fp_border(color = "#CCCCCC", width = 0.5), part = "body") %>%
  # Font
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  # Alignment
  align(j = 2, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  set_caption("Table 3: Pearson Correlation Coefficients Between Continuous Variables")

doc3 <- read_docx() %>% body_add_flextable(ft3)
print(doc3, target = "table3.docx")

# ================ Plots ==================

# --- Consistent theme ---
plot_theme <- theme_minimal(base_family = "Arial") +
  theme(
    plot.title       = element_text(size = 13, face = "bold", hjust = 0.5, color = "black"),
    axis.title       = element_text(size = 11, color = "black"),
    axis.text        = element_text(size = 9,  color = "black"),
    panel.grid.major = element_line(color = "#DDDDDD"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# -------------------------------------------------------
# Plot 1: Price Spread Based on Property Type
# -------------------------------------------------------
p1_data <- data %>%
  filter(
    room_type %in% c("Entire home/apt", "Private room"),
    !is.na(price), price > 0
  ) %>%
  mutate(room_label = case_when(
    room_type == "Entire home/apt" ~ "Entire Home / Apt",
    room_type == "Private room"    ~ "Private Room"
  ))

p1 <- ggplot(p1_data, aes(x = price, y = room_label, fill = room_label)) +
  geom_boxplot(color = "black", outlier.shape = 16, outlier.colour = "blue") +
  scale_fill_manual(values = c("Entire Home / Apt" = "#AED6F1",
                               "Private Room"      = "#D5D8DC")) +
  scale_x_continuous(limits = c(0, 500),
                     labels = dollar_format(prefix = "$")) +
  #scale_x_log10(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Plot 1: Price Spread Based on Property Type",
    x     = "Total Price Per Night",
    y     = NULL
  ) +
  plot_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10, face = "bold"))

ggsave("plot111.png", plot = p1, width = 8, height = 4, dpi = 150)

# -------------------------------------------------------
# Plot 2: Median Price and Frequency by Occupancy
# -------------------------------------------------------
p2_data <- data %>%
  filter(!is.na(accommodates), accommodates > 0,
         !is.na(price), price > 0) %>%
  mutate(bin = case_when(
    accommodates == 1                       ~ "1",
    accommodates == 2                       ~ "2",
    accommodates == 3                       ~ "3",
    accommodates == 4                       ~ "4",
    accommodates >= 5  & accommodates <= 7  ~ "5–7",
    accommodates >= 8  & accommodates <= 9  ~ "8–9",
    accommodates >= 10 & accommodates <= 12 ~ "10–12",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(bin)) %>%
  mutate(bin = factor(bin, levels = c("1","2","3","4","5–7","8–9","10–12")))

p2_summary <- p2_data %>%
  group_by(bin) %>%
  summarise(freq = n(), median_price = median(price), .groups = "drop")

scale_factor <- max(p2_summary$freq) / max(p2_summary$median_price)

p2 <- ggplot(p2_summary, aes(x = bin)) +
  geom_col(aes(y = freq), fill = "#AED6F1", color = "black", width = 0.6) +
  geom_line(aes(y = median_price * scale_factor, group = 1),
            color = "red", linewidth = 0.9) +
  geom_point(aes(y = median_price * scale_factor),
             color = "red", size = 3) +
  scale_y_continuous(
    name = "Frequency",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Median Price Per Night",
                        labels = dollar_format(prefix = "$"))
  ) +
  labs(
    title = "Plot 2: Price and Frequency by Occupancy",
    x     = "Maximum Occupant Capacity"
  ) +
  plot_theme +
  theme(axis.title.y.right = element_text(color = "red"),
        axis.text.y.right  = element_text(color = "red"))

ggsave("plot2.png", plot = p2, width = 8, height = 5, dpi = 150)
# -------------------------------------------------------
# Plot 3: Median Price by Number of Bedrooms (bar Chart)
# -------------------------------------------------------
p3_data <- data %>%
  filter(!is.na(price), !is.na(bedrooms), price > 0, price <= 1000,
         bedrooms == floor(bedrooms)) %>%
  mutate(bedroom_group = case_when(
    bedrooms >= 7 ~ "7+",
    TRUE ~ as.character(as.integer(bedrooms))
  )) %>%
  group_by(bedroom_group) %>%
  summarise(median_price = median(price), n = n()) %>%
  filter(bedroom_group %in% c("1", "2", "3", "4", "5", "6", "7+")) %>%
  mutate(bedroom_group = factor(bedroom_group, levels = c("1", "2", "3", "4", "5", "6", "7+")))

p3_data <- p3_data %>%
  mutate(bedroom_group = factor(bedroom_group, 
                                levels = c("1", "2", "3", "4", "5", "6", "7+")))

p3 <- ggplot(p3_data, aes(x = bedroom_group, y = median_price)) +
  geom_col(fill = "#AED6F1", color = "black", width = 0.6) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = "Plot 3: Median Nightly Price by Number of Bedrooms",
       x = "Bedrooms", y = "Median Price Per Night")

ggsave("plot3.png", plot = p3, width = 8, height = 5, dpi = 150)

cat("All three plots saved.\n")

# -------------------------------------------------------
# Plot 4: MAP!!!
# -------------------------------------------------------
library(maptiles)
library(tidyterra)
library(sf)
library(ggplot2)

map_data <- data %>%
  filter(!is.na(latitude), !is.na(longitude),
         !is.na(price), price > 0, price <= 1000)

# Define the bounding box around Sydney listings
bbox <- st_bbox(c(
  xmin = min(map_data$longitude) - 0.05,
  xmax = max(map_data$longitude) + 0.05,
  ymin = min(map_data$latitude)  - 0.05,
  ymax = max(map_data$latitude)  + 0.05
), crs = st_crs(4326))

# Download the map tiles
sydney_map <- get_tiles(bbox, provider = "CartoDB.Positron", zoom = 11)

# Plot
p4 <- ggplot() +
  geom_spatraster_rgb(data = sydney_map) +
  geom_point(data = map_data,
             aes(x = longitude, y = latitude, color = price),
             alpha = 0.5, size = 0.8) +
  scale_color_gradient(low = "blue", high = "red",
                       labels = dollar_format(prefix = "$"),
                       name = "Nightly Price") +
  labs(title = "Plot 4: Geographic Distribution of Listings by Price",
       x = "Longitude", y = "Latitude") +
  plot_theme

ggsave("plot4.png", plot = p4, width = 8, height = 7, dpi = 300)

