# Sberbank housing data
# 00_pre_process.R

pre_process <- function(df) {
  
  require(dplyr)
  require(readr)
  require(assertthat)
  
  # Join dataset with macro using timestamp
  macro_cols <- c("timestamp",
                  "balance_trade",
                  "balance_trade_growth",
                  "eurrub",
                  "average_provision_of_build_contract",
                  "micex_rgbi_tr",
                  "micex_cbi_tr", 
                  "deposits_rate", 
                  "mortgage_value", 
                  "mortgage_rate",
                  "income_per_cap", 
                  "museum_visitis_per_100_cap",
                  "cpi",
                  "apartment_build")
  
  df <- df %>%
    left_join(macro[, macro_cols], by = c("timestamp" = "timestamp"))
  
  # Replace errors in data with na
  df$state <- ifelse(as.integer(df$state) > 4, NA, df$state)
  df$build_year <- ifelse(as.integer(df$build_year) < 1860 | 
                            as.integer(df$build_year) > 2018, 
                          NA, df$build_year)
  df$full_sq <- ifelse(df$full_sq > 2000, NA, df$full_sq)
  df$num_room <- ifelse(df$num_room == 0, NA, df$num_room)
  
  # Assert correct
  #assert_that(is.na(df$state[10090]))
  #assert_that(is.na(df$build_year[28555]))
  #assert_that(is.na(df$full_sq[3528]))
  #assert_that(is.na(df$num_room[27455]))
  
  
  # Set correct data types
  
  # Integers
  to_int <- c("build_year", "max_floor", "num_room", "kitch_sq")
  df[, to_int] <- lapply(df[, to_int], as.integer)
  
  # Factors
  to_fact <- c("state", "material", "product_type", "sub_area", "ecology")
  df[, to_fact] <- lapply(df[, to_fact], function(x) ifelse(is.na(x), "NA", x))
  
  # One hot encoding is handled by model.matrix in later stages
  
  # Binary values
  to_bin <- c("culture_objects_top_25", "thermal_power_plant_raion", 
              "incineration_raion", "oil_chemistry_raion", "radiation_raion", 
              "railroad_terminal_raion", "big_market_raion", 
              "nuclear_reactor_raion", "detention_facility_raion", 
              "water_1line", "big_road1_1line", "railroad_1line")
  df[, to_bin] <- lapply(df[, to_bin], 
                         function(x) ifelse(x == "yes", 1, 
                                            ifelse(x == "no", 0, NA)))
  

  # Handle missing numerical values

  # Median imputation?
  
  # Function to trim extreme values?
  
  # Output data frame
  df
}

