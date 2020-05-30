# The purpose of this script is to prepare data for modeling

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(tis)
library(here)

# Load data
df_bgt <- read_rds(here("data", "02_clean", "df_bgt.rds"))
df_weather <- read_rds(here("data", "02_clean", "df_weather.rds"))

# ENGINEER DATE FEATURES ------------------------------------------------------

# Extract data features
df_date <- df_bgt %>%
  select(date) %>%
  distinct() %>%
  mutate(
    wday = wday(date, label = TRUE) %>% str_to_lower(),
    weekend = wday %in% c("sat", "sun"),
    holiday = isHoliday(date)
  )

# PREPARE FEATURE DATA --------------------------------------------------------

# Join all feature data
df_feat <- df_bgt %>%
  select(date) %>%
  distinct(date) %>%
  left_join(df_date, by = "date") %>%
  left_join(df_weather, by = "date")


# PREPARE FINAL DATASETS ------------------------------------------------------

# Nest data
grid_data <- df_bgt %>%
  left_join(df_feat, by = "date") %>%
  group_by(type) %>%
  drop_na() %>%
  nest() %>%
  ungroup()

# Save data
map2(
  grid_data$data, 
  grid_data$type,
  ~write_rds(.x, here("data", "03_prepared", paste0("df_", .y, ".rds")))
)
