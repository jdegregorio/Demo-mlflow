# The purpose of this script is to perform basic cleaning on the raw data

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(here)


# CLEAN BGT COUNTER DATA ------------------------------------------------------

# Load data
#df_bgt <- read_csv(here("data", "01_raw", "bgt_counter.csv"))
df_bgt <- read_csv("./data/01_raw/bgt_counter.csv")

# Downsample to daily counts
df_bgt <- df_bgt %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarize_all(sum) %>%
  ungroup()

# Pivot longer, parse direction/type
df_bgt <- df_bgt %>%
  select(date, starts_with("ped"), starts_with("bike")) %>%
  pivot_longer(
    -date,
    names_to = c("type", "direction"),
    names_sep = "_",
    values_to = "count"
  )

# Aggregate for total traffic by type
df_bgt <- df_bgt %>%
  group_by(date, type) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  arrange(date)

# Save clean data
#write_rds(df_bgt, here("data", "02_clean", "df_bgt.rds"))
write_rds(df_bgt, "./data/02_clean/df_bgt.rds")

# CLEAN WEATHER DATA ----------------------------------------------------------

# Load data
# df_weather <- read_delim(
#   here("data", "01_raw", "weather_daily_seattle_sandpoint.csv"),
#   delim = ";"
# )
df_weather <- read_delim(
  "./data/01_raw/weather_daily_seattle_sandpoint.csv",
  delim = ";"
)

# Clean fields
df_weather <- df_weather %>%
  rename_all(str_to_lower) %>%
  filter(
    ghcn_din == "USW00094290",
    name == "SEATTLE SAND PT WSFO"
  ) %>%
  arrange(date) %>%
  select(date, prcp, snow, tmin, tmax)

# Save clean data
# write_rds(df_weather, here("data", "02_clean", "df_weather.rds"))
write_rds(df_weather, "./data/02_clean/df_weather.rds")
