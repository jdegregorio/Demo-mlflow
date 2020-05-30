# The purpose of this script is to generate a model

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(here)
library(recipes)
library(parsnip)
library(rsample)
library(yardstick)
library(reticulate)

# Load mlflow library
mlflow <- import("mlflow")

# Load data
df_all <- read_rds(here("data", "03_prepared", "df_bike.rds"))

# Initiate experiment
mlflow$log_param("alpha", 4)
mlflow$log_param("beta", 2)

mlflow$log_metric("alpha", 100)
mlflow$log_metric("alpha", 50)
mlflow$log_metric("alpha", 10)


# CREATE PIPELINE & TRAINING FUNCTINOS ----------------------------------------

# Define recipe
recipe_01 <- 
  recipe(
    count ~ .,
    data = df_all %>% select(-date)
  ) %>%
  step_dummy(all_nominal()) %>%
  step_mutate_at(all_predictors(), fn = as.numeric) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# Split test (holdout) data
split_init <- initial_split(df_all, prop = 0.8)
df_train <- training(split_init)
df_test  <- testing(split_init)

# # Dev Args
# obj_split <- grid_cv$splits[[1]]
# obj_recipe_template <- recipe_01

# Define function to prepare receipe
cv_prep <- function(obj_split, obj_recipe_template) {
  
  # Extract training data
  data_train <- training(obj_split)
  
  # Prepare recipe
  obj_recipe_prepped <- prep(obj_recipe_template, training = data_train)
  
  return(obj_recipe_prepped)
}

# Define fitting function
cv_fit <- function(obj_split, obj_recipe_prepped) {
  
  # Define model
  mod_spec <- 
    linear_reg() %>%
    set_engine("lm")
  
  # Split and prepare training data
  data_train <- training(obj_split)
  
  # Apply recipe to data
  data_train <- bake(obj_recipe_prepped, new_data = data_train)
  
  # Fit model
  mod_fit <- fit(mod_spec, count ~ ., data = data_train)
  
  return(mod_fit)
}

# Define function to predict
cv_pred <- function(obj_split, obj_recipe_prepped, mod_fit){

  # Extract and prepare validation data
  df_valid <- testing(obj_split)
  df_valid <- bake(obj_recipe_prepped, new_data = df_valid)
  
  # Make predictions
  df_pred <- tibble(
    truth = df_valid$count,
    estimate = predict(mod_fit, new_data = df_valid, type = "numeric") %>% pull(.pred)
  )
  
  return(df_pred)
}


# RUN CROSS VALIDATION --------------------------------------------------------

# Create repeated CV samples
df_cv <- 
  rsample::vfold_cv(df_train, v = 10, repeats = 1) %>%
  mutate(
    obj_recipe_prepped = map(splits, cv_prep, obj_recipe_template = recipe_01),
    mod_fit = map2(splits, obj_recipe_prepped, cv_fit),
    df_pred = pmap(list(splits, obj_recipe_prepped, mod_fit), cv_pred),
    df_metric = map(df_pred, metrics, truth = truth, estimate = estimate),
    df_metric = map(df_metric,~.x %>% select(.metric, .estimate) %>% pivot_wider(names_from = ".metric", values_from = ".estimate"))
  ) %>%
  unnest(df_metric) %>%
  select(id, rmse, rsq, mae)
