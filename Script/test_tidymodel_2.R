
library(tidymodels)
library(tidyverse)
library(agua)
library(bundle)
library(h2o)

set.seed(4595)



concrete_split <- initial_split(concrete, strata = compressive_strength)
concrete_train <- training(concrete_split)
concrete_test <- testing(concrete_split)

# run for a maximum of 120 seconds
auto_spec <-
  rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

normalized_rec <-
  recipe(compressive_strength ~ ., data = concrete_train) %>%
  step_normalize(all_predictors())

auto_wflow <-
  workflow() %>%
  add_model(auto_spec) %>%
  add_recipe(normalized_rec)

auto_fit <- fit(auto_wflow, data = concrete_train)
auto_fit_bundle <- bundle(auto_fit)
saveRDS(auto_fit_bundle, file = "test.h2o.rand_forest_bundled.rds") #save the object

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

# readRDS("test.h2o.rand_forest_bundled.rds") %>%  unbundle()

h2o_start()
auto_fit_bundle <- readRDS("test.h2o.rand_forest_bundled.rds")
auto_fit <- unbundle(auto_fit_bundle)
predict(auto_fit, new_data = concrete_test)

h2o.explain(auto_fit,  as_h2o(concrete_test))

h2o_end()

