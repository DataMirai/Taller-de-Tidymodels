

pacman::p_load(tidymodels,tidyverse,agua, bundle, h2o)

h2o_start()

# automl example 
auto_mod <- auto_ml() %>% 
  set_engine("h2o", max_runtime_secs = 20) %>% 
  set_mode("regression") %>% 
  fit(mpg ~ ., data = mtcars)

mtcars_wf <- h2o::as.h2o(mtcars %>% dplyr::rename(.outcome = mpg))

auto_mod %>% 
  extract_fit_engine() %>% 
  h2o::h2o.shap_summary_plot(mtcars_wf)

