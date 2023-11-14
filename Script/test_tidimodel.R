
pacman::p_load(tidymodels,tidyverse, agua, h2o)

# ejemplo h2o (funciona)----
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



# ejemplo 2 (falla) ----

datos <- readxl::read_xlsx('Data/datos.xlsx') %>% 
  filter(!is.na(Var_respuesta) ) %>% 
  mutate(Var_respuesta= as_factor(Var_respuesta)) %>% 
  rename(.outcome = Var_respuesta)


install.packages("h2o", type = "source", repos = "https://h2o-release.s3.amazonaws.com/h2o/rel-3.42.0/4/R")


datos$.outcome %>%  unique()

h2o_start()

h2o_end()
library(h2o)

h2o.init()


datos_x <- h2o::as.h2o(datos) 
datos_x$.outcome <-  h2o.asfactor(datos_x$.outcome)
  
# automl example ----
auto_mod <- auto_ml() %>% 
  set_engine("h2o", max_runtime_secs = 200) %>% 
  set_mode("classification") %>% 
  fit(.outcome ~ ., data = datos_x)

Var_respuesta_wf <- h2o::as.h2o(datos %>% dplyr::rename(.outcome = Var_respuesta))
h2o.asfactor()


auto_mod %>% 
  extract_fit_engine() %>% 
  h2o::h2o.shap_summary_plot(Var_respuesta_wf)

h2o_end()
