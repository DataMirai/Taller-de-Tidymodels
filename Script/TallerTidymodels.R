# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Librerias | Carga de datos ----
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  readxl,      # Lectura de los datos
  tidyverse ,  # Acceso al entorno de procesamiento "tidyverse"
  tidymodels,   # Acceso al entorno de modelado "tidymodels"
  themis
)

datos <- read_xlsx('Data/datos.xlsx') %>% 
  filter(!is.na(Var_respuesta) )

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# Splits ----
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

## No strat ----
data_split_no_strat <- initial_split(datos, prop = 0.6 )
# Put 3/4 of the data into the training set 
# Create data frames for the two sets:

ggpubr::ggarrange(
  training(data_split_no_strat) %>% 
    group_by(Var_respuesta) %>% 
    summarise(recuento = n()) %>% 
    ungroup() %>% 
    mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
    ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(prop_var_respuesta)),
              position = position_stack(vjust = 0.5),
              size = 4) +
    scale_fill_manual( values=c("azure4", "azure3"))
  ,
  testing(data_split_no_strat) %>% 
    group_by(Var_respuesta) %>% 
    summarise(recuento = n()) %>% 
    mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
    ungroup() %>% 
    ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(prop_var_respuesta)),
              position = position_stack(vjust = 0.5),
              size = 4)  +
    scale_fill_manual( values=c("azure4", "azure3")),
  common.legend = T, legend = 'bottom'
)

## Strat ----
data_split_strat <- initial_split(datos, prop = 0.60, strata = Var_respuesta)


ggpubr::ggarrange(
  training(data_split_strat) %>% 
    group_by(Var_respuesta) %>% 
    summarise(recuento = n()) %>% 
    ungroup() %>% 
    mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
    ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(prop_var_respuesta)),
              position = position_stack(vjust = 0.5),
              size = 4)+
    scale_fill_manual( values=c("#E86EB7", "#108064"))
  ,
  testing(data_split_strat) %>% 
    group_by(Var_respuesta) %>% 
    summarise(recuento = n()) %>% 
    mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
    ungroup() %>% 
    ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(prop_var_respuesta)),
              position = position_stack(vjust = 0.5),
              size = 4)+
    scale_fill_manual( values=c("#E86EB7", "#108064")), 
  common.legend = T,
  legend = 'bottom'
)
## Comparacion ----
ggpubr::ggarrange(
  ggpubr::ggarrange(
    training(data_split_no_strat) %>% 
      group_by(Var_respuesta) %>% 
      summarise(recuento = n()) %>% 
      ungroup() %>% 
      mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
      ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(prop_var_respuesta)), position = position_stack(vjust = 0.5), size = 4) +
    scale_fill_manual( values=c("azure4", "azure3"))
    ,
    testing(data_split_no_strat) %>% 
      group_by(Var_respuesta) %>% 
      summarise(recuento = n()) %>% 
      mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
      ungroup() %>% 
      ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(prop_var_respuesta)), position = position_stack(vjust = 0.5), size = 4) +
      scale_fill_manual( values=c("azure4", "azure3")),
    common.legend = T, legend = 'bottom'
  ),
  ggpubr::ggarrange(
    training(data_split_strat) %>% 
      group_by(Var_respuesta) %>% 
      summarise(recuento = n()) %>% 
      ungroup() %>% 
      mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
      ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(prop_var_respuesta)), position = position_stack(vjust = 0.5), size = 4)+
      scale_fill_manual( values=c("#E86EB7", "#108064"))
    ,
    testing(data_split_strat) %>% 
      group_by(Var_respuesta) %>% 
      summarise(recuento = n()) %>% 
      mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
      ungroup() %>% 
      ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(prop_var_respuesta)), position = position_stack(vjust = 0.5), size = 4)+
      scale_fill_manual( values=c("#E86EB7", "#108064")), 
    common.legend = T,
    legend = 'bottom'
  ),
  labels = c('No estratificados', 'Estratificados'), label.x = 0.1
)

# Recipes (recetas) ----

Formula_1 <- as.formula(
  paste0(
    'Var_respuesta ~ ',
    paste(datos %>% 
            select(Edad, dplyr::matches('Cog'),dplyr::matches('Esc') ) %>% 
            names(), 
          collapse = '+' )))



Receta_1 <- recipe(Formula_1,data = datos, strata = Var_respuesta )  
Receta_1  %>%  prep() %>%  bake(new_data = NULL)

Receta_1  %>%  
  step_filter_missing(all_predictors(),  threshold = 0.26) %>% 
  prep() %>%  bake(new_data = NULL)


?step_impute_linear
Receta_1  %>%  
  step_filter_missing(all_predictors(),  threshold = 0.26) %>% 
  step_impute_mean(Cognicion_03) %>%
  # step_impute_bag(all_numeric(),- all_outcomes() ) %>%
  prep() %>%  bake(new_data = NULL)

  step_filter_missing(all_predictors(),  threshold = 0.26) %>% 
  

  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_normalize(all_numeric(),-all_outcomes() ) %>% 
  # step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_adasyn(Var_respuesta, over_ratio = 1, neighbors = 10) %>% 
  prep()


