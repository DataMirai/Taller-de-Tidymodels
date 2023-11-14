if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  themis, tidyposterior,baguette,corrr,readr,magrittr,forcats,skimr,
  doParallel,DALEXtra,broom,xgboost,patchwork,probably,
  readxl, # importación y exportaciónd e datos)
  kernelshap,  klaR,discrim ,rules ,brulee,sda,mda,workflowsets 
)


PNS_data <- read_xlsx('/PEP_Aitor.xlsx') %>% 
  # filtros requeridos para tener la base de datos correcta
  filter(
    !str_detect(.$Ident_caso, '[:alpha:]') &
      !is.na(PNS_DEFINITIVA) &
      Edad_estudio >= 16 &
      FES_SSD_NoAffective_NoToxicPsych ==1) %>% 
  mutate_if(is.character, as_factor ) %>% 
  mutate(PNS_DEFINITIVA= as_factor(PNS_DEFINITIVA))


Split_PNS <- readRDS('C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/Split_PNS.rds')

PNS_train <- training(Split_PNS)
PNS_test  <- testing(Split_PNS)




Formula_Multi <- PNS_data %>% 
  dplyr::select(
    # 27 parametros de 150 datos, unos 200 con el resampling, justillo pero estamos entre 5.5-7.5 OPP
    Edad_primer_episodio,
    Estimation_ci,Atention,Working_memory,Verbal_memor,Executive_function,Cognitive_reserve , 
    Prolactina_VB,Neutrofilos_VB,Hemoglobina_VB,Sodio_VB,perimetro_VB,Fósforo_VB,Glucosa_VB,T4_libre_VB,HCM_VB,CHCM_VB,
    DUP, PANSS_positivos_VB, PANSS_negativos_VB,
    PRS_MDD, PRS_AN,PRS_EA ,PRS_SA,PRS_TRS,PRS_CUD, PRS_SZ) %>% 
  names() %>% 
  paste(., collapse = ' + ') %>% 
  paste('PNS_DEFINITIVA ~' ,.) %>% 
  as.formula() 

Receta_Multi  <- recipe(
  # esta receta tiene 14 parametros para estimar, de posible 153 observaciones, bien
  Formula_Multi,
  data = PNS_train, strata = PNS_DEFINITIVA )  %>% 
  step_filter_missing(all_predictors(),  threshold = 0.26) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_normalize(all_numeric(),-all_outcomes() ) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_adasyn(PNS_DEFINITIVA, over_ratio = 1, neighbors = 10) %>% 
  step_other(threshold = 0.6 ) %>% 
  prep()

Formula_Multi_2 <- PNS_data %>% 
  dplyr::select(
    # 27 parametros de 150 datos, unos 200 con el resampling, justillo pero estamos entre 5.5-7.5 OPP
    Edad_primer_episodio,
    Estimation_ci,Atention,Working_memory,Verbal_memor,Executive_function,Cognitive_reserve , 
    peso_VB:Testosterona_VB,
    DUP, PANSS_positivos_VB, PANSS_negativos_VB,
    contains('PRS')) %>% 
  names() %>% length()
  paste(., collapse = ' + ') %>% 
  paste('PNS_DEFINITIVA ~' ,.) %>% 
  as.formula() 

Receta_Multi_2  <- recipe(
  # esta receta tiene 14 parametros para estimar, de posible 153 observaciones, bien
  Formula_Multi_2,
  data = PNS_train, strata = PNS_DEFINITIVA )  %>% 
  step_filter_missing(all_predictors(),  threshold = 0.26) %>% 
  step_impute_knn(all_nominal(),-all_outcomes()) %>%
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  step_normalize(all_numeric(),-all_outcomes() ) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  step_adasyn(PNS_DEFINITIVA, over_ratio = 1, neighbors = 10) %>% 
  step_other(threshold = 0.6 ) %>% 
  prep()




# //////////////////////////////////////////////////////////////////////
# Models definition ---- 
# //////////////////////////////////////////////////////////////////////

Model_RandomForest <- 
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

# Model_XGBoost <- 
#   boost_tree(
#     mtry = tune(), trees = tune(), min_n = tune(), 
#     tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
#     sample_size = tune())  %>% 
#   set_engine("xgboost") %>% 
#   set_mode("classification")

Model_Bagged_Tree <- 
  bag_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# Model_Naive_Bayes <- 
#   naive_Bayes(smoothness = tune(),Laplace = tune() ) %>% 
#   set_engine("klaR") %>% 
#   set_mode("classification")

Model_C5 <- 
  C5_rules(trees = tune(), min_n = tune() ) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

# Model_Discrim_Flexible <- 
# discrim_flexible(num_terms = tune(), prod_degree = tune() ) %>% 
#   set_engine("earth") %>% 
#   set_mode("classification")


Receta_list <- list(
  M5 = Receta_Multi,
  M6 = Receta_Multi_2)

Model_list <- list(
  RandomForest     = Model_RandomForest,
  # XGBoost          = Model_XGBoost,
  Bagged_Trees     = Model_Bagged_Tree,
  # Naive_Bayes      = Model_Naive_Bayes,
  # Discrim_flexible = Model_Discrim_Flexible ,
  C5               = Model_C5
)

wf_set <- workflow_set(
  preproc = Receta_list, 
  models = Model_list, 
  cross = T)

train_resamples <-  vfold_cv(PNS_train, v = 5, strata = PNS_DEFINITIVA)

class_metric <- metric_set(accuracy, j_index, precision, sensitivity, specificity, roc_auc, f_meas,recall, mcc)


wf_sample_exp <- 
  wf_set %>% 
  workflow_map(
    resamples = train_resamples, 
    fn = "tune_grid",
    grid = 20,
    verbose = TRUE, 
    metrics = class_metric, 
    seed = 2465)


# saveRDS(wf_sample_exp,'C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/M5_M6_workflows.rds')












