if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  # para cargar todas als librereias requeridas directamente 
  tidyverse, tidymodels, # Conjunto de librerías de progtamación de buena sintaxis y funcional
  themis, tidyposterior,baguette,corrr,readr,magrittr,forcats,skimr,
  doParallel,DALEXtra,broom,xgboost,patchwork,probably,
  readxl, # importación y exportaciónd e datos)
  kernelshap,  klaR,discrim ,rules ,brulee,sda,mda,workflowsets 
)


set.seed(2465)

PNS_split <- readRDS('C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/Split_PNS.rds' )
class_metric <- metric_set(accuracy, j_index, precision, sensitivity, specificity, roc_auc, f_meas,recall, mcc)
All_models_1_3_4 <- readRDS('C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/All_workflows.rds')
All_models_1 <- All_models_1_3_4 %>%  filter(str_detect(wflow_id, 'M1'))
All_models_3 <- All_models_1_3_4 %>%  filter(str_detect(wflow_id, 'M3'))
All_models_4 <- All_models_1_3_4 %>%  filter(str_detect(wflow_id, 'M4'))
All_models_5_6 <- readRDS('C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/All_M5_M6_workflows.rds')
All_models_5 <- All_models_5_6 %>%  filter(str_detect(wflow_id, 'M5'))
All_models_6 <- All_models_5_6 %>%  filter(str_detect(wflow_id, 'M6'))

#///////////////////////////////////////////////////////////

M1_best_result <- All_models_1 %>% 
  extract_workflow_set_result('M1_RandomForest') %>% 
  select_best(metric = 'sensitivity')

M1_fitted <- All_models_1 %>% 
  extract_workflow('M1_RandomForest') %>% 
  finalize_workflow(M1_best_result) %>%
  last_fit(PNS_split, metrics = class_metric )

M1_fitted %>%  collect_metrics()
M1_fitted %>%  collect_predictions() %>%  conf_mat(truth = PNS_DEFINITIVA, estimate= .pred_class )

M3_best_result <- All_models_3 %>% 
  extract_workflow_set_result('M3_RandomForest') %>% 
  select_best(metric = 'sensitivity')

M3_fitted <- All_models_3 %>% 
  extract_workflow('M3_RandomForest') %>% 
  finalize_workflow(M3_best_result) %>%
  last_fit(PNS_split, metrics = class_metric )

M3_fitted %>%  collect_metrics()
M3_fitted %>%  collect_predictions() %>%  conf_mat(truth = PNS_DEFINITIVA, estimate= .pred_class )

M4_best_result <- All_models_4 %>% 
  extract_workflow_set_result('M4_RandomForest') %>% 
  select_best(metric = 'sensitivity')

M4_fitted <- All_models_4 %>% 
  extract_workflow('M4_RandomForest') %>% 
  finalize_workflow(M4_best_result) %>%
  last_fit(PNS_split, metrics = class_metric )

M4_fitted %>%  collect_metrics()
M4_fitted %>%  collect_predictions() %>%  conf_mat(truth = PNS_DEFINITIVA, estimate= .pred_class )


M5_best_result <- All_models_5 %>% 
  extract_workflow_set_result('M5_RandomForest') %>% 
  select_best(metric = 'sensitivity')

M5_fitted <- All_models_5 %>% 
  extract_workflow('M5_RandomForest') %>% 
  finalize_workflow(M5_best_result) %>%
  last_fit(PNS_split, metrics = class_metric )

M5_fitted %>%  collect_metrics()
M5_fitted %>%  collect_predictions() %>%  conf_mat(truth = PNS_DEFINITIVA, estimate= .pred_class )



M6_best_result <- All_models_6 %>% 
  extract_workflow_set_result('M6_RandomForest') %>% 
  select_best(metric = 'sensitivity')

M6_fitted <- All_models_6 %>% 
  extract_workflow('M6_RandomForest') %>% 
  finalize_workflow(M6_best_result) %>%
  last_fit(PNS_split, metrics = class_metric )

M6_fitted %>%  collect_metrics()
M6_fitted %>%  collect_predictions() %>%  conf_mat(truth = PNS_DEFINITIVA, estimate= .pred_class )



# 
# saveRDS(M1_fitted,'C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/M1/M1.rds')
# saveRDS(M3_fitted,'C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/M3/M3.rds')
# saveRDS(M4_fitted,'C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/M4/M4.rds')
# saveRDS(M5_fitted,'C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/M5/M5.rds')
# saveRDS(M6_fitted,'C:/Users/Aitor/Desktop/FARMA-PEP-PRED/Scripts/PEP_modelos/Modelos_GISELA/M6/M6.rds')
# 


