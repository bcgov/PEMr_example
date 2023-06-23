
# clean and attribute training points
# output is csv table 

devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
devtools::load_all("D:\\PEM_DATA\\PEMmodelr")
library(PEMr)
library(tidyr)
library(ggplot2)

# load all pem packages
run_pemr()

#aoi <- "DateCreek"
aoi <- "Deception"

fid <- setup_folders(paste0(aoi,"_AOI"))

# import training points
cleandat <- read.csv(file.path(fid$model_inputs0310[2], "training_pts.csv")) 

reduced_vars <- read.csv(file.path(fid$model_inputs0310[2],  "reduced_covariate_list.csv")) %>% 
  dplyr::pull()

#### 1) Find best tuning 

# filter the original points (not neighbours) and remove correlated covars
trDat <- cleandat %>%
  dplyr::filter(position %in% "Orig") %>%
  select_pure_training() %>%
  dplyr::select(mapunit1, slice, any_of(reduced_vars)) %>%
  dplyr::mutate(slice = as.factor(slice)) %>%  
  tidyr::drop_na() %>% 
  dplyr::mutate(mapunit1 = factor(mapunit1)) %>%  
  droplevels()

# tune the parameters
tune_res <- tune_rf(trDat, output = 'full')

tune_res$splits

# show best 
best_tune_roc_auc <- tune::show_best(tune_res, metric = "roc_auc")
best_tune_accuracy <- tune::show_best(tune_res, metric = "accuracy")

# best_tune <- select_best(tune_res, metric = "accuracy")
tune_res %>%
  tune::collect_metrics() %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(mean, min_n, mtry) %>%
  tidyr::pivot_longer(min_n:mtry,
                      values_to = "value",
                      names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

best_tune <- tune::select_best(tune_res, metric = "accuracy")

write.csv(best_tune, file.path(fid$model_inputs0310[2], "best_tuning.csv"), row.names = FALSE)


###########

# set up the training points by BGCs into a list and add model folders

out_tps <- prep_tps_model(cleandat, out_dir = fid$model_inputs0310[2])



