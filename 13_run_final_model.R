
devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
devtools::load_all("D:\\PEM_DATA\\PEMmodelr")

library(PEMr)
library(dplyr)

# load all pem packages
run_pemr()

aoi <- "Deception"

fid <- setup_folders(paste0(aoi,"_AOI"))

fmat <- read.csv(file.path(fid$AOI_dir[2], "fuzzy_matrix.csv" ))%>%
  dplyr::select(target, Pred, fVal)

# set up tuning (applies to all models)
best_tune <- fread(file.path (in_dir, "best_tuning.csv"))
mtry <- best_tune$mtry
min_n <- best_tune$min_n

# select reduced variables
best_balance <- read.csv(file.path(fid$model_inputs0310[2], "best_balancing.csv"))
reduced_vars <- read.csv(file.path(fid$model_inputs0310[2],  "reduced_covariate_list.csv")) %>% pull()
bgc_pts_subzone <- readRDS(file.path(fid$model_inputs0310[2], "model_input_pts.rds"))


run_all_final_models(bgc_pts_subzone, best_balance, reduced_vars, mbal = "overall",out_dir = fid$model_final[2])
