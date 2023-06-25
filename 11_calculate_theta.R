
# Optional script

devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
devtools::load_all("D:\\PEM_DATA\\PEMmodelr")

library(PEMr)
library(dplyr)

# load all pem packages
run_pemr()

aoi <- "Deception"

fid <- setup_folders(paste0(aoi,"_AOI"))

in_dir <- fid$model_inputs0310[2]

fmat <- read.csv(file.path(fid$AOI_dir[2], "fuzzy_matrix.csv" ))%>%
  dplyr::select(target, Pred, fVal)

# set up tuning (applies to all models)
best_tune <- read.csv(file.path (in_dir, "best_tuning.csv"))
mtry <- best_tune$mtry
min_n <- best_tune$min_n

reduced_vars <- read.csv(file.path(in_dir,  "reduced_covariate_list.csv")) %>% pull()
bgc_pts_subzone <- readRDS(file.path(in_dir, "model_input_pts.rds"))
best_balance <- read.csv(file.path(in_dir, "best_balancing.csv"))


# Determine optimum Theta value for the metric of choice

# Step 1: calculate theta for the Base model 

bgcs <- names(bgc_pts_subzone)

for(i in bgcs){
  
  #i = bgcs[1]
  
  bgc_raw_thetas <- file.path(fid$model_draft[2], i, "raw_outputs")
  
  theta_folder = file.path(fid$model_final[2], i, "theta_results")
  if(!dir.exists(theta_folder)) {dir.create(theta_folder)}
  
  acc_out <- generate_theta_metrics(bgc_raw_thetas)
  write.csv(acc_out, file.path(theta_folder, "compiled_theta_results_raw.csv"), row.names = FALSE)
  
  theta_thresh <- generate_theta_threshold(acc_out)
  write.csv(theta_thresh, file.path(theta_folder, "theta_threshold_raw.csv"),row.names = FALSE)
  
}


# Step 1: calculate theta for best balanced model 

# select the balance metrics to estimate theta
mbal <-"overall" 

mbaldf <- best_balance %>% dplyr::filter(maxmetric == mbal) %>%
  select(bgc, balance, ds_ratio, sm_ratio) 

model_bgc <- lapply(names(bgc_pts_subzone), function(xx) {
  
  #xx <- names(bgc_pts_subzone[1])
  
  alldat = bgc_pts_subzone[[xx]]
  
  outDir = file.path(fid$model_final[2], xx, "theta_results")
  detailed_outdir = file.path(fid$model_final[2], xx, "theta_results")
  
  tdat <- alldat %>% mutate(slice = factor(slice))
  tdat <- tdat %>%
    dplyr::select(id, mapunit1, mapunit2, position,
                  transect_id, tid, slice, any_of(reduced_vars))
  
  tdat <- tdat[complete.cases(tdat[, 8:length(tdat)]), ]
  
  # testing 
  #final_data = as.data.table(final_data)
  bgc_bal = mbaldf %>% filter(bgc == xx)
  ds_ratio = bgc_bal %>% pull(ds_ratio)
  sm_ratio = bgc_bal %>% pull(sm_ratio)
  
  bal_model <- balance_optimisation_iteration(
    train_data =  tdat ,
    ds_iterations = ds_ratio,
    smote_iterations =  sm_ratio,
    mtry = mtry,
    min_n = min_n,
    fuzz_matrix = fmat,
    out_dir = fid$model_inputs0310[2],
    use.neighbours = TRUE,
    detailed_output = TRUE, 
    out_dir_detailed = detailed_outdir)
  
}) 


# # Determine optimum Theta value for best overall 

bgcs <- names(bgc_pts_subzone)

for(i in bgcs){
  
  #i = bgcs[1]
  theta_folder = file.path(fid$model_final[2], i, "theta_results")
  
  acc_out <- generate_theta_metrics(theta_folder)
  write.csv(acc_out, file.path(theta_folder, "compiled_theta_results_balance.csv"), row.names = FALSE)
  
  theta_thresh <- generate_theta_threshold(acc_out)
  write.csv(theta_thresh, file.path(theta_folder, "theta_threshold_balance.csv"),row.names = FALSE)
  
}
