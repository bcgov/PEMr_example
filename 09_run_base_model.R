# run base model

#devtools::load_all("D:\\PEM_DATA\\PEMprepr")
#devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
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
best_tune <- read.csv(file.path(in_dir, "best_tuning.csv"))
mtry <- best_tune$mtry
min_n <- best_tune$min_n

# select reduced variables
reduced_vars <- read.csv(file.path(in_dir,  "reduced_covariate_list.csv")) %>% 
  dplyr::pull()

bgc_pts_subzone <- readRDS(file.path(fid$model_inputs0310[2], "model_input_pts.rds"))



# 1) run basic model with no balance options

model_bgc <- lapply(names(bgc_pts_subzone), function(xx){
  
  # xx <- names(bgc_pts_subzone[1])
  
  alldat = bgc_pts_subzone[[xx]]
  
  # for Date Creek 
  # utid = unique(alldat$tid)
  # 
  # if("ichmc2_1.1_1" %in% utid){
  #   alldat =  alldat %>% 
  #     filter(!tid == "ichmc2_1.1_1")
  #   print("filtering ichmc2 tids")
  # }
  # if("ichmc1_2.3_8" %in% utid){
  #   alldat =  alldat %>% 
  #     filter(!tid == "ichmc1_2.3_8")
  #   
  #   print("filtering ichmc1 tids")
  # }
  #
  
  
  outDir = file.path(fid$model_draft[2], xx)
  
  if(!dir.exists(file.path(outDir, "raw_outputs"))){
    dir.create(file.path(outDir, "raw_outputs"))
  }
  
  detailed_outdir <- file.path(outDir, "raw_outputs")
  
  tdat <- alldat %>% mutate(slice = factor(slice))
  
  tdat <- tdat %>%
    dplyr::select(id, mapunit1, mapunit2, position,
                  transect_id, tid, slice, any_of(reduced_vars))
  
  tdat <- tdat[complete.cases(tdat[, 8:length(tdat)]), ]
  
  # run basic model (no smote and no downsample)
  train_data <- tdat
  
  train_data <- droplevels(train_data)
  
  baseout <- run_base_model(
    train_data,
    fuzz_matrix = fmat,
    mtry = mtry,
    min_n = min_n,
    use.neighbours = TRUE, 
    detailed_output = TRUE, 
    out_dir = detailed_outdir)
  
  write.csv(baseout, file.path(outDir, "acc_base_results.csv"), row.names = FALSE)
  
  # generate model accuracy report
  model_report(train_data, baseout, outDir)
  
})

