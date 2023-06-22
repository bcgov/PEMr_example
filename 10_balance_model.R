# run balance options

#devtools::load_all("D:\\PEM_DATA\\PEMprepr")
#devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
devtools::load_all("D:\\PEM_DATA\\PEMmodelr")

library(PEMr)
library(dplyr)

# load all pem packages
run_pemr()


fid <- setup_folders("DateCreek_AOI")

in_dir <- fid$model_inputs0310[2]

fmat <- read.csv(file.path(fid$AOI_dir[2], "fuzzy_matrix.csv" ))%>%
  dplyr::select(target, Pred, fVal)

# set up tuning (applies to all models)
best_tune <- read.csv(file.path (in_dir, "best_tuning.csv"))
mtry <- best_tune$mtry
min_n <- best_tune$min_n

# select reduced variables
reduced_vars <- read.csv(file.path(in_dir,  "reduced_covariate_list.csv")) %>% pull()

bgc_pts_subzone <- readRDS(file.path(fid$model_inputs0310[2], "model_input_pts.rds"))



# 2) Run the balance optimisation process  

# need to run the balance options 

# Alternatively Run the balance optimised version - old version iterate through all combinations 

bal_bgc <- lapply(names(bgc_pts_subzone), function(xx) {
  
  #xx <- names(bgc_pts_subzone[3])
  
  alldat = bgc_pts_subzone[[xx]]
  
  outDir = file.path(fid$model_draft[2], xx)
  
  tdat <- alldat %>% mutate(slice = factor(slice))
  tdat <- tdat %>%
    dplyr::select(id,mapunit1, mapunit2, position,
                  transect_id, tid, slice, any_of(reduced_vars))
  
  tdat <- tdat[complete.cases(tdat[, 8:length(tdat)]), ]
  tdat <- droplevels(tdat)
  
  balance_optimisation_iteration(
    train_data = tdat,
    fuzz_matrix = fmat,
    ds_iterations = 30, #c(10,20,30,40,50,60,70,80,90),
    smote_iterations = NA,#c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.8, 0.9),
    mtry = mtry,
    min_n = min_n,
    use.neighbours = TRUE,
    out_dir = outDir
  )
  
})



# 3 Combine all the outputs and find out which is the best balance option

# combine all balance options into a single data table
acc_total <- foreach::foreach(xx = names(bgc_pts_subzone), .errorhandling = "pass",.combine = rbind) %do% {
  #xx = names(bgc_pts_subzone)[1]
  outDir = file.path(fid$model_draft[2], xx)
  allacc <- combine_balance_ouputs(outDir)
  
  #outDir = file.path(fid$model_draft[2], xx)
  #  end testing
  bal_dir = outDir
  alldata_list <- list.files(file.path(bal_dir), full.names = TRUE, pattern = "acc_", recursive = TRUE)
  
  # remove files with no information
  data_list<- alldata_list[file.info(alldata_list)$size>10]
  
  aresults <- foreach::foreach(k = data_list[1:2], .errorhandling = "pass",.combine = rbind) %do% {
    #k = data_list[2]
    # print(k)
    temp = read.csv(k)
    
    temp <- temp %>% dplyr::mutate(filename = paste(basename(k)))
    temp
  }
  
  allacc <- allacc %>% dplyr::mutate(bgc = xx)
  
}


# select the best metrics for each Bec variant 

bgcs <- unique(as.factor(acc_total$bgc)) %>% droplevels()

best_results <- foreach(b = levels(bgcs), .combine=rbind) %do% {
  
  #b <- bgcs[1]
  
  acc_bgc <- acc_total %>% dplyr::filter(bgc %in% b)
  best_metrics <- select_best_acc(acc_bgc) %>% 
    mutate(bgc = b)
  
  best_metrics
  
}


in_dir <- fid$model_inputs0310[2]

write.csv(best_results, file.path(in_dir, "best_balancing.csv"), row.names = F)
