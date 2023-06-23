# run balance options

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

# select reduced variables
reduced_vars <- read.csv(file.path(in_dir,  "reduced_covariate_list.csv")) %>% pull()

bgc_pts_subzone <- readRDS(file.path(fid$model_inputs0310[2], "model_input_pts.rds"))


# 2) Run the balance optimisation process  
#You need to rerun this with DS only, SM only and DS and SM together

bal_bgc <- lapply(names(bgc_pts_subzone), function(xx) {
  
  xx <- names(bgc_pts_subzone[3])
  
  alldat = bgc_pts_subzone[[xx]]
  
  outDir = file.path(fid$model_draft[2], xx)
  
  raw_dat <- read.csv(file.path(outDir, "acc_base_results.csv"))
  
  tdat <- alldat %>% mutate(slice = factor(slice))
  tdat <- tdat %>%
    dplyr::select(id,mapunit1, mapunit2, position,
                  transect_id, tid, slice, any_of(reduced_vars))
  
  tdat <- tdat[complete.cases(tdat[, 8:length(tdat)]), ]
  tdat <- droplevels(tdat)
  
  balance_optimisation_iteration(
    train_data = tdat,
    fuzz_matrix = fmat,
    ds_iterations = 0.4, # c(10,20,30,40,50,60,70,80,90),
    smote_iterations = NA,#c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.8, 0.9),
    mtry = mtry,
    min_n = min_n,
    use.neighbours = TRUE,
    out_dir = outDir, 
    detailed_output = FALSE, 
    out_dir_detailed = NA)
  
  write.csv(raw_dat,(file.path(outDir,"balance", "acc_base_results.csv")),row.names = FALSE)
  
})

# combine all balance metrics and 

acc_total <- foreach::foreach(xx = names(bgc_pts_subzone), .errorhandling = "pass",.combine = rbind) %do% {
  
  #xx = names(bgc_pts_subzone)[1]
  outDir = file.path(fid$model_draft[2], xx,"balance")
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


#in_dir <- fid$model_inputs0310[2]

write.csv(best_results, file.path(fid$model_inputs0310[2], "best_balancing.csv"), row.names = F)



# 
# # OPTIONAL : Output as a table in graphic form 
# 
# best_balance <- read.csv(file.path(fid$model_inputs0310[2], "best_balancing.csv"))
# 
# bb <- best_balance %>%
#   select(-ds, -sm, -ds_ratio, -sm_ratio, - max, - raw )%>%
#   mutate(best = paste0(balance, " + ", pcdelta, "%"))
# 
# bb <- bb %>%
#   mutate(maxmetric2 = gsub("aspat_paf_theta", 'Aspatial_', maxmetric))%>%
#   mutate(maxmetric2 = gsub("spat_paf_theta", 'Spatial_', maxmetric2))%>%
#   mutate(maxmetric2 = gsub("overall", 'best_overall', maxmetric2))%>%
#   mutate(maxmetric2 = gsub("aspatial_sum", 'Aspatial_overall', maxmetric2))%>%
#   mutate(maxmetric2 = gsub("spatial_sum", 'Spatial_overall', maxmetric2))
# 
# 
# bb1 <- pivot_wider(bb, id_cols = maxmetric2, names_from = bgc,  values_from =  best)
# 
# write.csv(bb1, file.path(fid$model_inputs0310[2], "best_balancing_format.csv"), row.names = FALSE)



