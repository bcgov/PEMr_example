
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

fid <- setup_folders("DateCreek_AOI")

# Recursive Feature selection / Correlated variable reduction

res_size = "5m"

mcols <- readRDS(file.path(fid$model_inputs0310[2], "full_covariate_list.rds"))

rast_list <- list.files(file.path(fid$cov_dir_1020[2],res_size), pattern = ".sdat$|.tif$", full.names = T, recursive = T)

rast_list <- rast_list[tolower(gsub(".sdat$|.tif$", "", basename(rast_list))) %in% tolower(mcols)]

# read in the raster template used for modelling (i.e 5m resolution)
rstack <- terra::rast(rast_list)

subsmpl <- terra::spatSample(rstack, size = 100000, method = "regular", xy = FALSE, na.rm = TRUE) # sample raster

reduced_vars <- reduce_features(subsmpl, cutoff = 0.70, corr_plot = FALSE)

write.csv(reduced_vars, file.path(fid$model_inputs0310[2], "reduced_covariate_list.csv"),row.names = FALSE)




### Optional:  tuning optimization step (takes a long time to run)

# import training points
cleandat <- read.csv(file.path(fid$model_inputs0310[2], "training_pts.csv")) 


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

# set up the training points by BGCs and model folders

cleandat <- read.csv(file.path(fid$model_inputs0310[2], "training_pts.csv")) 

# set up the training points by BGC
zones <- c(as.character(unique(cleandat$bgc_cat)))

bgc_pts_subzone <- lapply(zones, function (i){ 
  #i =  zones[2]  
  pts_subzone <- cleandat %>%
    dplyr::filter(stringr::str_detect(tid, as.character(paste0(tolower(i), "_")))) %>% 
    droplevels()
  
  # remove any bec sites series that are not in the bec_zone catergory 
  
  munits <- grep(unique(pts_subzone$mapunit1), pattern = "_\\d",value = TRUE, invert = FALSE)
  
  diff_bec_mapunits <- grep(munits,pattern = paste0("^",i,"_"), value = TRUE , invert =TRUE) 
  
  pts_subzone <- pts_subzone %>%
    dplyr::filter(!mapunit1 %in% diff_bec_mapunits)
  
  if(nrow(pts_subzone) == 0){ pts_subzone = NULL} else {ppts_subzone = pts_subzone }
  pts_subzone
})

# format names
names(bgc_pts_subzone) <- zones

saveRDS(bgc_pts_subzone, file.path(fid$model_inputs0310[2], "model_input_pts.rds"))



# Create a folder per BGC and run a check on the training points 

model_bgc <- lapply(names(bgc_pts_subzone), function(xx){
  
  #xx <- names(bgc_pts_subzone[1])
  
  alldat = bgc_pts_subzone[[xx]]
  #out_name = names(bgc_pts_subzone[xx])
  
  # set up output folders
  outDir = file.path(fid$model_draft[2], xx)
  if(!dir.exists(file.path(outDir))){dir.create(file.path(outDir))} 
  
  # set up final model folder
  finalDir = file.path(fid$model_final[2], xx)
  if(!dir.exists(file.path(finalDir))){dir.create(file.path(finalDir))} 
  
  tdat <- alldat %>% mutate(slice = factor(slice))
  tdat <- tdat %>% 
    dplyr::select(id, fnf, x, y, bgc_cat, mapunit1, mapunit2, position, transect_id, tid, slice, everything())
  
  tdat_all <- tdat[complete.cases(tdat[ ,12:length(tdat)]),]
  #tdat_centre <- tdat_all %>% dplyr::filter(position %in% "Orig")
  
  #PEMsamplr::trainingpt_report(tpts = tdat_all, trans = trans, out_dir = outDir)
  
})

