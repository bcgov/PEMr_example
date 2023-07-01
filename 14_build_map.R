
devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
devtools::load_all("D:\\PEM_DATA\\PEMmodelr")

library(PEMprepr)
library(PEMmodelr)
library(terra)
library(tidyterra)

fid <- setup_folders("Deception_AOI")  

model_dir <- file.path(fid$model_final[2])
bec_zones <- as.list(list.files(model_dir))

covdir <- fid$cov_dir_1020[2]
res_folder = "5m"

#template <- terra::rast(file.path(covdir, res_folder, "template.tif"))

# select reduced variables
reduced_vars <- read.csv(file.path(fid$model_inputs0310[2],  "reduced_covariate_list.csv")) %>% pull()

# get full raster list 
rast_list <- list.files(file.path(covdir, res_folder), pattern = ".tif$", full.names = TRUE)
rast_list <- rast_list[tolower(gsub(".tif", "", basename(rast_list))) %in% (reduced_vars)]
rstack <- terra::rast(rast_list)

# generate tiles for mapping 

tile_dir <- file.path(model_dir, "tiles")

if(!dir.exists(file.path(tile_dir))){
  dir.create(file.path(tile_dir)) 
  # make tiles (might want to make NA tile )
  ntiles <- makeTiles(template, 500, filename= file.path(tile_dir, "tile_.tif"),  na.rm=FALSE, overwrite = TRUE)
}else if(dir.exists(file.path(tile_dir))){
  ntiles <- list.files(tile_dir, full.names = T)
}



# for each bec zone
bgcs <- list.dirs(model_dir, recursive = F,full.names = FALSE)
bgcs <- gsub("tiles", '', bgcs)
bgcs <- bgcs[-4]

# loop through each of the 

map_bgc <- for(b in bgcs){
  
  b = bgcs[3]
  #b = bgcs
  
  mfit = list.files(file.path(model_dir, b), recursive = TRUE, pattern = "final_modelraw.rds$", full.names = T)
  
  mmodel <- readRDS(file.path(mfit))
  
  out_dir <- file.path(model_dir, b, "map")
  # check if out_dir exists
  if(!dir.exists(file.path(out_dir))){ dir.create(file.path(out_dir))}
  
  maps <- predict_map(mmodel, out_dir, tile_size = 500, tile_dir, rstack, probability = FALSE)
  
} 


####################################################
# Compile all the maps together 

bec_shp <- st_read(file.path(fid$shape_dir_1010[2], "bec.gpkg"), quiet = TRUE) %>%
  select(MAP_LABEL)

model_dir

folders <- as.list(c("ESSFmc", "ESSFmcw", "SBSmc2")) 

# step 1:  set up a key for the combined map (includes all the units)
rkey <- foreach::foreach(f = folders,.combine = rbind) %do%{
  
  keys <- read.csv(file.path(model_dir, f, "map", "response_names.csv")) %>%
    mutate(model  = f)
}

rkey <- rkey %>% dplyr::mutate(map.response = seq(1:length(.pred_class)))


# Step 2: For each bgc, filter and mask the raster map and update key if needed

combine_map <- lapply(folders, function(f){
  
  #f <- folders[[1]]
  
  rtemp <- terra::rast(file.path(model_dir, f, "map", "mosaic.tif"))
  
  rtemp[is.na(rtemp[])] <- 0 
  
  # filter to only predict over bgc
  bec_filter <- bec_shp %>%
    filter(MAP_LABEL == f) %>%
    dplyr::select(MAP_LABEL) 
  
  rtemp <- terra::mask(rtemp, bec_filter)
  
  subkey <- rkey %>% dplyr::filter(model == f) %>%
    mutate(mosaic = as.numeric(rownames(.)))
  
  # check if the key matches or needs reclassification 
  if (isTRUE(unique(subkey$mosaic == subkey$map.response))) {
    
    print("matching key")
    
  } else {
    
    print("updating key")
    
    m <- subkey %>%
      mutate(to = as.numeric(X), 
             from = as.numeric(X)+1) %>%
      dplyr::select(to, from, map.response) 
    
    reclm <-  as.matrix(m, ncol=3, byrow= TRUE)
    rtemp <-  classify(rtemp, reclm, right = FALSE)#, include.lowest=TRUE)
    
  }
  
  rtemp <- terra::classify(rtemp, cbind(-Inf, 0, NA), include.lowest=TRUE)
  rtemp
  
})

# join all the maps together

if(length(folders) == 3) {
  
  all_key <- terra::merge(combine_map[[1]], combine_map[[2]])
  all_key <- terra::merge(all_key, combine_map[[3]])
  
} 
# 
# all_key <- merge(combine_map[[1]], combine_map[[2]], overlap=TRUE)
# all_key <- merge(all_key, combine_map[[3]], overlap = TRUE)
# all_key <- merge(all_key, combo_map[[4]], overlap = TRUE)
# all_key <- merge(all_key, combo_map[[5]], overlap = TRUE)
# all_key <- merge(all_key, combo_map[[6]], overlap = TRUE)
# all_key <- merge(all_key, combo_map[[7]], overlap = TRUE)

# tidy key and output maps
rkey <- rkey %>% dplyr::select(.pred_class, model, map.response)

writeRaster(all_key, file.path(model_dir, "forest_combo_bgcs.tif"), overwrite = TRUE)

write.csv(rkey, file.path(model_dir, "response_combo_bcgs_key.csv"))





