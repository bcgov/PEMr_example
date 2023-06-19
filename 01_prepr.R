# step 1: 

# load in the data 

remotes::install_github("bcgov/PEMr", build_vignettes = FALSE)

library(PEMr)

run_pemr()

library(PEMprepr)
library(ggplot2)
library(bcmaps)
#library(sf)
#library(terra)

fid <- setup_folders("Buck_AOI")

# save aoi.gpkg in for folder location 
# AOI / 00_raw_inputs/10_vector

# prepare vector data: 

aoi_raw <- st_read(file.path(fid$shape_dir_0010[2], "aoi.gpkg"))
aoi <- aoi_snap(aoi_raw, "expand")

mapview::mapview(aoi)

# write out (this will be added to function later on)
sf::st_write(aoi, file.path(fid$shape_dir_1010[1], "aoi_snapped.gpkg"))


# will to test at home
# create vector layers

create_base_vectors(in_aoi = aoi,
                    out_path = fid$shape_dir_0010[1])

v <- list.files(path = fid$shape_dir_0010[1], pattern = ".gpkg",
                recursive = TRUE)

v

# make any edits here then keep in raw folder. (see manual)

# Optional : Review and shift to the other file location 

origindir <- fid$shape_dir_0010[1]
filestocopy <- list.files(path = fid$shape_dir_0010[1], pattern = ".gpkg",recursive = TRUE)
targetdir <- fid$shape_dir_1010[1]
lapply(filestocopy, function(x) file.copy(paste (origindir, x , sep = "/"),  
                                          paste (targetdir,x, sep = "/"), recursive = FALSE,  copy.mode = TRUE))
#file.remove(filestocopy)





# Prepare Raster Data (for landscape)
res = 25
res_name = paste0(res, 'm')

r25 <- create_template(aoi,res)
terra::writeRaster(r25, file.path(fid$cov_dir_1020[2],res_name, "template.tif"), overwrite = TRUE)


# Prepare Raster Data(for 5m lidar template)
res = 5
res_name = paste0(res, 'm')
r5 <- create_template(aoi,res)
terra::writeRaster(r5, file.path(fid$cov_dir_1020[2],res_name, "template.tif"), overwrite = TRUE)



# read in base DEM and align the dEM

#demfile <- file.path( fid$cov_dir_1020[2],res_scale, "dem.tif")
#demfile25 <- file.path( fid$cov_dir_1020[2],"25m", "dem.tif")

#dem <- terra::rast(file.path( fid$cov_dir_1020[2],res_scale, "dem.tif"))


# read in base raster
trim_raw <- cded_raster(aoi)

# convert the trim to matching raster 
trim <- terra::rast(trim_raw)
trim <- terra::project(trim, r25)

layer_options <- c("sinksfilled", "sinkroute", "dem_preproc", "slope_aspect_curve",
                   "tcatchment", "tca", "scatchment", "twi", "channelsnetwork",
                   "overlandflow", "overlandflow2", "multiresflatness", "multiresflatness2",
                   "multiresflatness3", "tri", "convergence", "openness",
                   "dah", "tpi", "ridgevalley", "mrn", "flowaccumulation",
                   "slopelength", "flowaccumulation2", "flowaccumulation3",
                   "flowpathlength", "flowpathlength2", "flowpathlength3", "lsfactor",
                   "solarrad", "convexity", "vertdistance", "tci_low",
                   "swi", "windexp", "texture", "protection", "vrm",
                   "mbi", "mscale_tpi", "relposition", "slopecurvatures",
                   "steepestslope", "upslopearea")

# USe 5m scale as built for modelling 

# run a single output 
create_covariates(dtm = trim,           ## raster created above
                  SAGApath = "C:/SAGA/saga-7.7.0_x64/", ## Where SAGA GIS is installed
                  output = file.path(fid$cov_dir_1020[2], "25m"), ## from the setup_folders above
                  layers = "sinkroute")        ## test one is working 

# run all  covariate
create_covariates(dtm = trim,           ## raster created above
                  SAGApath = "C:/SAGA/saga-7.7.0_x64/", ## Where SAGA GIS is installed
                  output = file.path(fid$cov_dir_1020[2], "25m"), ## from the setup_folders above
                  layers = "all")        ## test one is working 




l <- list.files(path = fid$cov_dir_1020[2], pattern = ".sdat$",
                recursive = TRUE)



# add component to rename the outputs and combine into single folder 


# generate BEC

bec_sf <- sf::st_read(file.path(fid$shape_dir_1010[1], "bec.gpkg")) %>%
  sf::st_cast(., "MULTIPOLYGON") 

bec_code <- bec_sf %>% st_drop_geometry()  %>% dplyr::select(MAP_LABEL) %>%
  unique() 

bec_code <- bec_code %>% 
  mutate(bgc_unique_code = seq(1, length(bec_code$MAP_LABEL),1))

bec_sf <- dplyr::left_join(bec_sf, bec_code)

bec_vec <- terra::vect(bec_sf)

# generate a 25m raster

bec_ras25 <- terra::rasterize(bec_vec, r25, field = "MAP_LABEL")

terra::writeRaster(bec_ras25, file.path(fid$cov_dir_1020[2], "25m", "bec.tif"), overwrite = TRUE)


# generate a 5m raster 

bec_ras5 <- terra::rasterize(bec_vec, r5, field = "MAP_LABEL")

terra::writeRaster(bec_ras5, file.path(fid$cov_dir_1020[2], "5m", "bec.tif"), overwrite = TRUE)


