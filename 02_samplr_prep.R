
library(PEMr)
run_pemr()

#devtools::install_github("josephlewis/leastcostpath")
#devtools::install_github("kdaust/clhs") 

#library(PEMprepr)
#library(PEMsamplr)
library(ggplot2)
library(sf)
library(terra)
library(bcmaps)
#library(leastcostpath)

fid <- setup_folders("Buck_AOI")

###############################################

# Generate landscape covariates

dem <- terra::rast(file.path( fid$cov_dir_1020[2],"25m", "dem.tif"))

outpath <- fid$sampling_input_landscape[2]

SAGApath_dir ="C:/SAGA/saga-7.7.0_x64/"

template = terra::rast(file.path( fid$cov_dir_1020[2],"25m", "template.tif"))

# generate landscape covariates

saga_param <- list(T_SLOPE = 64, TPCTL_V = 6, T_PCTL_R = 2,
                   P_SLOPE = 4.0, P_PCTL = 3.0, UPDATE = 1,
                   CLASSIFY = 1, MAX_RES = 100)

create_samplr_covariates(dem, SAGApath =  SAGApath_dir,
                         output =  outpath,
                         sieve_size = 10,
                         rtemplate = template,
                         dah_threshold = 0.2, 
                         saga_param = saga_param,
                         covariates = NULL)

# generate landscape validation 

fileoi <- c("dah_LS.tif", "mrvbf_LS.tif", "landform_LS.tif")

filesoi <- list.files(outpath, full.names = TRUE)[list.files(outpath) %in% fileoi]

landscapes <- create_binned_landscape(outpath)

terra::plot(landscapes)

terra::writeRaster(landscapes, file.path(outpath, "landscape_variable_validation.tif"), overwrite = TRUE)


# Check the landscape distribution by bgc

becpath <-fid$shape_dir_1010[1]

bec <- sf::st_read(file.path(becpath, "bec.gpkg")) %>%
  sf::st_cast(., "MULTIPOLYGON") 

routdf <- check_bgc_landscapes(bec,landscapes)

ggplot2::ggplot(routdf, aes(landscape)) +
  ggplot2::geom_histogram() +
  ggplot2::facet_wrap(~MAP_LABEL)

#ggplot2::ggplot(routdf, aes(landscape, fill = MAP_LABEL)) +
#  ggplot2::geom_histogram() 




#############################################
# Generate Cost layer 

dem <- terra::rast(file.path( fid$cov_dir_1020[2],"25m", "dem.tif"))
vec_dir <- fid$shape_dir_1010[2]

cities <- st_read(file.path(vec_dir, "major_towns.gpkg"))

nearest_town = "Houston"
start <- cities[cities$NAME == nearest_town,"NAME"]

roads_raw <- sf::st_read(file.path(vec_dir, "road_major.gpkg"), quiet = TRUE) %>%
  sf::st_zm()

# check the roads layer
check_road_layer(roads_raw)

## generate transition layer 
costprep <- prep_cost_layers_lcp(x = dem,
                                 cost_function = "tobler offpath", 
                                 neighbours = 8, 
                                 roads = roads_raw, 
                                 crit_slope = 12, 
                                 max_slope = NULL, 
                                 percentile = 0.5, 
                                 exaggeration = FALSE) 
#build cost layer 
acost <- create_accum_cost(x = costprep, origin = start, FUN = min)
names(acost) = "cost"

terra::writeRaster(acost, file.path(fid$sampling_input_landscape[2], "cost_raw.tif"), overwrite = TRUE)


# Check costs by bgc vs binned landscape
landscape <- terra::rast(file.path(outpath, "landscape_variable_validation.tif"))
bgccost <- check_bgc_cost(bgc = bec, 
                          binned_landscape = landscape, 
                          cost = acost)

ggplot2::ggplot(bgccost, ggplot2::aes(landscape, fill = cost_code)) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::facet_wrap(~MAP_LABEL)




# generate cost penalty 

final_cost <- create_cost_penalty(vec_dir = vec_dir, 
                                  cost = acost, 
                                  dem = dem, 
                                  costval = 3000,
                                  vri_cost = 2500,
                                  calc_by_qq = TRUE)



terra::plot(final_cost)

# create no sample areas 
cost_masked <- create_cost_nosample(vec_dir = vec_dir, 
                                    cost = final_cost)

terra::writeRaster(cost_masked, 
                   file.path(fid$sampling_input_landscape[2],  
                             "cost.tif"), overwrite = TRUE)


# generate a BGC cost mask per BGC in map area

exclusion_path <- fid$sampling_input_exclusion[2]

create_bgc_mask(vec_dir, cost_masked, exclusion_path)



