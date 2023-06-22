## run Vehicle routing problem 
# this is still in development 

# set up start location
vec_dir = fid$shape_dir_1010[2]
cities <- st_read(file.path(vec_dir, "major_towns_bc.gpkg"))
nearest_town = "datecreek_test"
start <- cities[cities$NAME == nearest_town,"NAME"]
start <- start[1,]

#cost <- terra::rast(file.path(fid$sampling_input_landscape[2], "cost.tif"))
vrp_outpath <- fid$samplingplan_vrp[2]

# example with one clhs
boi <- "ICHmc1"
clhs_set <- "ICHmc1_clhs_sample_2.gpkg"

# define selected clhs option
sample_points <- sf::st_read(list.files(fid$samplingplan_clhs[2], pattern = clhs_set, full.names = TRUE))
pnts <- sample_points[1:5,]
# read in transition layer 

tr1 <- terra::rast(file.path(fid$sampling_input_landscape[2], "cost_road.tif"))
#terra::plot(sample_cost_masked)

library(sf)
library(gdistance)
library(sp)
library(foreach)

reticulate::source_python('D:/PEM_DATA/DateCreek_AOI/mTSP.py')

#tr1 <- readRDS("inputs/transition_layer.rds")
#pnts <- st_read("inputs/ICHmc1_clhs_sample_1.gpkg")

setup_python <- function(){
  reticulate::conda_create("pemr")
  enviros <- reticulate::conda_list()
  reticulate::use_python(enviros[enviros$name == "pemr","python"])
  reticulate::conda_install("ortools",envname = "pemr", pip = T)
  reticulate::py_module_available("ortools")
}

setup_python()



plot(pnts)
startLoc <- pnts[5,]
pnts_in <- pnts[-5,]
test <- create_vrp(tr1, pnts_in, startLoc, 3)


## THis needs work to complete vrp 






