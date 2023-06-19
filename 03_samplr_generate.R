
library(PEMr)
run_pemr()

#remotes::install_github("bcgov/PEMprepr", build_vignettes = TRUE)
devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
#devtools::install_github("kdaust/clhs") 

library(PEMprepr)
library(PEMsamplr)
library(ggplot2)
library(sf)
library(terra)
library(clhs)
library(foreach)

fid <- setup_folders("DateCreek_AOI")

###############################################################
# 1) generate clhs

clhs_outpath <- fid$samplingplan_clhs[2]

landscape_dir <- fid$sampling_input_landscape[2]

fileoi <- c("dah_LS.tif", "mrvbf_LS.tif", "landform_LS.tif", "cost.tif")

filesoi <- list.files(landscape_dir, full.names = TRUE)[list.files(landscape_dir) %in% fileoi]

all_cov <- terra::rast(filesoi)

# read in bec data
bec_dir = fid$sampling_input_exclusion[2]

boi <- list.files(bec_dir, pattern = ".tif")


for(b in boi) {
  b <- boi[7]
  boi_mask <- terra::rast(file.path(bec_dir, b)) 
  names(boi_mask) = "mask"
  bname <- gsub("_exclude_mask.tif", "", b)
  
  sample_layers_masked <- c(all_cov, boi_mask) %>%  
    terra::mask(boi_mask) 
  sample_layers_masked <- sample_layers_masked[[1:4]]
  
  terra::writeRaster( sample_layers_masked, file.path(clhs_outpath, paste0(bname,"_clhs_sample_mask.tif")), overwrite = TRUE)
  
  # create 10 different sample plans
  for(rot in 1:5){ 
    #rot = 1
    sample_points <- create_clhs(all_cov = sample_layers_masked, 
                                 num_slices = 5, 
                                 to_include = NULL, 
                                 n_points = 5 , 
                                 min_dist = 1000,
                                 num_sample = 5000000)
    sample_points <- sample_points %>% 
      dplyr::mutate(bgc = bname)
    
    sf::st_write(sample_points, file.path(clhs_outpath, paste0(bname,"_clhs_sample_",rot,".gpkg")),append=FALSE )
    
  }
  
} 

# Review the sample plans and select the method with simple cost metrics #or

ftemp <- list.files(clhs_outpath, pattern = ".gpkg$", full.names = TRUE)

all_samples <- foreach(fs = 1:length(ftemp), .combine = "rbind" ) %do% {
  
  ff <- ftemp[fs]
  layers <- st_layers(ff)$name
  
  sample_points_all <- foreach(l = 1:length(layers), .combine = rbind) %do% {
    ll = layers[l]
    sptemp <- st_read(ff, layer = ll)
    sptemp <- sptemp %>%
      dplyr::mutate(clhs_repeat = basename(ff)) %>% 
      dplyr::mutate(bgc = stringr::str_extract(layers, "[^_]+"))
    
    sptemp
  }
  
}

# summarise the costs per sample plan
repsum <- all_samples %>%
  sf::st_drop_geometry() %>%
  dplyr::select(clhs_repeat, bgc, cost)

repsum <- repsum %>%
  dplyr::group_by(clhs_repeat, bgc) %>%
  dplyr::mutate(tcost = sum(cost)) %>%
  dplyr::select(-cost) %>%
  dplyr::distinct()

# plot the total costs by subzone
p1 <- ggplot(repsum, aes(y = tcost, x =  clhs_repeat )) +
  geom_point()+
  facet_wrap(~bgc, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1






#3) Generate Location Options for Paired Samples

#For sampling efficiency and field safety paired transects at a cLHS points is desireable. The cLHS point is the centre point of the first transect. We generate additional points at 400m from the cLHS points in the eight cardinal and ordinal directions . This distance provides a minimum 100m spacing from the cLHS transect to any second transect. Cardinal points that fall outside the buffered cost surface are removed as candidates.
#Selection of the second (or third) transect from the available set could be made in several ways:randomly, choice of least cost, transect with maximum variable difference from cLHS point.

#Currently the second location is based on the least cost.

# select the bec variant of interest and the clhs chosen
# note this needs to be repeated for each bcg unit 


#boi <- "ICHmc1"
#clhs_set <- "ICHmc1_clhs_sample_2.gpkg"

boi <- "ICHmc2"
clhs_set <- "ICHmc2_clhs_sample_3.gpkg"

# define location of output 
out_path <- fid$sampleplan_final_transect[2]

# define selected clhs option
sample_points <- sf::st_read(list.files(fid$samplingplan_clhs[2], pattern = clhs_set, full.names = TRUE))

# read in cost layer 
cost <- terra::rast(file.path(fid$sampling_input_landscape[2], "cost.tif"))

# read in mask_poly 
mask_poly <- sf::st_read(list.files(fid$sampling_input_exclusion[2], 
                                    pattern = paste0(boi,"_exclude_poly.gpkg"), full.names = TRUE))
st_crs(mask_poly) <- 3005

# Generate the transects and poinst and output as geopacakage
build_site_transects(sample_points, cost, mask_poly, centroid_distance = 400, out_path) 

# export a tracking sheet for the samples 
allpoints <- grep("points_all", sf::st_layers(file.path(out_path, "s1_sampling.gpkg"))$name, value = T)
boi <- stringr::str_extract(allpoints, "[^_]+")

for (ii in 1:length(boi)) {
  #ii = 1
  b <- boi[ii]
  points <- st_read(file.path(out_path, outname), layer = paste0(b,"_points_all")) 
  pointsout <- points %>%
    cbind(st_coordinates(points)) %>% 
    dplyr::select(bgc,id, rotation, X,Y) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(Surveyor = "", Date_Completed = "", Transect_comment = "") 
  
  write.csv(pointsout, file.path(out_path, paste0(b, "_tracking_sheet.csv")))
  
}

