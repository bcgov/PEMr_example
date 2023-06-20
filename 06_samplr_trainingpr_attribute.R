
# clean and attribute training points
# output is csv table 


devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")

library(PEMr)

# load all pem packages
run_pemr()

fid <- setup_folders("DateCreek_AOI")

mapkey <- read.csv(file.path(fid$AOI_dir[2], "DateCreek_MapUnitLegend.csv"))


# attribute and add neighbours to the training data

## clean points 
cleandat <- fid$trainpts_maps[2]

res_folder = "5m"

#location of raster base on which modelling will be applied
covdir <- fid$cov_dir_1020[2]

# location of attributed and output data 
#outdat <- fid$training_data_1030[2]

# read in the raster template used for modelling (i.e 5m resolution)
trast <- terra::rast(file.path(covdir, res_folder, "template.tif"))

processed_transects <- st_read(file.path(cleandat, "proc_s1_transects.gpkg"))

# convert lines to points
allpts <- convert_lines_pts(processed_transects, trast)

# Add the extra points that are not lines? 


## Add neighbours 
tpoints_ne <- add_neighbours(allpts,trast)

# attribute points 
allrasts <- file.path(covdir, res_folder)
allpts <- attribute_points(tpoints_ne, allrasts) 

st_write(allpts, dsn = file.path(fid$trainpts_att[2], "allpts.gpkg"), delete_layer = TRUE)


# read in training points
allpts <- st_read(file.path(fid$trainpts_attrib[2], "allpts.gpkg"))

# read in bec data 
bec <- sf::st_read(file.path(fid$shape_dir_1010[2], "bec.gpkg")) %>% sf::st_cast(., "MULTIPOLYGON") 

# define units to map
mpts <- define_mapunits(allpts, mapkey, "Full")

# get list of unique values
#maplist <- list_mapunits(mpts)

# match to the key and filter for forest and non_forest points
subzones <- unique(bec$MAP_LABEL)
subzones <- tolower(gsub("\\s+","",subzones))

out <- mpts %>%
  cbind(st_coordinates(.)) %>%
  mutate(fnf = ifelse(grepl(paste0(subzones, collapse = "|"), tolower(mapunit1)), "forest", "non_forest"))

if(!"MAP_LABEL" %in% names(out)){
  out <- out %>%
    st_join(st_transform(bec[, "MAP_LABEL"], st_crs(.)), join = st_nearest_feature) 
}

out <- out %>%
  st_drop_geometry() %>% 
  dplyr::select(fnf, everything()) %>%
  dplyr::rename(bgc_cat = MAP_LABEL) %>% 
  dplyr::rename_all(.funs = tolower)


if("stuc_stage" %in% names(out)){
  out <- out %>%
    dplyr::rename("struc_stage" = stuc_stage,
                 "struc_mod" = stuc_mod)
    
}  

write.csv(out, file.path(fid$training_data_1030[2], "allpts.csv"),row.names = FALSE)
