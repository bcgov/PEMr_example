# format the raw data and compile into a single training point data set

#devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")

library(PEMr)

# load all pem packages
run_pemr()

fid <- setup_folders("DateCreek_AOI")

# location of transects 
trans_output <- fid$trainpts_201040[2]

# location of the raw field data
rawdat <- fid$trainpts_transect[2]

# location of cleaned ouputs
cleandat <- fid$trainpts_maps[2]

# Create a consolidated transect layout 
transect_layout <- generate_transectlayout(fid$sampleplan_final_transect[2])
transect_layout_buf <- sf::st_buffer(transect_layout, 10)
st_write(transect_layout, file.path(trans_output, "transect_layout_s1_TEST.gpkg"), delete_layer = TRUE)


# import and clean field data
points <- format_fielddata(rawdat, transect_layout_buf)
sf::st_write(points, file.path(cleandat, "s1_points_TEST.gpkg"), delete_layer = TRUE)

# format tracklog
tracks <- format_tracklog(rawdat, transect_layout_buf)
sf::st_write(tracks, file.path(cleandat, "s1_tracklog.gpkg"), delete_layer = TRUE)

# process the points into line and review
transect_layout <- st_read(file.path(trans_output, "transect_layout_s1.gpkg"))
transect_layout_buf <- sf::st_buffer(transect_layout, 10)
points <- st_read( file.path(cleandat, "s1_points.gpkg"))

# covert to lines 
processed_transects <- make_lines(GPSPoints = points, 
                                  Transects = transect_layout_buf, 
                                  method = "pts2lines",  
                                  tBuffer = 20, PROJ = 3005) 

#st_write(processed_transects,  file.path(cleandat, "proc_s1_transects.gpkg"), 
         #delete_layer = TRUE)

# run the training pt report
tpts<- st_read(file.path(cleandat, "s1_points.gpkg"))
trans <- st_read(file.path(cleandat, "proc_s1_transects.gpkg"))
#out_dir <- cleandat

PEMsamplr::trainingpt_report(tpts = points, trans = trans, out_dir = cleandat)

