
#remotes::install_github("bcgov/PEMprepr", build_vignettes = TRUE)
devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
devtools::load_all("D:\\PEM_DATA\\PEMmodelr")


library(PEMprepr)
library(PEMsamplr)
library(PEMmodelr)
library(sf)
library(terra)


fid <- setup_folders("DateCreek_AOI")

tpts <- read.csv(file.path(fid$training_data_1030[2], "allpts.csv"))

tpts <- tpts %>% 
  mutate(mapunit1 = as.factor(mapunit1),
         mapunit2 = as.factor(mapunit2)) 

# check data type
tpts <- tpts %>% 
  filter(data_type != "repeat") %>%
  filter(data_type != "incidental")


# check minimum number of mapunits and filter out 
tpts <- filter_min_mapunits(tpts, min_no = 10)


# select the covariates for the model 

# mcols <- c("dem_convergence", "dem_convexity", #"dem_cnetwork" ,
#            "dem_dah" ,"dem_dem_preproc",  #"dem_flow_accum_ft" , 
#            #"dem_flow_accum_p" ,"dem_flow_accum_td", 
#            "dem_flowpathlentd", #"dem_max_fp_l" , 
#            "dem_max_fp_l2" ,"dem_ls_factor" ,"dem_mbi",               
#            #"dem_mrn" , "dem_mrn_area", "dem_mrn_mheight",
#            "dem_mscale_tpi", "dem_mrrtf" ,"dem_mrvbf" , 
#            "dem_mrrtf2" ,"dem_mrvbf2" , "dem_open_neg" ,
#            "dem_open_pos" , #"dem_hdist"  ,"dem_vdist" ,              
#            #"dem_hdistnob" ,"dem_vdistnob", 
#            "dem_protection" , "dem_slope_height",
#            "dem_ms_position" , #"dem_norm_height" , 
#            "dem_stand_height" ,#"dem_valleydepth" , 
#            "dem_rid_level",   "dem_tcatchment" 
# )

# deception 


source("_covar_list_fn.r")

mcols = covar_list("deception")

tpts <- tpts %>%
  dplyr::select(-c(dem.1,template.1))

mpts <- tpts %>%
  dplyr::select(-c(order, point_type, observer, transition, data_type, stuc_stage, stuc_mod, date_ymd, time_hms, edatope, comments, photos)) %>%
  dplyr::select(id, fnf, x, y, bgc_cat, mapunit1, mapunit2, position, transect_id, tid, slice, any_of(mcols))


mcols <- names(mpts)[12:length(names(mpts))]
saveRDS(mcols, file.path(fid$model_inputs0310[2], "full_covariate_list.rds"))

write.csv(mpts, file.path(fid$model_inputs0310[2], "training_pts.csv"),row.names = FALSE)
