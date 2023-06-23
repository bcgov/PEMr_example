# clean and attribute training points
# output is csv table 

devtools::load_all("D:\\PEM_DATA\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMsamplr")
devtools::load_all("D:\\PEM_DATA\\PEMmodelr")
#remotes::install_github("bcgov/PEMr", build_vignettes = FALSE)

library(PEMr)

# load all pem packages
run_pemr()

#aoi <- "DateCreek"
aoi <- "Deception"

fid <- setup_folders(paste0(aoi,"_AOI"))
mapkey <- read.csv(file.path(fid$AOI_dir[2], paste0(aoi,"_MapUnitLegend.csv")))

# attribute points - # warning this can take a long time with lots of data 
allpts <- prep_tps_attribute(res = 5)
st_write(allpts, dsn = file.path(fid$trainpts_att[2], "allpts.gpkg"), delete_layer = TRUE)


# determine Bec level
allpts <- st_read(file.path(fid$trainpts_attrib[2], "allpts.gpkg"))

tpts <- prep_tps_bgc(allpts, mapkey, min_no = 10)

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
# this is a short cut to use the old dataset covariates (ie. tiffs and not run through the entire new proces)
source("_covar_list_fn.r")

mcols = covar_list("deception")

final_tp <- prep_tps_covs(tpts, mcols, outdir = fid$model_inputs0310[2])
