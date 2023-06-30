# generate final accuracy statistics

devtools::load_all("D:\\PEM_DATA\\PEMr_packages\\PEMprepr")
devtools::load_all("D:\\PEM_DATA\\PEMr_packages\\PEMsamplr")
devtools::load_all("D:\\PEM_DATA\\PEMr_packages\\PEMmodelr")

library(PEMprepr)
library(PEMmodelr)
library(dplyr)
library(stringr)

aoi <- "Deception"

fid <- setup_folders(paste0(aoi,"_AOI"))

in_dir <- fid$model_inputs0310[2]

fmat <- read.csv(file.path(fid$AOI_dir[2], "fuzzy_matrix.csv" ))%>%
  dplyr::select(target, Pred, fVal)

# set up tuning (applies to all models)
best_tune <- fread(file.path (in_dir, "best_tuning.csv"))
mtry <- best_tune$mtry
min_n <- best_tune$min_n

# select reduced variables
reduced_vars <- read.csv(file.path(in_dir,  "reduced_covariate_list.csv")) %>% pull()

best_balance <- read.csv(file.path(in_dir, "best_balancing.csv"))

bgc_pts_subzone <- readRDS(file.path(fid$model_inputs0310[2], "model_input_pts.rds"))


# Outputs required 

# 1: raw model accuracy metrics (with neighbours)
# 2: raw model accuracy metrics (without neighbours)
# 4: extract best balance option (with neighbours)
# 5: extract best balance option (without neighbours)

prep_final_acc_metric(bgc_pts_subzone, fid, fmat, mtry, min_n, best_balance, final_model_metric = "overall")

# generate basic model statistic 

# for each of the bgcs (add loop here)


output_bgc <- lapply(names(bgc_pts_subzone), function(xx){
  
  #xx <- names(bgc_pts_subzone[1])
  
  outDir_raw = file.path(fid$model_final[2], xx, "model_outputs")
  
  raw_nn <- read.csv(file.path(outDir_raw, "acc_base_results_no_neighbours.csv")) %>%
    mutate(acc_type = "raw")
  
  #acc_plot_mu(raw_nn, type = "spat")
  #acc_plot_mu(raw_nn, type = "aspat")
  
  #acc_plot_metrics_site(raw_nn)
  #acc_plot_metrics(raw_nn, slice = FALSE)
  #acc_plot_metrics(raw_nn, slice = TRUE)
  
  raw_wn <- read.csv(file.path(outDir_raw, "acc_base_results_neighbours.csv")) %>%
    mutate(acc_type = "raw_neighbours")
  
  bal_wn <- read.csv(file.path(outDir_raw, "best_balance_acc_neighbours.csv")) %>%
    mutate(acc_type = "rebalanced_neighbours") 
  
  bal_nn <- read.csv(file.path(outDir_raw, "best_balance_acc_no_neighbours.csv")) %>%
    mutate(acc_type = "rebalanced") 
  
  
  acc_all <- rbind(raw_nn, raw_wn, bal_wn, bal_nn )
  
  # generate a full table with shows all combinations 
  
  noi <-  names(acc_all)[str_detect(names(acc_all),"_theta")]
  
  acc_all <- acc_all %>%
    mutate(slice = factor(slice), acc_type = factor(acc_type)) %>% 
    dplyr::select(acc_type, slice, acc, kap, oob, all_of(noi))%>%
    distinct()
  
  acc_all <-  acc_all  %>% 
    rowwise()%>% 
    mutate(inv_OOB = 1-oob)%>%
    ungroup()%>%
    mutate(across(where(is.numeric), ~ . *100)) %>% distinct() %>% 
    mutate(MLaccuracy = acc,
           inv_OOB = inv_OOB,
           kappa = kap) %>% 
    dplyr::select(-acc, -kap, -oob) 
  
  library(stringr)
  
  acc_all2 <- acc_all %>% 
    pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
    distinct() %>% 
    mutate(type_sa = case_when(
      str_detect(accuracy_type, "aspat_") ~ "proportional",
      str_detect(accuracy_type, "spat_") ~ "spatial")) %>%
    mutate(type = case_when(
      accuracy_type %in% c("MLaccuracy","kappa", "inv_OOB") ~ "0_machine",
      str_detect(accuracy_type, "^aspat_paf_") ~ "3_fuzzy_p+a",
      str_detect(accuracy_type, "^aspat_pa_") ~ "2_p+alternate",
      str_detect(accuracy_type, "^aspat_p_") ~ "1_primary",
      str_detect(accuracy_type, "^spat_p_") ~ "1_primary",
      str_detect(accuracy_type, "^spat_pa_") ~ "2_p+alternate",
      str_detect(accuracy_type, "^spat_paf_") ~ "3_fuzzy_p+a")) %>% 
    filter(!is.na(type)) %>% filter(!accuracy_type == "slice") %>% 
    mutate(accuracy_type = str_replace(accuracy_type, "theta1", "theta 1"),
           accuracy_type = str_replace(accuracy_type, "aspat_p_theta", "\u0398 "),
           accuracy_type = str_replace(accuracy_type, "aspat_pa_theta", "\u0398 "),
           accuracy_type = str_replace(accuracy_type, "aspat_paf_theta", "\u0398 "),
           accuracy_type = str_replace(accuracy_type, "spat_paf_theta", "\u0398 "),
           accuracy_type = str_replace(accuracy_type, "spat_pa_theta", "\u0398 "),
           accuracy_type = str_replace(accuracy_type, "spat_p_theta", "\u0398 "))%>%
    mutate(bgc = xx)
  
  
  write.csv(acc_all2, file.path(fid$model_final[2], xx, "all_results_summary.csv"), row.names = F)
  
}) 



# Second figure: recreate this for the paper
xx <- names(bgc_pts_subzone[1])
acc_all1 <- read.csv(file.path(fid$model_final[2], xx, "all_results_summary.csv"))

xx <- names(bgc_pts_subzone[2])
acc_all2 <- read.csv(file.path(fid$model_final[2], xx, "all_results_summary.csv"))

xx <- names(bgc_pts_subzone[3])
acc_all3 <- read.csv(file.path(fid$model_final[2], xx, "all_results_summary.csv"))

acc_all = rbind(acc_all1, acc_all2, acc_all3)



# plot  raw vs raw_neighbours


acc_type_to_include = c("raw","raw_neighbours", "rebalanced_neighbours")

# filter the type of model outputs wanted: 

acc <- acc_all %>% 
  filter(acc_type %in% acc_type_to_include)

# select spatial (P, alt, fuzzy), machine , aspatial (p+a)

acc <- acc %>% 
  mutate(keep = case_when(
    type_sa == "spatial" & type %in% c('1_primary', '2_p+alternate', '3_fuzzy_p+a') ~ 1,
    type == "0_machine" ~ 1,
    type_sa == "proportional" & type == '2_p+alternate' ~ 1))

acc1 <- acc %>% 
  filter(keep == 1)%>% 
  select(-keep, -slice)


temp <- acc1 %>% 
  mutate(type2 = ifelse(type_sa == "proportional", "proportional(p+a)",type))%>%
  mutate(type2 = ifelse(type == "0_machine", "0_machine", type2))%>%
 # filter(acc_type != "raw_neighbours")%>%
  select(-type, -type_sa)

acc = temp



###Create ggplot graphic
#plot_raw_bal <- ggplot(aes(y = value, x = accuracy_type, fill = acc_type), data = acc ) + # fill = accuracy_type2
plot_raw_bal <- ggplot(aes(y = value, x=factor(accuracy_type, level=c('kappa', "MLaccuracy" ,  "inv_OOB", "Θ  1","Θ .5", "Θ 0")), fill = acc_type), data = acc ) + # fill = accuracy_type2
  
    geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.8, width = 0.5, 
               position = position_dodge(0.6)) +
  scale_fill_brewer(type = "qual") +
  facet_grid(bgc~type2 , labeller = labeller(type2 = 
                                               c("0_machine" = "machine",
                                                 "1_primary" = "primary",
                                                 "2_p+alternate" = "alternate",
                                                 "proportional(p+a)" = "proportional(p+a)",
                                                 "3_fuzzy_p+a" = "fuzzy")), scales = "free_x") +
  #ggtitle(main = xx )+
  geom_hline(yintercept = 65,linetype ="dashed", color = "black") + 
  theme_pem_facet() +
  scale_fill_manual(values=c("grey90", "grey70", "grey50"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Metric", y = "Accuracy") #+
#ggtitle(xx)#+
#ylim(25, 100)

plot_raw_bal

finalise_facet_plot(plot_raw_bal, file.path("Figures_outputs", "accuracy_raw_vs_balance_with_neighours.png"), width_pixels=520,
                    height_pixels=480)


# 
# # plot  raw vs raw_neighbours
# 
# acc_type_to_include = c("raw", "raw_neighbours")
# 
# # filter the type of model outputs wanted: 
# 
# acc <- acc_all %>% 
#   filter(acc_type %in% acc_type_to_include)
# 
# # select spatial (P, alt, fuzzy), machine , aspatial (p+a)
# 
# acc <- acc %>% 
#   mutate(keep = case_when(
#     type_sa == "spatial" & type %in% c('1_primary', '2_p+alternate', '3_fuzzy_p+a') ~ 1,
#     type == "0_machine" ~ 1,
#     type_sa == "proportional" & type == '2_p+alternate' ~ 1))
# 
# acc1 <- acc %>% 
#   filter(keep == 1)%>% 
#   select(-keep, -slice)
# 
# 
# temp <- acc1 %>% 
#   mutate(type2 = ifelse(type_sa == "proportional", "proportional(p+a)",type))%>%
#   mutate(type2 = ifelse(type == "0_machine", "0_machine", type2))%>%
#   # filter(acc_type != "raw_neighbours")%>%
#   select(-type, -type_sa)
# 
# acc = temp
# 
# 
# 
# ###Create ggplot graphic
# plot_raw_neighbours <- ggplot(aes(y = value, x = accuracy_type, fill = acc_type), data = acc ) + # fill = accuracy_type2
#   geom_boxplot() +
#   #scale_fill_brewer(type = "qual") +
#   facet_grid(bgc~type2 , labeller = labeller(type2 = 
#                                                c("0_machine" = "machine",
#                                                  "1_primary" = "primary",
#                                                  "2_p+alternate" = "alternate",
#                                                  "proportional(p+a)" = "proportional(p+a)",
#                                                  "3_fuzzy_p+a" = "fuzzy")), scales = "free_x") +
#   #ggtitle(main = xx )+
#   geom_hline(yintercept = 65,linetype ="dashed", color = "black") + 
#   theme_pem_facet() +
#   scale_fill_manual(values=c("grey90", "grey70", "grey50"))+
#   #theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(x = "Metric", y = "Accuracy") #+
# #ggtitle(xx)#+
# #ylim(25, 100)
# 
# plot_raw_neighbours
# 
# 
# 
























# 
# 
# # acc_sum <- acc %>%
# #   dplyr::group_by(acc_type, type, accuracy_type,type_sa) %>%
# #   dplyr::summarise(mean = mean(value),
# #                    q25 = quantile(value, prob = 0.25),
# #                    q75 = quantile(value, prob = 0.75))
# 
# 
# 
# 
# 
# 
# outDir_raw = file.path(fid$model_final[2], xx, "raw")
# 
# raw_wn <- read.csv(file.path(outDir_raw, "acc_base_results_neighbours.csv")) %>%
#   mutate(acc_type = "raw_neighbours")
# 
# bal_wn <- read.csv(file.path(outDir_raw, "best_balance_acc.csv")) %>%
#   mutate(acc_type = "rebalanced") %>%
#   mutate(oob = 0)
# 
# acc_all <- rbind( raw_wn, bal_wn)
# 
# thetathresh <- read.csv(file.path(outDir_raw, "theta_threshold.csv")) %>% 
#   select(-X)%>%
#   mutate(acc_type = "raw_neighbours")
# 
# mint = thetathresh %>% 
#   group_by(type, accuracy_type) %>% 
#   filter(lag(above_thresh == FALSE))%>%
#   slice_min(theta_final) %>% 
#   rowwise %>%
#   mutate(theta_thresh = theta_final -0.05) %>%
#   mutate(accuracy_type2 = paste0(substr(type, 1, 5),"_",accuracy_type, "_")) %>%
#   mutate(type = case_when(
#     str_detect(accuracy_type2, "aspat_pa_") ~ "4_aspatial",
#     str_detect(accuracy_type2, "^spati_p_") ~ "1_primary",
#     str_detect(accuracy_type2, "^spati_pa_") ~ "2_p+alternate",
#     str_detect(accuracy_type2, "^spati_paf_") ~ "3_fuzzy_p+a")) %>%
#   dplyr::select(type,theta_thresh) %>%
#   filter(!is.na(type))
# 
# #mint
# # generate a full table with shows all combinations 
# 
# noi <-  names(acc_all)[str_detect(names(acc_all),"_theta")]
# 
# acc_all <- acc_all %>%
#   mutate(slice = factor(slice), acc_type = factor(acc_type)) %>% 
#   dplyr::select(acc_type, slice, acc, kap, oob, all_of(noi))%>%
#   distinct()
# 
# acc_all <-  acc_all  %>% 
#   mutate(across(where(is.numeric), ~ . *100)) %>% distinct() %>% 
#   mutate(MLaccuracy = acc,
#          oob = (1-oob)*100,
#          kappa = kap) %>% 
#   dplyr::select(-acc, -kap) 
# 
# 
# acc_theta <- acc_all %>% 
#   pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
#   distinct() %>% 
#   mutate(type = case_when(
#     accuracy_type %in% c("MLaccuracy", "oob",  "kappa") ~ "0_machine",
#     str_detect(accuracy_type, "aspat_pa_") ~ "4_aspatial",
#     str_detect(accuracy_type, "^spat_p_") ~ "1_primary",
#     str_detect(accuracy_type, "^spat_pa_") ~ "2_p+alternate",
#     str_detect(accuracy_type, "^spat_paf_") ~ "3_fuzzy_p+a")) %>% 
#   filter(!is.na(type)) %>% filter(!accuracy_type == "slice") 
# 
# acc_theta <- acc_theta  %>%
#   mutate(accuracy_type_no = as.numeric(str_replace(accuracy_type, ".*_theta", "")))%>%
#   mutate(accuracy_type = str_replace(accuracy_type, "theta1", "theta 1"),
#          accuracy_type = str_replace(accuracy_type, "aspat_pa_theta", "\u0398 "),
#          accuracy_type = str_replace(accuracy_type, "spat_paf_theta", "\u0398 "),
#          accuracy_type = str_replace(accuracy_type, "spat_pa_theta", "\u0398 "),
#          accuracy_type = str_replace(accuracy_type, "spat_p_theta", "\u0398 "))
# #acc_all2 <- acc_all2 %>% dplyr::filter(bgc == "SBSmc2", balance == "rebalanced") %>% mutate(accuracy_type2 = as.factor(accuracy_type)) %>% droplevels()
# 
# acc_theta <- acc_theta %>% filter(type != "0_machine")
# 
# 
# ###Create ggplot graphic
# overall_acc <- ggplot(aes(y = value, x = accuracy_type_no, fill = acc_type), data = acc_theta) + # fill = accuracy_type2
#   geom_boxplot() +
#   scale_fill_brewer(type = "qual") +
#   facet_grid( ~ type, labeller = labeller(type = 
#                                             c("0_machine" = "machine",
#                                               "1_primary" = "primary",
#                                               "2_p+alternate" = "alternate",
#                                               "3_fuzzy_p+a" = "fuzzy",
#                                               "4_aspatial" = "proportional (p+a)")), scales = "free_x")+
#   geom_vline(data = mint, mapping = aes(xintercept = theta_thresh),linetype ="dashed", color = "black") +
#   #facet_wrap(~acc_type)+
#   #geom_jitter(position=position_jitter(width=.1), colour = "grey", alpha = 0.8) + 
#   geom_hline(yintercept = 65,linetype ="dashed", color = "black") + 
#   theme_pem_facet() +
#   ggtitle(xx)+
#   #scale_fill_manual(values=c("grey90", "grey70", "grey50"))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
#   xlab("Metric") + ylab("Accuracy") + 
#   ylim(25, 100) + 
#   xlim(0,1)
# 
# 
# overall_acc


# 
# acc_all2 <- acc_all %>% 
#   pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
#   distinct() %>% 
#   mutate(type = case_when(
#     accuracy_type %in% c("MLaccuracy", "oob",  "kappa") ~ "0_machine",
#     str_detect(accuracy_type, "aspat_pa_") ~ "4_aspatial",
#     str_detect(accuracy_type, "^spat_p_") ~ "1_primary",
#     str_detect(accuracy_type, "^spat_pa_") ~ "2_p+alternate",
#     str_detect(accuracy_type, "^spat_paf_") ~ "3_fuzzy_p+a")) %>% 
#   filter(!is.na(type)) %>% filter(!accuracy_type == "slice") %>% 
#   mutate(accuracy_type = str_replace(accuracy_type, "theta1", "theta 1"),
#          accuracy_type = str_replace(accuracy_type, "aspat_pa_theta", "\u0398 "),
#          accuracy_type = str_replace(accuracy_type, "spat_paf_theta", "\u0398 "),
#          accuracy_type = str_replace(accuracy_type, "spat_pa_theta", "\u0398 "),
#          accuracy_type = str_replace(accuracy_type, "spat_p_theta", "\u0398 "))
# #acc_all2 <- acc_all2 %>% dplyr::filter(bgc == "SBSmc2", balance == "rebalanced") %>% mutate(accuracy_type2 = as.factor(accuracy_type)) %>% droplevels()
# 
# ###Create ggplot graphic
# overall_acc <- ggplot(aes(y = value, x = accuracy_type, fill = acc_type), data = acc_all2 ) + # fill = accuracy_type2
#   geom_boxplot() +
#   scale_fill_brewer(type = "qual") +
#   facet_grid( ~ type, labeller = labeller(type = 
#                                             c("0_machine" = "machine",
#                                               "1_primary" = "primary",
#                                               "2_p+alternate" = "alternate",
#                                               "3_fuzzy_p+a" = "fuzzy",
#                                               "4_aspatial" = "proportional (p+a)")), scales = "free_x")+
#   #facet_wrap(~acc_type)+
#   #geom_jitter(position=position_jitter(width=.1), colour = "grey", alpha = 0.8) + 
#   geom_hline(yintercept = 65,linetype ="dashed", color = "black") + 
#   theme_pem_facet() +
#   ggtitle(xx)+
#   #scale_fill_manual(values=c("grey90", "grey70", "grey50"))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") +
#   xlab("Metric") + ylab("Accuracy") + 
#   ylim(25, 100)
# 
# 
# overall_acc
# 
# finalise_facet_plot(overall_acc, "./PEM_standards_manuscripts/outputs/accuracy_raw_vs_balance_no_extras_25limit.png", width_pixels=480,
#                     height_pixels=480)
# })
