################################################################################
##
## Script to compute null models for FD - for MAMMALS
##
## Camille Magneville
##
## 23/04/2024 - 10/2024
##
## 5_a_FD_Null_models_MAMMALS.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`
`%dopar%` <- foreach::`%dopar%`


# 1 - Load data ========================================================

# Species-traits data:
sp_tr_MAMMALS <- readRDS(here::here("transformed_data",
                                  "final_traits_restricted_MAMMALS.rds"))

# Occurrence data:
sp_occ_MAMMALS <- readRDS(here::here("transformed_data",
                                   "sp_asb_50km_restricted_MAMMALS.rds"))

# Check that traits have the right format:
str(sp_tr_MAMMALS)

# Create traits category data frame:
traits_nm <- c("ActivityTime",
               "BodyMass",
               "ForagingStratum",
               "GenerationLength",
               "Hibernation",
               "TrophicLevel",
               "OffspringPerYear")
traits_cat <- c("O", "Q", "N", "Q", "N", "O", "Q")
trait_cat_df <- data.frame(traits_nm, traits_cat)
colnames(trait_cat_df) <- c("trait_name", "trait_type")

# Rename species in sp*tr df to remove "_":
rownames(sp_tr_MAMMALS) <- stringr::str_replace(rownames(sp_tr_MAMMALS), '_', ' ')

# One species is present in the occ db (91 cells) but no traits:
setdiff(colnames(sp_occ_MAMMALS), rownames(sp_tr_MAMMALS))
sp_occ_MAMMALS <- sp_occ_MAMMALS[, which(colnames(sp_occ_MAMMALS) != "Arvicola italicus")]

# 2 - Summarise traits and assemblages =========================================


# Species traits summary:
traits_summ_MAMMALS <- mFD::sp.tr.summary(
  tr_cat     = trait_cat_df,
  sp_tr      = sp_tr_MAMMALS,
  stop_if_NA = TRUE)
traits_summ_MAMMALS$tr_summary_list

# Summary of the assemblages * species dataframe:
asb_sp_summ_MAMMALS <- mFD::asb.sp.summary(asb_sp_w = sp_occ_MAMMALS)
sort(asb_sp_summ_MAMMALS$sp_tot_w, decreasing = TRUE) # number of occurrences of each species
sort(asb_sp_summ_MAMMALS$asb_sp_richn, decreasing = TRUE) # number of species per asb


# 3 - Compute functional distances based on traits =============================


sp_dist_MAMMALS <- mFD::funct.dist(
  sp_tr         = sp_tr_MAMMALS,
  tr_cat        = trait_cat_df,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

# Traits-based distance:
dist_MAMMALS <- mFD::dist.to.df(list("dist" = sp_dist_MAMMALS))
dist_MAMMALS


# 4 - Compute functional spaces and assess their quality =======================


fspaces_quality_MAMMALS <- mFD::quality.fspaces(
  sp_dist             = sp_dist_MAMMALS,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")
fspaces_quality_MAMMALS$quality_fspaces

# Plot it:
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_MAMMALS,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d",
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")

# Note: the 4D is ok (trade off number of species per asb and quality)


# 5 - Test correlation between functional axes and traits =====================


# Retrieve MAMMALS coordinates in the functional space:
sp_faxes_coord_MAMMALS <- fspaces_quality_MAMMALS$"details_fspaces"$"sp_pc_coord"

# Test correlation:
tr_faxes_MAMMALS <- mFD::traits.faxes.cor(
  sp_tr          = sp_tr_MAMMALS,
  sp_faxes_coord = sp_faxes_coord_MAMMALS[ , c("PC1", "PC2", "PC3", "PC4")],
  plot           = TRUE)
tr_faxes_MAMMALS


# 6 - Plot functional spaces ===================================================


fct_space_MAMMALS <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_MAMMALS[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgoldenrod3",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "cyan4",
  fill_vert       = "cyan4",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 1,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)
fct_space_MAMMALS


# 7 - Compute functional diversity indices =====================================


# Compute FOri and FMPD:
fmpd_fori_indices_MAMMALS <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_MAMMALS[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = sp_occ_MAMMALS,
  ind_vect         = c("fmpd", "fori"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)
fmpd_fori_MAMMALS <- fmpd_fori_indices_MAMMALS$functional_diversity_indices


# Remove assemblages that have less that 4 species for FRic (4 included):
low_nb_sp_asb <- rownames(fmpd_fori_MAMMALS[which(fmpd_fori_MAMMALS$sp_richn < 5), ])
sp_occ_subset_MAMMALS <- as.data.frame(sp_occ_MAMMALS)
sp_occ_subset_MAMMALS <- sp_occ_subset_MAMMALS %>%
  tibble::rownames_to_column(var = "Asb") %>%
  dplyr::filter(! Asb %in% low_nb_sp_asb) %>%
  tibble::column_to_rownames("Asb") %>%
  as.matrix()


# Compute FRic:
fric_indices_MAMMALS <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_MAMMALS[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = sp_occ_subset_MAMMALS,
  ind_vect         = c("fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)
fric_MAMMALS <- fric_indices_MAMMALS$functional_diversity_indices


# Save observed data:
saveRDS(fmpd_fori_indices_MAMMALS, here::here("transformed_data",
                                            "div_values_null_models",
                                            "FD_FMPD_FOri_50km_MAMMALS.rds"))
saveRDS(fric_indices_MAMMALS, here::here("transformed_data",
                                       "div_values_null_models",
                                       "FD_FRic_50km_MAMMALS.rds"))

# 8 - Compute FD Null Models ===================================================


# Note: For each grid cell, same species richness but different species ...
# ... compositions: as many null asb as wanted through the `nb_asb_rep` input:


FD_null_asb_list <- compute.null.model.FD.paral(sp_faxes_coord = sp_faxes_coord_MAMMALS[, c("PC1",
                                                                                          "PC2",
                                                                                          "PC3",
                                                                                          "PC4")],
                                                faxes_nm_vect = c("PC1", "PC2", "PC3",
                                                                  "PC4"),
                                                sp_asb_df = sp_occ_MAMMALS,
                                                nb_asb_rep = 500)

fric_null_models <- FD_null_asb_list$fric
fmpd_null_models <- FD_null_asb_list$fmpd
fori_null_models <- FD_null_asb_list$fori

saveRDS(fric_null_models, here::here("transformed_data",
                                     "div_values_null_models",
                                     "FD_FRic_null_models_50km_MAMMALS.rds"))
saveRDS(fmpd_null_models, here::here("transformed_data",
                                     "div_values_null_models",
                                     "FD_FMPD_null_models_50km_MAMMALS.rds"))
saveRDS(fori_null_models, here::here("transformed_data",
                                     "div_values_null_models",
                                     "FD_FOri_null_models_50km_MAMMALS.rds"))



# 9 - Compute SES ==============================================================


# Note SES: If the test were a one-sided test of whether the value observed was
# significantly lower than expected, we would require a P -value less than or
# equal to 0.05. If the test were a one-sided test of whether the value
# observed was significantly higher than expected, we would require a P -value
# greater than or equal to 0.95. If the test were two sided, we would require
# P -values less than or equal to 0.025 or greater than or equal to 0.975.


# Load null model data:
MAMMALS_null_model_fric <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                            "FD_FRic_null_models_50km_MAMMALS.rds"))
MAMMALS_null_model_fmpd <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                            "FD_FMPD_null_models_50km_MAMMALS.rds"))
MAMMALS_null_model_fori <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                            "FD_FOri_null_models_50km_MAMMALS.rds"))

# Idgrid as rownames:
MAMMALS_null_model_fric <- tibble::column_to_rownames(MAMMALS_null_model_fric,
                                                    var = "Idgrid")
MAMMALS_null_model_fmpd <- tibble::column_to_rownames(MAMMALS_null_model_fmpd,
                                                    var = "Idgrid")
MAMMALS_null_model_fori <- tibble::column_to_rownames(MAMMALS_null_model_fori,
                                                    var = "Idgrid")
# Load the actual values of FD indices:
MAMMALS_FD_FRic_50km <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "FD_FRic_50km_MAMMALS.rds"))
MAMMALS_FD_FMPD_FOri_50km <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "FD_FMPD_FOri_50km_MAMMALS.rds"))

#Prepare for the use in the function:
MAMMALS_FD_FRic_50km_final <- MAMMALS_FD_FRic_50km$functional_diversity_indices %>%
  tibble::rownames_to_column(var = "Idgrid") %>%
  dplyr::select(c("Idgrid", "fric")) %>%
  dplyr::rename("metric" = fric)
MAMMALS_FD_FOri_50km_final <- MAMMALS_FD_FMPD_FOri_50km$functional_diversity_indices %>%
  tibble::rownames_to_column(var = "Idgrid") %>%
  dplyr::select(c("Idgrid", "fori")) %>%
  dplyr::rename("metric" = fori)
MAMMALS_FD_FMPD_50km_final <- MAMMALS_FD_FMPD_FOri_50km$functional_diversity_indices %>%
  tibble::rownames_to_column(var = "Idgrid") %>%
  dplyr::select(c("Idgrid", "fmpd")) %>%
  dplyr::rename("metric" = fmpd)

# Compute SES for FRic FD:
MAMMALS_ses_fric_df <- compute.null.model.metrics(null_model_df = MAMMALS_null_model_fric,
                                                null_metric_to_compute = c("ses"),
                                                ind_values_df = MAMMALS_FD_FRic_50km_final)
saveRDS(MAMMALS_ses_fric_df, here::here("transformed_data",
                                      "div_values_null_models",
                                      "FD_FRic_null_models_metrics_50km_MAMMALS.rds"))


# Compute SES ratios for FMPD:
MAMMALS_ses_fmpd_df <- compute.null.model.metrics(null_model_df = MAMMALS_null_model_fmpd,
                                                null_metric_to_compute = c("ses"),
                                                ind_values_df = MAMMALS_FD_FMPD_50km_final)
saveRDS(MAMMALS_ses_fmpd_df, here::here("transformed_data",
                                      "div_values_null_models",
                                      "FD_FMPD_null_models_metrics_50km_MAMMALS.rds"))


# Compute SES ratios for FOri:
MAMMALS_ses_fori_df <- compute.null.model.metrics(null_model_df = MAMMALS_null_model_fori,
                                                null_metric_to_compute = c("ses"),
                                                ind_values_df = MAMMALS_FD_FOri_50km_final)
saveRDS(MAMMALS_ses_fori_df, here::here("transformed_data",
                                      "div_values_null_models",
                                      "FD_FOri_null_models_metrics_50km_MAMMALS.rds"))


