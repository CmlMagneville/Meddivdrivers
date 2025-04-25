################################################################################
##
## Script to compute PD null models for Faith's PD, MPD, MNPD - for MAMMALS
##
## Camille Magneville
##
## 09/2024
##
## 2_a_PD_Null_models_MAMMALS.R
##
################################################################################


# !!! NOTE: DONE AT 50*50km SCALE !!!


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ========================================================


# Load occ data:
mammals_occ_df <- readRDS(here::here("transformed_data",
                                   "sp_asb_50km_restricted_MAMMALS.rds"))

# Load mammals phylogeny:
mammals_phylogeny <- ape::read.tree(file = here::here("transformed_data",
                                                    "phylogeny_restricted_MAMMALS.tree"))



# 2 - Compute PD metrics for each grid cell - observed data =============


# Compute Faith's PD for 50km cells:
mammals_PD_Faith_50km <- compute.PD.per.cell(sp_asb_df = mammals_occ_df,
                                           phylo = mammals_phylogeny,
                                           metric_nm = "Faith_index",
                                           grid = "50x50",
                                           taxon = "Mammals")

# Compute Mean Pairwise Distance (MPD) for 50km cells:
mammals_PD_MPD_50km <- compute.PD.per.cell(sp_asb_df = mammals_occ_df,
                                         phylo = mammals_phylogeny,
                                         metric_nm = "MPD",
                                         grid = "50x50",
                                         taxon = "Mammals")

# Compute MNTD for 50km cells:
mammals_PD_MNTD_50km <- compute.PD.per.cell(sp_asb_df = mammals_occ_df,
                                          phylo = mammals_phylogeny,
                                          metric_nm = "MNTD",
                                          grid = "50x50",
                                          taxon = "Mammals")

saveRDS(mammals_PD_Faith_50km, here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_Faith_50km_MAMMALS.rds"))
saveRDS(mammals_PD_MPD_50km, here::here("transformed_data",
                                      "div_values_null_models",
                                      "PD_MPD_50km_MAMMALS.rds"))
saveRDS(mammals_PD_MNTD_50km, here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_MNTD_50km_MAMMALS.rds"))



# 3 - Compute PD null models ============================================


# Note: For each grid cell, same species richness but different species ...
# ... compositions: as many null asb as wanted through the `nb_asb_rep` input:


PD_null_asb_list <- compute.null.model.PD(phylo_tree = mammals_phylogeny,
                                          sp_asb_df = mammals_occ_df,
                                          nb_asb_rep = 500)

faith_null_models <- PD_null_asb_list$faith
mpd_null_models <- PD_null_asb_list$mpd
mntd_null_models <- PD_null_asb_list$mntd

saveRDS(faith_null_models, here::here("transformed_data",
                                      "div_values_null_models",
                                      "PD_Faith_null_models_50km_MAMMALS.rds"))
saveRDS(mpd_null_models, here::here("transformed_data",
                                    "div_values_null_models",
                                    "PD_MPD_null_models_50km_MAMMALS.rds"))
saveRDS(mntd_null_models, here::here("transformed_data",
                                     "div_values_null_models",
                                     "PD_MNTD_null_models_50km_MAMMALS.rds"))


# 4 - Compute SES ========================================================


# Note SES: If the test were a one-sided test of whether the value observed was
# significantly lower than expected, we would require a P -value less than or
# equal to 0.05. If the test were a one-sided test of whether the value
# observed was significantly higher than expected, we would require a P -value
# greater than or equal to 0.95. If the test were two sided, we would require
# P -values less than or equal to 0.025 or greater than or equal to 0.975.


# Load null model data:
mammals_null_model_faith <- readRDS(here::here("transformed_data",
                                             "div_values_null_models",
                                             "PD_Faith_null_models_50km_MAMMALS.rds"))
mammals_null_model_mpd <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "PD_MPD_null_models_50km_MAMMALS.rds"))
mammals_null_model_mntd <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                            "PD_MNTD_null_models_50km_MAMMALS.rds"))

# Load the actual values of PD indices:
mammals_PD_Faith_50km <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "PD_Faith_50km_MAMMALS.rds"))
mammals_PD_MPD_50km <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_MPD_50km_MAMMALS.rds"))
mammals_PD_MNTD_50km <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_MNTD_50km_MAMMALS.rds"))


# Compute SES for Faith PD:
mammals_ses_faith_df <- compute.null.model.metrics(null_model_df = mammals_null_model_faith,
                                                 null_metric_to_compute = c("ses"),
                                                 ind_values_df = mammals_PD_Faith_50km)
saveRDS(mammals_ses_faith_df, here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_Faith_null_models_metrics_50km_MAMMALS.rds"))


# Compute SES ratios for MPD:
mammals_ses_mpd_df <- compute.null.model.metrics(null_model_df = mammals_null_model_mpd,
                                               null_metric_to_compute = c("ses"),
                                               ind_values_df = mammals_PD_MPD_50km)
saveRDS(mammals_ses_mpd_df, here::here("transformed_data",
                                     "div_values_null_models",
                                     "PD_MPD_null_models_metrics_50km_MAMMALS.rds"))


# Compute SES ratios for MNTD:
mammals_ses_mntd_df <- compute.null.model.metrics(null_model_df = mammals_null_model_mntd,
                                                null_metric_to_compute = c("ses"),
                                                ind_values_df = mammals_PD_MNTD_50km)
saveRDS(mammals_ses_mntd_df, here::here("transformed_data",
                                      "div_values_null_models",
                                      "PD_MNTD_null_models_metrics_50km_MAMMALS.rds"))


