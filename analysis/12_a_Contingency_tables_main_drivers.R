################################################################################
##
## Script to test whether +/- SES have associated with high/low values of
## ... certain drivers - directionality of each driver for the main ones
## Camille Magneville
##
## 11/2024
##
## 12_a_Contingency_tables_main_drivers.R
##
################################################################################

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# 1 - TREES ====================================================================

# 1 - a - PD ===================================================================

# Note: Get a contingency tables for the main drivers of PD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
faith_ses_trees_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_Faith_null_models_metrics_50km_TREES.rds"))
mpd_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_MPD_null_models_metrics_50km_TREES.rds"))
mntd_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_MNTD_null_models_metrics_50km_TREES.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_faith_trees_df <- dplyr::inner_join(envdriv_full_db,
                                      faith_ses_trees_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")
driv_mpd_trees_df <- dplyr::inner_join(envdriv_full_db,
                                         mpd_ses_trees_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")
driv_mntd_trees_df <- dplyr::inner_join(envdriv_full_db,
                                         mntd_ses_trees_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Past MAT sd, Herbivores Consumption, Depth Mean,

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_faith_trees_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Richness",
                     taxa_nm = "TREES")

# Herbivores cons (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_faith_trees_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Richness",
                     taxa_nm = "TREES")

# Depth Mean (function saves automatically graphs): significant association
driv_faith_trees_df$Depth_mean <- as.numeric(driv_faith_trees_df$Depth_mean)
contingency.analyses(driver_ses_df = driv_faith_trees_df,
                     driver_nm = "Depth_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Richness",
                     taxa_nm = "TREES")

## Dispersion ------------------------------------------------------------------

# Drivers to study: Cl Vel YD decrease

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Cl Vel YD decrease (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_mpd_trees_df,
                     driver_nm = "Past_CCVelYoungerDryas_mean.voccMag",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Dispersion",
                     taxa_nm = "TREES")

## Originality -----------------------------------------------------------------

# Drivers to study: Depth mean

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Depth mean (function saves automatically graphs): significant association
driv_mntd_trees_df$Depth_mean <- as.numeric(driv_mntd_trees_df$Depth_mean)
contingency.analyses(driver_ses_df = driv_mntd_trees_df,
                     driver_nm = "Depth_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Originality",
                     taxa_nm = "TREES")


# 1 - b - FD ===================================================================

# Note: Get a contingency tables for the main drivers of FD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
fric_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "FD_FRic_null_models_metrics_50km_TREES.rds"))
fmpd_ses_trees_df <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "FD_FMPD_null_models_metrics_50km_TREES.rds"))
fori_ses_trees_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FORi_null_models_metrics_50km_TREES.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_fric_trees_df <- dplyr::inner_join(envdriv_full_db,
                                         fric_ses_trees_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")
driv_fmpd_trees_df <- dplyr::inner_join(envdriv_full_db,
                                       fmpd_ses_trees_df[, c("Idgrid", "ses")],
                                       by = "Idgrid")
driv_fori_trees_df <- dplyr::inner_join(envdriv_full_db,
                                        fori_ses_trees_df[, c("Idgrid", "ses")],
                                        by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Growth rate pop, herb cons and Past MAT sd

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Growth rate pop (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_trees_df,
                     driver_nm = "Pr_RatePop_2020_mean",
                     color_nms = c("#e8dbe1", "#ce94b1", "#882255"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "TREES")

# Herbivores cons (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_trees_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "TREES")

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_trees_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "TREES")

## Dispersion ------------------------------------------------------------------

# Drivers to study: Cl velocity LGM and Past MAT sd

# Cl. Vel. LGM (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fmpd_trees_df,
                     driver_nm = "Past_CCVelLGM_mean.voccMag",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Dispersion",
                     taxa_nm = "TREES")
# Past MAT sd (function saves automatically graphs): Significant association
contingency.analyses(driver_ses_df = driv_fmpd_trees_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Dispersion",
                     taxa_nm = "TREES")

# Originality ------------------------------------------------------------------

# Drivers to study: Cl Velocity YD decrease

# Cl. Vel. YD decrease (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fori_trees_df,
                     driver_nm = "Past_CCVelYoungerDryas_mean.voccMag",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Originality",
                     taxa_nm = "TREES")



# 2 - BIRDS ====================================================================

# 2 - a - PD ===================================================================

# Note: Get a contingency tables for the main drivers of PD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
faith_ses_birds_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_BIRDS.rds"))
mpd_ses_birds_df <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_MPD_null_models_metrics_50km_BIRDS.rds"))
mntd_ses_birds_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_MNTD_null_models_metrics_50km_BIRDS.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_faith_birds_df <- dplyr::inner_join(envdriv_full_db,
                                         faith_ses_birds_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")
driv_mpd_birds_df <- dplyr::inner_join(envdriv_full_db,
                                       mpd_ses_birds_df[, c("Idgrid", "ses")],
                                       by = "Idgrid")
driv_mntd_birds_df <- dplyr::inner_join(envdriv_full_db,
                                        mntd_ses_birds_df[, c("Idgrid", "ses")],
                                        by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Past MAT

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_faith_birds_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Richness",
                     taxa_nm = "BIRDS")

## Dispersion ------------------------------------------------------------------

# Drivers to study: MAT mean

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# MAT Mean (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_mpd_birds_df,
                     driver_nm = "Present_MAT_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Dispersion",
                     taxa_nm = "BIRDS")

## Originality -----------------------------------------------------------------

# Drivers to study: Past MAT sd

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_mntd_birds_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Originality",
                     taxa_nm = "BIRDS")

# 2 - b - FD ===================================================================

# Note: Get a contingency tables for the main drivers of FD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
fric_ses_birds_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FRic_null_models_metrics_50km_BIRDS.rds"))
fmpd_ses_birds_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FMPD_null_models_metrics_50km_BIRDS.rds"))
fori_ses_birds_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FORi_null_models_metrics_50km_BIRDS.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_fric_birds_df <- dplyr::inner_join(envdriv_full_db,
                                        fric_ses_birds_df[, c("Idgrid", "ses")],
                                        by = "Idgrid")
driv_fmpd_birds_df <- dplyr::inner_join(envdriv_full_db,
                                        fmpd_ses_birds_df[, c("Idgrid", "ses")],
                                        by = "Idgrid")
driv_fori_birds_df <- dplyr::inner_join(envdriv_full_db,
                                        fori_ses_birds_df[, c("Idgrid", "ses")],
                                        by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: MAT mean

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# MAT Mean (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_birds_df,
                     driver_nm = "Present_MAT_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "BIRDS")


## Dispersion ------------------------------------------------------------------

# Drivers to study: MAT Mean and AI mean

# MAT Mean (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fmpd_birds_df,
                     driver_nm = "Present_MAT_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Dispersion",
                     taxa_nm = "BIRDS")

# AI Mean (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fmpd_birds_df,
                     driver_nm = "Present_AI_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Dispersion",
                     taxa_nm = "BIRDS")

# Originality ------------------------------------------------------------------

# Drivers to study: MAT Mean

# MAT Mean (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fori_birds_df,
                     driver_nm = "Present_MAT_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Originality",
                     taxa_nm = "BIRDS")

# 3 - BUTTERFLIES ====================================================================

# 3 - a - PD ===================================================================

# Note: Get a contingency tables for the main drivers of PD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
faith_ses_butterflies_df <- readRDS(here::here("transformed_data",
                                               "div_values_null_models",
                                               "PD_Faith_null_models_metrics_50km_BUTTERFLIES.rds"))
mpd_ses_butterflies_df <- readRDS(here::here("transformed_data",
                                             "div_values_null_models",
                                             "PD_MPD_null_models_metrics_50km_BUTTERFLIES.rds"))
mntd_ses_butterflies_df <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "PD_MNTD_null_models_metrics_50km_BUTTERFLIES.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_faith_butterflies_df <- dplyr::inner_join(envdriv_full_db,
                                               faith_ses_butterflies_df[, c("Idgrid", "ses")],
                                               by = "Idgrid")
driv_mpd_butterflies_df <- dplyr::inner_join(envdriv_full_db,
                                             mpd_ses_butterflies_df[, c("Idgrid", "ses")],
                                             by = "Idgrid")
driv_mntd_butterflies_df <- dplyr::inner_join(envdriv_full_db,
                                              mntd_ses_butterflies_df[, c("Idgrid", "ses")],
                                              by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Past MAT sd

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_faith_butterflies_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Richness",
                     taxa_nm = "BUTTERFLIES")

## Dispersion ------------------------------------------------------------------

# Drivers to study: MAT mean

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# MAT Mean (function saves automatically graphs): NO significant association
contingency.analyses(driver_ses_df = driv_mpd_butterflies_df,
                     driver_nm = "Present_MAT_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Dispersion",
                     taxa_nm = "BUTTERFLIES")
plot(x = driv_mpd_butterflies_df$ses, y = driv_mpd_butterflies_df$Present_MAT_mean)

## Originality -----------------------------------------------------------------

# Drivers to study: Past MAT sd

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_mntd_butterflies_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Originality",
                     taxa_nm = "BUTTERFLIES")

# 3 - b - FD ===================================================================

# Note: Get a contingency tables for the main drivers of FD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
fric_ses_butterflies_df <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "FD_FRic_null_models_metrics_50km_BUTTERFLIES.rds"))
fmpd_ses_butterflies_df <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "FD_FMPD_null_models_metrics_50km_BUTTERFLIES.rds"))
fori_ses_butterflies_df <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "FD_FORi_null_models_metrics_50km_BUTTERFLIES.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_fric_butterflies_df <- dplyr::inner_join(envdriv_full_db,
                                              fric_ses_butterflies_df[, c("Idgrid", "ses")],
                                              by = "Idgrid")
driv_fmpd_butterflies_df <- dplyr::inner_join(envdriv_full_db,
                                              fmpd_ses_butterflies_df[, c("Idgrid", "ses")],
                                              by = "Idgrid")
driv_fori_butterflies_df <- dplyr::inner_join(envdriv_full_db,
                                              fori_ses_butterflies_df[, c("Idgrid", "ses")],
                                              by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Clim Veloc Holocene and Past MAT sd

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Cl Vel Holocene (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_butterflies_df,
                     driver_nm = "Past_CCVelShortTerm_mean.voccMag",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "BUTTERFLIES")

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_butterflies_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "BUTTERFLIES")

## Dispersion ------------------------------------------------------------------

# Drivers to study: Cl Velocity Holocene

# Cl. Vel. Holocene (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fmpd_butterflies_df,
                     driver_nm = "Past_CCVelShortTerm_mean.voccMag",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Dispersion",
                     taxa_nm = "BUTTERFLIES")

## Originality ------------------------------------------------------------------

# Drivers to study: AI mean

# AI Mean (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fori_butterflies_df,
                     driver_nm = "Present_AI_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Originality",
                     taxa_nm = "BUTTERFLIES")

# 4 - REPTILES ====================================================================

# 4 - a - PD ===================================================================

# Note: Get a contingency tables for the main drivers of PD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
faith_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                            "PD_Faith_null_models_metrics_50km_REPTILES.rds"))
mpd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "PD_MPD_null_models_metrics_50km_REPTILES.rds"))
mntd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "PD_MNTD_null_models_metrics_50km_REPTILES.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_faith_reptiles_df <- dplyr::inner_join(envdriv_full_db,
                                            faith_ses_reptiles_df[, c("Idgrid", "ses")],
                                            by = "Idgrid")
driv_mpd_reptiles_df <- dplyr::inner_join(envdriv_full_db,
                                          mpd_ses_reptiles_df[, c("Idgrid", "ses")],
                                          by = "Idgrid")
driv_mntd_reptiles_df <- dplyr::inner_join(envdriv_full_db,
                                           mntd_ses_reptiles_df[, c("Idgrid", "ses")],
                                           by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Herb consumption

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Herb cons (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_faith_reptiles_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Richness",
                     taxa_nm = "REPTILES")

## Dispersion ------------------------------------------------------------------

# Drivers to study: Past MAT sd

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_mpd_reptiles_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Dispersion",
                     taxa_nm = "REPTILES")

## Originality -----------------------------------------------------------------

# Drivers to study: Herb consumption

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Herb cons (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_mntd_reptiles_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Originality",
                     taxa_nm = "REPTILES")

# MAT mean (quite 75% too): significant association
contingency.analyses(driver_ses_df = driv_mntd_reptiles_df,
                     driver_nm = "Present_MAT_mean",
                     color_nms = c("#dfeae8", "#acd6cf", "#2dae99"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Originality",
                     taxa_nm = "REPTILES")

# 4 - b - FD ===================================================================

# Note: Get a contingency tables for the main drivers of FD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
fric_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FRic_null_models_metrics_50km_REPTILES.rds"))
fmpd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FMPD_null_models_metrics_50km_REPTILES.rds"))
fori_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FORi_null_models_metrics_50km_REPTILES.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_fric_reptiles_df <- dplyr::inner_join(envdriv_full_db,
                                           fric_ses_reptiles_df[, c("Idgrid", "ses")],
                                           by = "Idgrid")
driv_fmpd_reptiles_df <- dplyr::inner_join(envdriv_full_db,
                                           fmpd_ses_reptiles_df[, c("Idgrid", "ses")],
                                           by = "Idgrid")
driv_fori_reptiles_df <- dplyr::inner_join(envdriv_full_db,
                                           fori_ses_reptiles_df[, c("Idgrid", "ses")],
                                           by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Herb Consumption

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Herbivores cons (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_reptiles_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "REPTILES")

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_reptiles_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "REPTILES")

## Dispersion ------------------------------------------------------------------

# Drivers to study: Past MAT sd

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fmpd_reptiles_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Dispersion",
                     taxa_nm = "REPTILES")


# Originality ------------------------------------------------------------------

# Drivers to study: Past MAT sd, Herb Consumption (nearly 75% - 74% while past MAT 75%)

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fori_reptiles_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Originality",
                     taxa_nm = "REPTILES")

# Herbivores cons (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fori_reptiles_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Originality",
                     taxa_nm = "REPTILES")




# 5 - MAMMALS ====================================================================

# 5 - a - PD ===================================================================

# Note: Get a contingency tables for the main drivers of PD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
faith_ses_mammals_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "PD_Faith_null_models_metrics_50km_MAMMALS.rds"))
mpd_ses_mammals_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_MPD_null_models_metrics_50km_MAMMALS.rds"))
mntd_ses_mammals_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "PD_MNTD_null_models_metrics_50km_MAMMALS.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_faith_mammals_df <- dplyr::inner_join(envdriv_full_db,
                                           faith_ses_mammals_df[, c("Idgrid", "ses")],
                                           by = "Idgrid")
driv_mpd_mammals_df <- dplyr::inner_join(envdriv_full_db,
                                         mpd_ses_mammals_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")
driv_mntd_mammals_df <- dplyr::inner_join(envdriv_full_db,
                                          mntd_ses_mammals_df[, c("Idgrid", "ses")],
                                          by = "Idgrid")

## Richness --------------------------------------------------------------------

## Note : Nb of cells with - SES really low : only 25 so can't use it as a threshold
## ...

# Drivers to study: Herb consumption

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Herb cons (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_faith_mammals_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Richness",
                     taxa_nm = "MAMMALS")

## Dispersion ------------------------------------------------------------------

## Note : Nb of cells with + SES really low : only 19 so can't use it as a threshold
## ...
# Drivers to study: Past MAT sd

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): NO significant association
contingency.analyses(driver_ses_df = driv_mpd_mammals_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Dispersion",
                     taxa_nm = "MAMMALS")
# With extreme values: expected sample size < 5 (with Yate's corr non signif)
contingency.analyses(driver_ses_df = driv_mpd_mammals_df,
                       driver_nm = "Past_MAT_sd",
                       color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                       threshold_type = "extremes",
                       drivers_nm_df = drivers_nm_df,
                       facet_nm = "PD",
                       dim_nm = "Dispersion",
                       taxa_nm = "MAMMALS")

## Originality -----------------------------------------------------------------

## Note : Nb of cells with - SES really low : only 5 so can't use it as a threshold
## ...

# Drivers to study: Past MAT sd, Herb Cons

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): NO significant association and < 5
contingency.analyses(driver_ses_df = driv_mntd_mammals_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Originality",
                     taxa_nm = "MAMMALS")
# With extreme values: no signif association + < 5:
contingency.analyses(driver_ses_df = driv_mntd_mammals_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Originality",
                     taxa_nm = "MAMMALS")
plot(x = driv_mntd_mammals_df$ses, y = driv_mntd_mammals_df$Past_MAT_sd)

# Herb cons (function saves automatically graphs): significant association BUT < 5
contingency.analyses(driver_ses_df = driv_mntd_mammals_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Originality",
                     taxa_nm = "MAMMALS")

# 5 - b - FD ===================================================================

# Note: Get a contingency tables for the main drivers of FD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)


## Load data -------------------------------------------------------------------

# Load diversity data:
fric_ses_mammals_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "FD_FRic_null_models_metrics_50km_MAMMALS.rds"))
fmpd_ses_mammals_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "FD_FMPD_null_models_metrics_50km_MAMMALS.rds"))
fori_ses_mammals_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "FD_FORi_null_models_metrics_50km_MAMMALS.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_fric_mammals_df <- dplyr::inner_join(envdriv_full_db,
                                          fric_ses_mammals_df[, c("Idgrid", "ses")],
                                          by = "Idgrid")
driv_fmpd_mammals_df <- dplyr::inner_join(envdriv_full_db,
                                          fmpd_ses_mammals_df[, c("Idgrid", "ses")],
                                          by = "Idgrid")
driv_fori_mammals_df <- dplyr::inner_join(envdriv_full_db,
                                          fori_ses_mammals_df[, c("Idgrid", "ses")],
                                          by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Past MAT sd, Herb richn

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_mammals_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "MAMMALS")

# Herbivores richn (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fric_mammals_df,
                     driver_nm = "HerbRichn_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Richness",
                     taxa_nm = "MAMMALS")

## Dispersion ------------------------------------------------------------------

# Drivers to study: Herb richness and consumption

# Herbivores richn (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fmpd_mammals_df,
                     driver_nm = "HerbRichn_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Dispersion",
                     taxa_nm = "MAMMALS")

# Herbivores consumption (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fmpd_mammals_df,
                     driver_nm = "HerbCons_sum",
                     color_nms = c("#f2eedb", "#f0e3a5","#dab811"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Dispersion",
                     taxa_nm = "MAMMALS")

# Originality ------------------------------------------------------------------

# Drivers to study: Past MAT sd, Cl Vel YD decrease

# Past MAT sd (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fori_mammals_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Originality",
                     taxa_nm = "MAMMALS")

# Cl Vel YD decrease (function saves automatically graphs): significant association
contingency.analyses(driver_ses_df = driv_fori_mammals_df,
                     driver_nm = "Past_CCVelYoungerDryas_mean.voccMag",
                     color_nms = c("#ccdbe2", "#73b2d1", "#0881bd"),
                     threshold_type = "extremes_medium",
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "FD",
                     dim_nm = "Originality",
                     taxa_nm = "MAMMALS")


