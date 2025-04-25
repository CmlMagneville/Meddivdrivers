################################################################################
##
## Script to test
## ... correlations among diversity metrics
##
## Camille Magneville
##
## 11/2024 - 01/2025
##
## 15_Checks_diversity_correlation.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# 1 - Check residuals on a map + correl ========================================


# 1 - a - Load random forest data for each random forest =======================

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))

# TREES:
trees_pd_richn_rf <- readRDS(here::here("transformed_data",
                                        "residuals_rf_trees_PD_Faith_50.rds"))
trees_pd_disp_rf <- readRDS(here::here("transformed_data",
                                      "residuals_rf_trees_PD_mpd_50.rds"))
trees_pd_orig_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_trees_PD_mntd_50.rds"))
trees_fd_richn_rf <- readRDS(here::here("transformed_data",
                                        "residuals_rf_trees_FD_fric_50.rds"))
trees_fd_disp_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_trees_FD_fmpd_50.rds"))
trees_fd_orig_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_trees_FD_fori_50.rds"))

# BIRDS
birds_pd_richn_rf <- readRDS(here::here("transformed_data",
                                        "residuals_rf_birds_PD_Faith_50.rds"))
birds_pd_disp_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_birds_PD_mpd_50.rds"))
birds_pd_orig_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_birds_PD_mntd_50.rds"))
birds_fd_richn_rf <- readRDS(here::here("transformed_data",
                                        "residuals_rf_birds_FD_fric_50.rds"))
birds_fd_disp_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_birds_FD_fmpd_50.rds"))
birds_fd_orig_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_birds_FD_fori_50.rds"))

# REPTILES
reptiles_pd_richn_rf <- readRDS(here::here("transformed_data",
                                           "residuals_rf_reptiles_PD_Faith_50.rds"))
reptiles_pd_disp_rf <- readRDS(here::here("transformed_data",
                                          "residuals_rf_reptiles_PD_mpd_50.rds"))
reptiles_pd_orig_rf <- readRDS(here::here("transformed_data",
                                          "residuals_rf_reptiles_PD_mntd_50.rds"))
reptiles_fd_richn_rf <- readRDS(here::here("transformed_data",
                                           "residuals_rf_reptiles_FD_fric_50.rds"))
reptiles_fd_disp_rf <- readRDS(here::here("transformed_data",
                                          "residuals_rf_reptiles_FD_fmpd_50.rds"))
reptiles_fd_orig_rf <- readRDS(here::here("transformed_data",
                                          "residuals_rf_reptiles_FD_fori_50.rds"))

# MAMMALS
mammals_pd_richn_rf <- readRDS(here::here("transformed_data",
                                          "residuals_rf_mammals_PD_Faith_50.rds"))
mammals_pd_disp_rf <- readRDS(here::here("transformed_data",
                                         "residuals_rf_mammals_PD_mpd_50.rds"))
mammals_pd_orig_rf <- readRDS(here::here("transformed_data",
                                         "residuals_rf_mammals_PD_mntd_50.rds"))
mammals_fd_richn_rf <- readRDS(here::here("transformed_data",
                                          "residuals_rf_mammals_FD_fric_50.rds"))
mammals_fd_disp_rf <- readRDS(here::here("transformed_data",
                                         "residuals_rf_mammals_FD_fmpd_50.rds"))
mammals_fd_orig_rf <- readRDS(here::here("transformed_data",
                                         "residuals_rf_mammals_FD_fori_50.rds"))

# BUTTERFLIES
butterflies_pd_richn_rf <- readRDS(here::here("transformed_data",
                                              "residuals_rf_butterflies_PD_Faith_50.rds"))
butterflies_pd_disp_rf <- readRDS(here::here("transformed_data",
                                             "residuals_rf_butterflies_PD_mpd_50.rds"))
butterflies_pd_orig_rf <- readRDS(here::here("transformed_data",
                                             "residuals_rf_butterflies_PD_mntd_50.rds"))
butterflies_fd_richn_rf <- readRDS(here::here("transformed_data",
                                              "residuals_rf_butterflies_FD_fric_50.rds"))
butterflies_fd_disp_rf <- readRDS(here::here("transformed_data",
                                             "residuals_rf_butterflies_FD_fmpd_50.rds"))
butterflies_fd_orig_rf <- readRDS(here::here("transformed_data",
                                             "residuals_rf_butterflies_FD_fori_50.rds"))


# 1 - b - Compute mean absolute residuals over the 100 random forests ===================


# TREES: compute mean :
trees_pd_richn_rf$mean_residual <- rowMeans(trees_pd_richn_rf[, -1])
trees_pd_disp_rf$mean_residual <- rowMeans(trees_pd_disp_rf[, -1])
trees_pd_orig_rf$mean_residual <- rowMeans(trees_pd_orig_rf[, -1])
trees_fd_richn_rf$mean_residual <- rowMeans(trees_fd_richn_rf[, -1])
trees_fd_disp_rf$mean_residual <- rowMeans(trees_fd_disp_rf[, -1])
trees_fd_orig_rf$mean_residual <- rowMeans(trees_fd_orig_rf[, -1])
# and then take absolute value:
trees_pd_richn_rf$mean_residual <- abs(trees_pd_richn_rf$mean_residual)
trees_pd_disp_rf$mean_residual <- abs(trees_pd_disp_rf$mean_residual)
trees_pd_orig_rf$mean_residual <- abs(trees_pd_orig_rf$mean_residual)
trees_fd_richn_rf$mean_residual <- abs(trees_fd_richn_rf$mean_residual)
trees_fd_disp_rf$mean_residual <- abs(trees_fd_disp_rf$mean_residual)
trees_fd_orig_rf$mean_residual <- abs(trees_fd_orig_rf$mean_residual)

# BIRDS: compute mean :
birds_pd_richn_rf$mean_residual <- rowMeans(birds_pd_richn_rf[, -1])
birds_pd_disp_rf$mean_residual <- rowMeans(birds_pd_disp_rf[, -1])
birds_pd_orig_rf$mean_residual <- rowMeans(birds_pd_orig_rf[, -1])
birds_fd_richn_rf$mean_residual <- rowMeans(birds_fd_richn_rf[, -1])
birds_fd_disp_rf$mean_residual <- rowMeans(birds_fd_disp_rf[, -1])
birds_fd_orig_rf$mean_residual <- rowMeans(birds_fd_orig_rf[, -1])
# and then take absolute value:
birds_pd_richn_rf$mean_residual <- abs(birds_pd_richn_rf$mean_residual)
birds_pd_disp_rf$mean_residual <- abs(birds_pd_disp_rf$mean_residual)
birds_pd_orig_rf$mean_residual <- abs(birds_pd_orig_rf$mean_residual)
birds_fd_richn_rf$mean_residual <- abs(birds_fd_richn_rf$mean_residual)
birds_fd_disp_rf$mean_residual <- abs(birds_fd_disp_rf$mean_residual)
birds_fd_orig_rf$mean_residual <- abs(birds_fd_orig_rf$mean_residual)

# REPTILES: compute mean :
reptiles_pd_richn_rf$mean_residual <- rowMeans(reptiles_pd_richn_rf[, -1])
reptiles_pd_disp_rf$mean_residual <- rowMeans(reptiles_pd_disp_rf[, -1])
reptiles_pd_orig_rf$mean_residual <- rowMeans(reptiles_pd_orig_rf[, -1])
reptiles_fd_richn_rf$mean_residual <- rowMeans(reptiles_fd_richn_rf[, -1])
reptiles_fd_disp_rf$mean_residual <- rowMeans(reptiles_fd_disp_rf[, -1])
reptiles_fd_orig_rf$mean_residual <- rowMeans(reptiles_fd_orig_rf[, -1])
# and then take absolute value:
reptiles_pd_richn_rf$mean_residual <- abs(reptiles_pd_richn_rf$mean_residual)
reptiles_pd_disp_rf$mean_residual <- abs(reptiles_pd_disp_rf$mean_residual)
reptiles_pd_orig_rf$mean_residual <- abs(reptiles_pd_orig_rf$mean_residual)
reptiles_fd_richn_rf$mean_residual <- abs(reptiles_fd_richn_rf$mean_residual)
reptiles_fd_disp_rf$mean_residual <- abs(reptiles_fd_disp_rf$mean_residual)
reptiles_fd_orig_rf$mean_residual <- abs(reptiles_fd_orig_rf$mean_residual)

# MAMMALS: compute mean :
mammals_pd_richn_rf$mean_residual <- rowMeans(mammals_pd_richn_rf[, -1])
mammals_pd_disp_rf$mean_residual <- rowMeans(mammals_pd_disp_rf[, -1])
mammals_pd_orig_rf$mean_residual <- rowMeans(mammals_pd_orig_rf[, -1])
mammals_fd_richn_rf$mean_residual <- rowMeans(mammals_fd_richn_rf[, -1])
mammals_fd_disp_rf$mean_residual <- rowMeans(mammals_fd_disp_rf[, -1])
mammals_fd_orig_rf$mean_residual <- rowMeans(mammals_fd_orig_rf[, -1])
# and then take absolute value:
mammals_pd_richn_rf$mean_residual <- abs(mammals_pd_richn_rf$mean_residual)
mammals_pd_disp_rf$mean_residual <- abs(mammals_pd_disp_rf$mean_residual)
mammals_pd_orig_rf$mean_residual <- abs(mammals_pd_orig_rf$mean_residual)
mammals_fd_richn_rf$mean_residual <- abs(mammals_fd_richn_rf$mean_residual)
mammals_fd_disp_rf$mean_residual <- abs(mammals_fd_disp_rf$mean_residual)
mammals_fd_orig_rf$mean_residual <- abs(mammals_fd_orig_rf$mean_residual)

# BUTTERFLIES: compute mean :
butterflies_pd_richn_rf$mean_residual <- rowMeans(butterflies_pd_richn_rf[, -1])
butterflies_pd_disp_rf$mean_residual <- rowMeans(butterflies_pd_disp_rf[, -1])
butterflies_pd_orig_rf$mean_residual <- rowMeans(butterflies_pd_orig_rf[, -1])
butterflies_fd_richn_rf$mean_residual <- rowMeans(butterflies_fd_richn_rf[, -1])
butterflies_fd_disp_rf$mean_residual <- rowMeans(butterflies_fd_disp_rf[, -1])
butterflies_fd_orig_rf$mean_residual <- rowMeans(butterflies_fd_orig_rf[, -1])
# and then take absolute value:
butterflies_pd_richn_rf$mean_residual <- abs(butterflies_pd_richn_rf$mean_residual)
butterflies_pd_disp_rf$mean_residual <- abs(butterflies_pd_disp_rf$mean_residual)
butterflies_pd_orig_rf$mean_residual <- abs(butterflies_pd_orig_rf$mean_residual)
butterflies_fd_richn_rf$mean_residual <- abs(butterflies_fd_richn_rf$mean_residual)
butterflies_fd_disp_rf$mean_residual <- abs(butterflies_fd_disp_rf$mean_residual)
butterflies_fd_orig_rf$mean_residual <- abs(butterflies_fd_orig_rf$mean_residual)

# 1 - c - Plot a map of residuals for each taxa and div dimension ==============


# TREES
# PD Richness:
# second null models doesn't have name:
colnames(trees_pd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = trees_pd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_richness_TREES",
                  save = TRUE)
# PD Dispersion:
# second null models doesn't have name:
colnames(trees_pd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = trees_pd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_dispersion_TREES",
                  save = TRUE)
# PD Originality:
# second null models doesn't have name:
colnames(trees_pd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = trees_pd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_originality_TREES",
                  save = TRUE)
# FD Richness:
# second null models doesn't have name:
colnames(trees_fd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = trees_fd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_richness_TREES",
                  save = TRUE)
# FD Dispersion:
# second null models doesn't have name:
colnames(trees_fd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = trees_fd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_dispersion_TREES",
                  save = TRUE)
# FD Originality:
# second null models doesn't have name:
colnames(trees_fd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = trees_fd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_originality_TREES",
                  save = TRUE)

# BIRDS
# PD Richness:
# second null models doesn't have name:
colnames(birds_pd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = birds_pd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_richness_BIRDS",
                  save = TRUE)
# PD Dispersion:
# second null models doesn't have name:
colnames(birds_pd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = birds_pd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_dispersion_BIRDS",
                  save = TRUE)
# PD Originality:
# second null models doesn't have name:
colnames(birds_pd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = birds_pd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_originality_BIRDS",
                  save = TRUE)
# FD Richness:
# second null models doesn't have name:
colnames(birds_fd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = birds_fd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_richness_BIRDS",
                  save = TRUE)
# FD Dispersion:
# second null models doesn't have name:
colnames(birds_fd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = birds_fd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_dispersion_BIRDS",
                  save = TRUE)
# FD Originality:
# second null models doesn't have name:
colnames(birds_fd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = birds_fd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_originality_BIRDS",
                  save = TRUE)

# REPTILES
# PD Richness:
# second null models doesn't have name:
colnames(reptiles_pd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = reptiles_pd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_richness_REPTILES",
                  save = TRUE)
# PD Dispersion:
# second null models doesn't have name:
colnames(reptiles_pd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = reptiles_pd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_dispersion_REPTILES",
                  save = TRUE)
# PD Originality:
# second null models doesn't have name:
colnames(reptiles_pd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = reptiles_pd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_originality_REPTILES",
                  save = TRUE)
# FD Richness:
# second null models doesn't have name:
colnames(reptiles_fd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = reptiles_fd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_richness_REPTILES",
                  save = TRUE)
# FD Dispersion:
# second null models doesn't have name:
colnames(reptiles_fd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = reptiles_fd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_dispersion_REPTILES",
                  save = TRUE)
# FD Originality:
# second null models doesn't have name:
colnames(reptiles_fd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = reptiles_fd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_originality_REPTILES",
                  save = TRUE)

# MAMMALS
# PD Richness:
# second null models doesn't have name:
colnames(mammals_pd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = mammals_pd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_richness_MAMMALS",
                  save = TRUE)
# PD Dispersion:
# second null models doesn't have name:
colnames(mammals_pd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = mammals_pd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_dispersion_MAMMALS",
                  save = TRUE)
# PD Originality:
# second null models doesn't have name:
colnames(mammals_pd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = mammals_pd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_originality_MAMMALS",
                  save = TRUE)
# FD Richness:
# second null models doesn't have name:
colnames(mammals_fd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = mammals_fd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_richness_MAMMALS",
                  save = TRUE)
# FD Dispersion:
# second null models doesn't have name:
colnames(mammals_fd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = mammals_fd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_dispersion_MAMMALS",
                  save = TRUE)
# FD Originality:
# second null models doesn't have name:
colnames(mammals_fd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = mammals_fd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_originality_MAMMALS",
                  save = TRUE)

# BUTTERFLIES
# PD Richness:
# second null models doesn't have name:
colnames(butterflies_pd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = butterflies_pd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_richness_BUTTERFLIES",
                  save = TRUE)
# PD Dispersion:
# second null models doesn't have name:
colnames(butterflies_pd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = butterflies_pd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_dispersion_BUTTERFLIES",
                  save = TRUE)
# PD Originality:
# second null models doesn't have name:
colnames(butterflies_pd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = butterflies_pd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "PD_originality_BUTTERFLIES",
                  save = TRUE)
# FD Richness:
# second null models doesn't have name:
colnames(butterflies_fd_richn_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = butterflies_fd_richn_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_richness_BUTTERFLIES",
                  save = TRUE)
# FD Dispersion:
# second null models doesn't have name:
colnames(butterflies_fd_disp_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = butterflies_fd_disp_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_dispersion_BUTTERFLIES",
                  save = TRUE)
# FD Originality:
# second null models doesn't have name:
colnames(butterflies_fd_orig_rf)[2] <- "V2"
drivers.maps.plot(drivers_df = butterflies_fd_orig_rf,
                  driver_nm = "mean_residual",
                  grid = grid_50km,
                  col_pal = c("white", "brown"),
                  land_mask = land_mask,
                  type = "residuals",
                  div_dim = "FD_originality_BUTTERFLIES",
                  save = TRUE)


# 1 - d - Compute Moran's I spatial autocorrelation and map it =================


# TREES ------------------------------------------------------------------------
# FD Richness
# Link spatial coordinates and residuals:
spatial_df <- trees_fd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                      k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- trees_fd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Originality
# Link spatial coordinates and residuals:
spatial_df <- trees_fd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# PD Richness
# Link spatial coordinates and residuals:
spatial_df <- trees_pd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- trees_pd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Originality
# Link spatial coordinates and residuals:
spatial_df <- trees_pd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# BUTTERFLIES ------------------------------------------------------------------------
# FD Richness
# Link spatial coordinates and residuals:
spatial_df <- butterflies_fd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- butterflies_fd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Originality
# Link spatial coordinates and residuals:
spatial_df <- butterflies_fd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# PD Richness
# Link spatial coordinates and residuals:
spatial_df <- butterflies_pd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- butterflies_pd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Originality
# Link spatial coordinates and residuals:
spatial_df <- butterflies_pd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# REPTILES ------------------------------------------------------------------------
# FD Richness
# Link spatial coordinates and residuals:
spatial_df <- reptiles_fd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- reptiles_fd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Originality
# Link spatial coordinates and residuals:
spatial_df <- reptiles_fd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# PD Richness
# Link spatial coordinates and residuals:
spatial_df <- reptiles_pd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- reptiles_pd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Originality
# Link spatial coordinates and residuals:
spatial_df <- reptiles_pd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# BIRDS ------------------------------------------------------------------------
# FD Richness
# Link spatial coordinates and residuals:
spatial_df <- birds_fd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- birds_fd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Originality
# Link spatial coordinates and residuals:
spatial_df <- birds_fd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# PD Richness
# Link spatial coordinates and residuals:
spatial_df <- birds_pd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- birds_pd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Originality
# Link spatial coordinates and residuals:
spatial_df <- birds_pd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# MAMMALS ------------------------------------------------------------------------
# FD Richness
# Link spatial coordinates and residuals:
spatial_df <- mammals_fd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- mammals_fd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# FD Originality
# Link spatial coordinates and residuals:
spatial_df <- mammals_fd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# PD Richness
# Link spatial coordinates and residuals:
spatial_df <- mammals_pd_richn_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Dispersion
# Link spatial coordinates and residuals:
spatial_df <- mammals_pd_disp_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)

# PD Originality
# Link spatial coordinates and residuals:
spatial_df <- mammals_pd_orig_rf[, c("Idgrid", "mean_residual")]
spatial_df <- dplyr::left_join(spatial_df,
                               grid_50km,
                               by = "Idgrid")
spatial_df <- dplyr::select(spatial_df, c("Idgrid",
                                          "X_LLC",
                                          "Y_LLC",
                                          "mean_residual"))
# Convert data to spatial points df:
spatial_final_df <- sp::SpatialPointsDataFrame(cbind(spatial_df$X_LLC,
                                                     spatial_df$Y_LLC),
                                               spatial_df)
# Construct a list of spatial weights using the 8 neighbors (queen contig):
weight_list <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(spatial_final_df,
                                                               k = 8)))
# Compute Moran's I using models residuals:
spdep::moran.test(spatial_df$mean_residual, weight_list)


# 2 - Compute correlation between metrics ======================================


# TREES ------------------------------------------------------------------------
# Get raw diversity values:
fd_richn_trees <- readRDS(here::here("transformed_data",
                                     "div_values_null_models",
                                     "FD_FRic_50km_TREES.rds"))
fd_disp_orig_trees <- readRDS(here::here("transformed_data",
                                     "div_values_null_models",
                                     "FD_FMPD_FOri_50km_TREES.rds"))
pd_richn_trees <- readRDS(here::here("transformed_data",
                                     "div_values_null_models",
                                     "PD_Faith_50km_TREES.rds"))
pd_disp_trees <- readRDS(here::here("transformed_data",
                                     "div_values_null_models",
                                     "PD_MPD_50km_TREES.rds"))
pd_orig_trees <- readRDS(here::here("transformed_data",
                                     "div_values_null_models",
                                     "PD_MNTD_50km_TREES.rds"))

# Set rownames to columns for FD indices:
fd_richn_trees <- fd_richn_trees$functional_diversity_indices
fd_richn_trees <- tibble::rownames_to_column(fd_richn_trees, var = "Idgrid")
fd_disp_orig_trees <- fd_disp_orig_trees$functional_diversity_indices
fd_disp_orig_trees <- tibble::rownames_to_column(fd_disp_orig_trees, var = "Idgrid")

# Give names to PD indices:
colnames(pd_richn_trees)[2] <- "Faith"
colnames(pd_disp_trees)[2] <- "MPD"
colnames(pd_orig_trees)[2] <- "MNTD"

# Link diversity values:
trees_div_df <- fd_richn_trees %>%
  dplyr::left_join(fd_disp_orig_trees[, c(1, 3, 4)],
                   by = "Idgrid") %>%
  dplyr::left_join(pd_richn_trees[, c(1, 2)],
                   by = "Idgrid") %>%
  dplyr::left_join(pd_disp_trees[, c(1, 2)],
                   by = "Idgrid") %>%
  dplyr::left_join(pd_orig_trees[, c(1, 2)],
                   by = "Idgrid")

# Compute correlation and correlogram:
correl_matrix <- cor(trees_div_df[, -1])
corrplot::corrplot(correl_matrix, method = "color", type = "upper",
                   col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200),
                   addCoef.col = "black", tl.col = "black", tl.srt = 45)




# 3 - Compute correlation between metrics our=t of null models =================





