################################################################################
##
## Script to plot SES for PD and FD indices for all taxas
##
## Camille Magneville
##
## 04/04/2024
##
## 6_Maps_FD_PD_all_taxas.R
##
################################################################################


# !!! NOTE: DONE AT 50*50km SCALE !!!


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - TREES ####################################################################


# 1 a - Maps SES PD (Faith, MPD, MNTD) =========================================


# Load diversity data:
trees_ses_faith_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                              "PD_Faith_null_models_metrics_50km_TREES.rds"))
trees_ses_mpd_df <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_MPD_null_models_metrics_50km_TREES.rds"))
trees_ses_mntd_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_MNTD_null_models_metrics_50km_TREES.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))


# Transform the data frames to plot:
trees_ses_faith_clean_df <- trees_ses_faith_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
trees_ses_mpd_clean_df <- trees_ses_mpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
trees_ses_mntd_clean_df <- trees_ses_mntd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
trees_SES_faith_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_faith_clean_df,
                                          div_facet_nm = "PD",
                                          metric_nm = "SES Faith",
                                          grid = grid_50km,
                                          plot_title = TRUE,
                                          land_mask = land_mask,
                                          save = TRUE)
trees_SES_MPD_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_mpd_clean_df,
                                          div_facet_nm = "PD",
                                          metric_nm = "SES MPD",
                                          grid = grid_50km,
                                          plot_title = TRUE,
                                          land_mask = land_mask,
                                          save = TRUE)
trees_SES_MNTD_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_mntd_clean_df,
                                        div_facet_nm = "PD",
                                        metric_nm = "SES MNTD",
                                        grid = grid_50km,
                                        plot_title = TRUE,
                                        land_mask = land_mask,
                                        save = TRUE)

# Map the three dimensions of PD together:

# RIGHT ORDER: RICHNESS, MPD and THEN ORIGINALITY
div_per_cell_list <- list("faith" = trees_ses_faith_clean_df,
                          "mpd" = trees_ses_mpd_clean_df,
                          "mntd" = trees_ses_mntd_clean_df)
div_facet_nm <- "PD"
taxa_nm <- "Trees"
grid <- grid_50km
plot_title <- TRUE
save <- TRUE


# 1 b - Maps SES FD (FRic, FMPD, FOri) =========================================

# Load diversity data:
trees_ses_fric_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FRic_null_models_metrics_50km_TREES.rds"))
trees_ses_fmpd_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FMPD_null_models_metrics_50km_TREES.rds"))
trees_ses_fori_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FOri_null_models_metrics_50km_TREES.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))

# Transform the data frames to plot:
trees_ses_fric_clean_df <- trees_ses_fric_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
trees_ses_fmpd_clean_df <- trees_ses_fmpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
trees_ses_fori_clean_df <- trees_ses_fori_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
trees_SES_fric_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_fric_clean_df,
                                         div_facet_nm = "FD",
                                         metric_nm = "SES FRic",
                                         grid = grid_50km,
                                         plot_title = TRUE,
                                         land_mask = land_mask,
                                         save = TRUE)
trees_SES_FMPD_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_fmpd_clean_df,
                                         div_facet_nm = "FD",
                                         metric_nm = "SES FMPD",
                                         grid = grid_50km,
                                         plot_title = TRUE,
                                         land_mask = land_mask,
                                         save = TRUE)
trees_SES_fori_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_fori_clean_df,
                                         div_facet_nm = "FD",
                                         metric_nm = "SES FOri",
                                         grid = grid_50km,
                                         land_mask = land_mask,
                                         plot_title = TRUE,
                                         save = TRUE)





# 2 - BIRDS ####################################################################


# 2 a - Maps SES PD (Faith, MPD, MNTD) =========================================


# Load diversity data:
birds_ses_faith_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_BIRDS.rds"))
birds_ses_mpd_df <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_MPD_null_models_metrics_50km_BIRDS.rds"))
birds_ses_mntd_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_MNTD_null_models_metrics_50km_BIRDS.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))


# Transform the data frames to plot:
birds_ses_faith_clean_df <- birds_ses_faith_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
birds_ses_mpd_clean_df <- birds_ses_mpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
birds_ses_mntd_clean_df <- birds_ses_mntd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
birds_SES_faith_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_faith_clean_df,
                                          div_facet_nm = "PD",
                                          metric_nm = "SES Faith",
                                          grid = grid_50km,
                                          land_mask = land_mask,
                                          plot_title = TRUE,
                                          save = TRUE)
birds_SES_MPD_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_mpd_clean_df,
                                        div_facet_nm = "PD",
                                        metric_nm = "SES MPD",
                                        grid = grid_50km,
                                        land_mask = land_mask,
                                        plot_title = TRUE,
                                        save = TRUE)
birds_SES_MNTD_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_mntd_clean_df,
                                         div_facet_nm = "PD",
                                         metric_nm = "SES MNTD",
                                         grid = grid_50km,
                                         land_mask = land_mask,
                                         plot_title = TRUE,
                                         save = TRUE)




# 2 b - Maps SES FD (FRic, FMPD, FOri) =========================================


# Load diversity data:
birds_ses_fric_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                         "FD_FRic_null_models_metrics_50km_BIRDS.rds"))
birds_ses_fmpd_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FMPD_null_models_metrics_50km_BIRDS.rds"))
birds_ses_fori_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FOri_null_models_metrics_50km_BIRDS.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))


# Transform the data frames to plot:
birds_ses_fric_clean_df <- birds_ses_fric_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
birds_ses_fmpd_clean_df <- birds_ses_fmpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
birds_ses_fori_clean_df <- birds_ses_fori_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
birds_SES_fric_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_fric_clean_df,
                                          div_facet_nm = "FD",
                                          metric_nm = "SES FRic",
                                          grid = grid_50km,
                                          land_mask = land_mask,
                                          plot_title = TRUE,
                                          save = TRUE)
birds_SES_FMPD_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_fmpd_clean_df,
                                        div_facet_nm = "FD",
                                        metric_nm = "SES FMPD",
                                        grid = grid_50km,
                                        land_mask = land_mask,
                                        plot_title = TRUE,
                                        save = TRUE)
birds_SES_fori_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_fori_clean_df,
                                         div_facet_nm = "FD",
                                         metric_nm = "SES FOri",
                                         grid = grid_50km,
                                         land_mask = land_mask,
                                         plot_title = TRUE,
                                         save = TRUE)


# 3 - REPTILES #################################################################



# 2 a - Maps SES PD (Faith, MPD, MNTD) =========================================


# Load diversity data:
reptiles_ses_faith_df <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_REPTILES.rds"))
reptiles_ses_mpd_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                       "PD_MPD_null_models_metrics_50km_REPTILES.rds"))
reptiles_ses_mntd_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                        "PD_MNTD_null_models_metrics_50km_REPTILES.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))


# Transform the data frames to plot:
reptiles_ses_faith_clean_df <- reptiles_ses_faith_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Reptiles") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
reptiles_ses_mpd_clean_df <- reptiles_ses_mpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Reptiles") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
reptiles_ses_mntd_clean_df <- reptiles_ses_mntd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Reptiles") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
reptiles_SES_faith_50km_map <- div.maps.plot(div_per_cell_df = reptiles_ses_faith_clean_df,
                                          div_facet_nm = "PD",
                                          metric_nm = "SES Faith",
                                          grid = grid_50km,
                                          land_mask = land_mask,
                                          plot_title = TRUE,
                                          save = TRUE)
reptiles_SES_MPD_50km_map <- div.maps.plot(div_per_cell_df = reptiles_ses_mpd_clean_df,
                                        div_facet_nm = "PD",
                                        metric_nm = "SES MPD",
                                        grid = grid_50km,
                                        land_mask = land_mask,
                                        plot_title = TRUE,
                                        save = TRUE)
reptiles_SES_MNTD_50km_map <- div.maps.plot(div_per_cell_df = reptiles_ses_mntd_clean_df,
                                         div_facet_nm = "PD",
                                         metric_nm = "SES MNTD",
                                         grid = grid_50km,
                                         land_mask = land_mask,
                                         plot_title = TRUE,
                                         save = TRUE)




# 2 b - Maps SES FD (FRic, FMPD, FOri) =========================================

# Load diversity data:
reptiles_ses_fric_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FRic_null_models_metrics_50km_REPTILES.rds"))
reptiles_ses_fmpd_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FMPD_null_models_metrics_50km_REPTILES.rds"))
reptiles_ses_fori_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FOri_null_models_metrics_50km_REPTILES.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))


# Transform the data frames to plot:
reptiles_ses_fric_clean_df <- reptiles_ses_fric_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Reptiles") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
reptiles_ses_fmpd_clean_df <- reptiles_ses_fmpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Reptiles") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
reptiles_ses_fori_clean_df <- reptiles_ses_fori_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Reptiles") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
reptiles_SES_fric_50km_map <- div.maps.plot(div_per_cell_df = reptiles_ses_fric_clean_df,
                                            div_facet_nm = "FD",
                                            metric_nm = "SES FRic",
                                            grid = grid_50km,
                                            land_mask = land_mask,
                                            plot_title = TRUE,
                                            save = TRUE)
reptiles_SES_FMPD_50km_map <- div.maps.plot(div_per_cell_df = reptiles_ses_fmpd_clean_df,
                                            div_facet_nm = "FD",
                                            metric_nm = "SES FMPD",
                                            grid = grid_50km,
                                            land_mask = land_mask,
                                            plot_title = TRUE,
                                            save = TRUE)
reptiles_SES_fori_50km_map <- div.maps.plot(div_per_cell_df = reptiles_ses_fori_clean_df,
                                            div_facet_nm = "FD",
                                            metric_nm = "SES FOri",
                                            grid = grid_50km,
                                            land_mask = land_mask,
                                            plot_title = TRUE,
                                            save = TRUE)


# 4 - MAMMALS #################################################################



# 2 a - Maps SES PD (Faith, MPD, MNTD) =========================================


# Load diversity data:
mammals_ses_faith_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "PD_Faith_null_models_metrics_50km_MAMMALS.rds"))
mammals_ses_mpd_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_MPD_null_models_metrics_50km_MAMMALS.rds"))
mammals_ses_mntd_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "PD_MNTD_null_models_metrics_50km_MAMMALS.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))


# Transform the data frames to plot:
mammals_ses_faith_clean_df <- mammals_ses_faith_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Mammals") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
mammals_ses_mpd_clean_df <- mammals_ses_mpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Mammals") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
mammals_ses_mntd_clean_df <- mammals_ses_mntd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Mammals") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
mammals_SES_faith_50km_map <- div.maps.plot(div_per_cell_df = mammals_ses_faith_clean_df,
                                            div_facet_nm = "PD",
                                            metric_nm = "SES Faith",
                                            grid = grid_50km,
                                            land_mask = land_mask,
                                            plot_title = TRUE,
                                            save = TRUE)
mammals_SES_MPD_50km_map <- div.maps.plot(div_per_cell_df = mammals_ses_mpd_clean_df,
                                          div_facet_nm = "PD",
                                          metric_nm = "SES MPD",
                                          grid = grid_50km,
                                          land_mask = land_mask,
                                          plot_title = TRUE,
                                          save = TRUE)
mammals_SES_MNTD_50km_map <- div.maps.plot(div_per_cell_df = mammals_ses_mntd_clean_df,
                                           div_facet_nm = "PD",
                                           metric_nm = "SES MNTD",
                                           grid = grid_50km,
                                           land_mask = land_mask,
                                           plot_title = TRUE,
                                           save = TRUE)




# 2 b - Maps SES FD (FRic, FMPD, FOri) =========================================

# Load diversity data:
mammals_ses_fric_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "FD_FRic_null_models_metrics_50km_MAMMALS.rds"))
mammals_ses_fmpd_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "FD_FMPD_null_models_metrics_50km_MAMMALS.rds"))
mammals_ses_fori_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "FD_FOri_null_models_metrics_50km_MAMMALS.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))

# Transform the data frames to plot:
mammals_ses_fric_clean_df <- mammals_ses_fric_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Mammals") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
mammals_ses_fmpd_clean_df <- mammals_ses_fmpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Mammals") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
mammals_ses_fori_clean_df <- mammals_ses_fori_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Mammals") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
mammals_SES_fric_50km_map <- div.maps.plot(div_per_cell_df = mammals_ses_fric_clean_df,
                                           div_facet_nm = "FD",
                                           metric_nm = "SES FRic",
                                           grid = grid_50km,
                                           land_mask = land_mask,
                                           plot_title = TRUE,
                                           save = TRUE)
mammals_SES_FMPD_50km_map <- div.maps.plot(div_per_cell_df = mammals_ses_fmpd_clean_df,
                                           div_facet_nm = "FD",
                                           metric_nm = "SES FMPD",
                                           grid = grid_50km,
                                           land_mask = land_mask,
                                           plot_title = TRUE,
                                           save = TRUE)
mammals_SES_fori_50km_map <- div.maps.plot(div_per_cell_df = mammals_ses_fori_clean_df,
                                           div_facet_nm = "FD",
                                           metric_nm = "SES FOri",
                                           grid = grid_50km,
                                           land_mask = land_mask,
                                           plot_title = TRUE,
                                           save = TRUE)

# 5 - BUTTERFLIES #################################################################



# 2 a - Maps SES PD (Faith, MPD, MNTD) =========================================


# Load diversity data:
butterflies_ses_faith_df <- readRDS(here::here("transformed_data",
                                               "div_values_null_models",
                                               "PD_Faith_null_models_metrics_50km_BUTTERFLIES.rds"))
butterflies_ses_mpd_df <- readRDS(here::here("transformed_data",
                                             "div_values_null_models",
                                             "PD_MPD_null_models_metrics_50km_BUTTERFLIES.rds"))
butterflies_ses_mntd_df <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "PD_MNTD_null_models_metrics_50km_BUTTERFLIES.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))


# Transform the data frames to plot:
butterflies_ses_faith_clean_df <- butterflies_ses_faith_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Butterflies") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
butterflies_ses_mpd_clean_df <- butterflies_ses_mpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Butterflies") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
butterflies_ses_mntd_clean_df <- butterflies_ses_mntd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Butterflies") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
butterflies_SES_faith_50km_map <- div.maps.plot(div_per_cell_df = butterflies_ses_faith_clean_df,
                                                div_facet_nm = "PD",
                                                metric_nm = "SES Faith",
                                                grid = grid_50km,
                                                land_mask = land_mask,
                                                plot_title = TRUE,
                                                save = TRUE)
butterflies_SES_MPD_50km_map <- div.maps.plot(div_per_cell_df = butterflies_ses_mpd_clean_df,
                                              div_facet_nm = "PD",
                                              metric_nm = "SES MPD",
                                              grid = grid_50km,
                                              land_mask = land_mask,
                                              plot_title = TRUE,
                                              save = TRUE)
butterflies_SES_MNTD_50km_map <- div.maps.plot(div_per_cell_df = butterflies_ses_mntd_clean_df,
                                               div_facet_nm = "PD",
                                               metric_nm = "SES MNTD",
                                               grid = grid_50km,
                                               land_mask = land_mask,
                                               plot_title = TRUE,
                                               save = TRUE)




# 2 b - Maps SES FD (FRic, FMPD, FOri) =========================================

# Load diversity data:
butterflies_ses_fric_df <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "FD_FRic_null_models_metrics_50km_BUTTERFLIES.rds"))
butterflies_ses_fmpd_df <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "FD_FMPD_null_models_metrics_50km_BUTTERFLIES.rds"))
butterflies_ses_fori_df <- readRDS(here::here("transformed_data",
                                              "div_values_null_models",
                                              "FD_FOri_null_models_metrics_50km_BUTTERFLIES.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))


# Transform the data frames to plot:
butterflies_ses_fric_clean_df <- butterflies_ses_fric_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Butterflies") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
butterflies_ses_fmpd_clean_df <- butterflies_ses_fmpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Butterflies") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
butterflies_ses_fori_clean_df <- butterflies_ses_fori_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Butterflies") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
butterflies_SES_fric_50km_map <- div.maps.plot(div_per_cell_df = butterflies_ses_fric_clean_df,
                                               div_facet_nm = "FD",
                                               metric_nm = "SES FRic",
                                               grid = grid_50km,
                                               land_mask = land_mask,
                                               plot_title = TRUE,
                                               save = TRUE)
butterflies_SES_FMPD_50km_map <- div.maps.plot(div_per_cell_df = butterflies_ses_fmpd_clean_df,
                                               div_facet_nm = "FD",
                                               metric_nm = "SES FMPD",
                                               grid = grid_50km,
                                               land_mask = land_mask,
                                               plot_title = TRUE,
                                               save = TRUE)
butterflies_SES_fori_50km_map <- div.maps.plot(div_per_cell_df = butterflies_ses_fori_clean_df,
                                               div_facet_nm = "FD",
                                               metric_nm = "SES FOri",
                                               grid = grid_50km,
                                               land_mask = land_mask,
                                               plot_title = TRUE,
                                               save = TRUE)

