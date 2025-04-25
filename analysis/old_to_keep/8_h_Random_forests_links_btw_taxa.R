################################################################################
##
## Script to compute one random forest for each taxa with all drivers + other
## taxa diversity
##
## Camille Magneville
##
## 07/06/2024
##
## 8_e_Random_forests_links_btw_taxa.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ================================================================


# Load environmental drivers (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_noNA_db.rds"))

# Load SES PD - mntd:
mntd_ses_birds_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_MNTD_null_models_metrics_50km_BIRDS.rds"))
mntd_ses_trees_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_MNTD_null_models_metrics_50km_TREES.rds"))
mntd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "PD_MNTD_null_models_metrics_50km_REPTILES.rds"))

# Load SES PD - Faith:
faith_ses_birds_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_BIRDS.rds"))
faith_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_TREES.rds"))
faith_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                            "PD_Faith_null_models_metrics_50km_REPTILES.rds"))

# Load SES PD - mpd:
mpd_ses_birds_df <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_MPD_null_models_metrics_50km_BIRDS.rds"))
mpd_ses_trees_df <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_MPD_null_models_metrics_50km_TREES.rds"))
mpd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "PD_MPD_null_models_metrics_50km_REPTILES.rds"))


# Load grid data(for locating grid cells):
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)



# 2 - Random forests for PD - Faith ============================================


# 2 - a - Subset diversity db and link the databases (div taxa + env) ==========


# Get the names of the Idgrid to keep (diversity data for all taxa):
cells_ok_birds <- unique(faith_ses_birds_df$Idgrid)
cells_ok_reptiles <- unique(faith_ses_reptiles_df$Idgrid)
cells_ok_trees <- unique(faith_ses_trees_df$Idgrid)
cells_to_keep <- intersect(intersect(cells_ok_birds,
                                     cells_ok_reptiles),
                           cells_ok_trees)
locate.cells(cell_vect = cells_to_keep,
             grid = grid_50km)

# Only keep these cells in the diversity df:
faith_ses_birds_df <- faith_ses_birds_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)
faith_ses_reptiles_df <- faith_ses_reptiles_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)
faith_ses_trees_df <- faith_ses_trees_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)

# Rename the last ses columns of these db so can join them and keep ses values ok:
faith_ses_birds_df <- dplyr::rename(faith_ses_birds_df,
                                    "ses_birds" = "ses")
faith_ses_trees_df <- dplyr::rename(faith_ses_trees_df,
                                    "ses_trees" = "ses")
faith_ses_reptiles_df <- dplyr::rename(faith_ses_reptiles_df,
                                    "ses_reptiles" = "ses")

# Create one table for each taxa (env + div other taxa):
rf_faith_birds_df <- envdriv_full_db %>%
  dplyr::left_join(faith_ses_birds_df[, c("Idgrid", "ses_birds")],
                   by = "Idgrid") %>%
  dplyr::left_join(faith_ses_trees_df[, c("Idgrid", "ses_trees")],
                   by = "Idgrid") %>%
  dplyr::left_join(faith_ses_reptiles_df[, c("Idgrid", "ses_reptiles")],
                   by = "Idgrid")

rf_faith_reptiles_df <- envdriv_full_db %>%
  dplyr::left_join(faith_ses_reptiles_df[, c("Idgrid", "ses_reptiles")],
                   by = "Idgrid") %>%
  dplyr::left_join(faith_ses_trees_df[, c("Idgrid", "ses_trees")],
                   by = "Idgrid") %>%
  dplyr::left_join(faith_ses_birds_df[, c("Idgrid", "ses_birds")],
                   by = "Idgrid")

rf_faith_trees_df <- envdriv_full_db %>%
  dplyr::left_join(faith_ses_trees_df[, c("Idgrid", "ses_trees")],
                   by = "Idgrid") %>%
  dplyr::left_join(faith_ses_birds_df[, c("Idgrid", "ses_birds")],
                   by = "Idgrid") %>%
  dplyr::left_join(faith_ses_reptiles_df[, c("Idgrid", "ses_reptiles")],
                   by = "Idgrid")

# Put Idgrid as rownames:
rf_faith_birds_df <- rf_faith_birds_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_faith_trees_df <- rf_faith_trees_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_faith_reptiles_df <- rf_faith_reptiles_df %>%
  tibble::column_to_rownames(var = "Idgrid")


# 2 - b - BIRDS ================================================================


# Check types of variables
str(rf_faith_birds_df)

# Check that diversity metric doesn't have NA:
rownames(rf_faith_birds_df[which(is.na(rf_faith_birds_df$ses) == TRUE), ])

# Change SES from names num to num:
rf_faith_birds_df$ses_birds <- as.numeric(rf_faith_birds_df$ses_birds)
rf_faith_birds_df$ses_trees <- as.numeric(rf_faith_birds_df$ses_trees)
rf_faith_birds_df$ses_reptiles <- as.numeric(rf_faith_birds_df$ses_reptiles)

# Rename ses_birds -> ses so the rf function works and place it as last column:
rf_faith_birds_df <- rf_faith_birds_df %>%
  dplyr::rename("ses" = "ses_birds") %>%
  dplyr::relocate(ses, .after = dplyr::last_col())



# Set seed for randomisation:
set.seed(42)

# Compute 100 random forests and mean importance of each variable
# (500 trees and mtry = 16):
# % Var explained around 49%
varimp_birds <- test.rf.model(rf_data = rf_faith_birds_df,
                              iteration_nb = 100)
# Save it:
saveRDS(varimp_birds, here::here("transformed_data",
                                 "rf_birds_PD_Faith_50_withothertaxa.rds"))


# 2 - c - REPTILES =============================================================


# 2 - d - TREES ================================================================

