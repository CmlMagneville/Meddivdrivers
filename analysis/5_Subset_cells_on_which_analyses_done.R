################################################################################
##
## Script to restreint cells to those with at least 50% land + no islands +
## ... where drivers data available + where more than 4 species for all taxa
##
## Camille Magneville
##
## 11/2024
##
## 5_Subset_cells_on_which_analyses_done.R
##
################################################################################



# 1 - Load data =================================================================


## Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data",
                                      "env_db",
                                      "env_drivers_final_db.rds"))

## Load species occurrences:
birds_occ_df <- readRDS(here::here("transformed_data",
                                   "sp_asb_50km_BIRDS.rds"))
trees_occ_df <- readRDS(here::here("transformed_data",
                                   "sp_asb_50km_TREES.rds"))
reptiles_occ_df <- readRDS(here::here("transformed_data",
                                      "sp_asb_50km_REPTILES.rds"))
mammals_occ_df <- readRDS(here::here("transformed_data",
                                     "sp_asb_50km_MAMMALS.rds"))
butterflies_occ_df <- readRDS(here::here("transformed_data",
                                         "sp_asb_50km_BUTTERFLIES.rds"))

## Load land area:
land_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/land_area.rds")

## Load grids (to visualise):
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)



# 2 - Get the names of grid cells for which no island + 50% land ===============


# Keep only 50*50 data:
land_db <- land_db %>%
  dplyr::filter(Scale == "50")

# Only keep cells which have 50% or more of land in their area:
# (0.5*(2.5*10e9m2))
land_50_cells_nm <- land_db$Idgrid[which((land_db$Variable_code == "SurfCont") &
                                          (land_db$Value >= max(land_db$Value)/2))]

# Only keep cells which are not islands:
no_islands_cells_nm <- land_db$Idgrid[which((land_db$Variable_code == "SurfIsland") &
                                              (land_db$Value =! 0))]

# Keep the cells that are in both vectors:
land50_noislands_cells_nm <- intersect(land_50_cells_nm,
                                       no_islands_cells_nm)


# 3 - Get the names of cells which gather more than 4 species for all taxa =====


# Get the name of the cells for each taxa:
asb_5_BIRDS <- names(which(rowSums(birds_occ_df) > 4))
asb_5_TREES <- names(which(rowSums(trees_occ_df) > 4))
asb_5_REPTILES <- names(which(rowSums(reptiles_occ_df) > 4))
asb_5_MAMMALS <- names(which(rowSums(mammals_occ_df) > 4))
asb_5_BUTTERFLIES <- names(which(rowSums(butterflies_occ_df) > 4))

# Get the intersection:
more_4_sp_cells_nm <- intersect(intersect(intersect(intersect(asb_5_BIRDS, asb_5_BUTTERFLIES),
                                asb_5_TREES),
                                asb_5_MAMMALS),
                                asb_5_REPTILES)

# 4 - Get the names of cells where drivers ok ==================================


# Check columns with NA:
names(which(colSums(is.na(envdriv_full_db)) > 0))

# Check names of cells with NA:
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_CCVelLGM_mean.voccMag) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_CCVelHolocene_mean.voccMag) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_CCVelShortTerm_mean.voccMag) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_CCVelYoungerDryas_mean.voccMag) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)

envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_TAP_sd) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_MAT_sd) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_MAT_mean) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_MAT_mean) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_MAT_stdev) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_TAP_stdev) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)

# Manuel's work:
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_AI_mean) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_AI_stdev) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)


# Only one cell missing values for all drivers:
drivers_NA_cell_nm <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_AI_mean) == TRUE)]
drivers_no_NA_cells_nm <- envdriv_full_db$Idgrid[which(envdriv_full_db$Idgrid != drivers_NA_cell_nm)]


# 5 - Get the final name of cells on which analyse can be done ==================


final_cells_to_keep <- intersect(intersect(land50_noislands_cells_nm,
                                 more_4_sp_cells_nm),
                                 drivers_no_NA_cells_nm)

# Work with 411 cells: where are they:
locate.cells(cell_vect = final_cells_to_keep,
             grid = grid_50km)




# 6 - Restrict the drivers db to these cells and format ========================



restricted_envdriv_full_db <- envdriv_full_db[which(envdriv_full_db$Idgrid %in% final_cells_to_keep), ]

str(restricted_envdriv_full_db)

# Change Populations to numeric:
restricted_envdriv_full_db$Pr_Pop_2020_mean <- as.numeric(restricted_envdriv_full_db$Pr_Pop_2020_mean)
restricted_envdriv_full_db$Pr_RatePop_2020_mean <- as.numeric(restricted_envdriv_full_db$Pr_RatePop_2020_mean)

# Change Fire to numeric:
restricted_envdriv_full_db$Pr_FInt_2000_2023_mean <- as.numeric(restricted_envdriv_full_db$Pr_FInt_2000_2023_mean)
restricted_envdriv_full_db$Pr_FInt_2000_2023_sd <- as.numeric(restricted_envdriv_full_db$Pr_FInt_2000_2023_sd)
restricted_envdriv_full_db$Pr_FSurf_2000_2023_pixels <- as.numeric(restricted_envdriv_full_db$Pr_FSurf_2000_2023_pixels)

# Change Herbivore richness to integer:
restricted_envdriv_full_db$HerbRichn_sum <- as.integer(restricted_envdriv_full_db$HerbRichn_sum)

# Change soil and topo to numeric:
restricted_envdriv_full_db$Depth_mean <- as.numeric(restricted_envdriv_full_db$Depth_mean)
restricted_envdriv_full_db$Depth_stdev <- as.numeric(restricted_envdriv_full_db$Depth_stdev)
restricted_envdriv_full_db$Elv_mean <- as.numeric(restricted_envdriv_full_db$Elv_mean)
restricted_envdriv_full_db$Elv_stdev <- as.numeric(restricted_envdriv_full_db$Elv_stdev)
restricted_envdriv_full_db$Pr_FCon_percentage_percentage <- as.numeric(restricted_envdriv_full_db$Pr_FCon_percentage_percentage)

## Save this final environmental db:
saveRDS(restricted_envdriv_full_db,
        here::here("transformed_data", "env_db",
                   "env_drivers_final_restricted_db.rds"))


# 7 - Restrict species occurrences data to these cells + rm species present in rm cells ========


restricted_birds_occ_df <- birds_occ_df[which(rownames(birds_occ_df) %in% final_cells_to_keep), ]
restricted_mammals_occ_df <- mammals_occ_df[which(rownames(mammals_occ_df) %in% final_cells_to_keep), ]
restricted_trees_occ_df <- trees_occ_df[which(rownames(trees_occ_df) %in% final_cells_to_keep), ]
restricted_butterflies_occ_df <- butterflies_occ_df[which(rownames(butterflies_occ_df) %in% final_cells_to_keep), ]
restricted_reptiles_occ_df <- reptiles_occ_df[which(rownames(reptiles_occ_df) %in% final_cells_to_keep), ]


# Check that all species are present in at least one cell:
# Get a list of species present in 0 cells (as removed islands and -50% land)
sp_to_rm_birds <- colnames(restricted_birds_occ_df)[colSums(restricted_birds_occ_df) == 0]
sp_to_rm_trees <- colnames(restricted_trees_occ_df)[colSums(restricted_trees_occ_df) == 0]
sp_to_rm_mammals <- colnames(restricted_mammals_occ_df)[colSums(restricted_mammals_occ_df) == 0]
sp_to_rm_butterflies <- colnames(restricted_butterflies_occ_df)[colSums(restricted_butterflies_occ_df) == 0]
sp_to_rm_reptiles <- colnames(restricted_reptiles_occ_df)[colSums(restricted_reptiles_occ_df) == 0]

# Remove them from the occurrences data frame:
restricted_birds_occ_df <- restricted_birds_occ_df[, which(! colnames(restricted_birds_occ_df) %in% sp_to_rm_birds)]
restricted_mammals_occ_df <- restricted_mammals_occ_df[, which(! colnames(restricted_mammals_occ_df) %in% sp_to_rm_mammals)]
restricted_reptiles_occ_df <- restricted_reptiles_occ_df[, which(! colnames(restricted_reptiles_occ_df) %in% sp_to_rm_reptiles)]
restricted_butterflies_occ_df <- restricted_butterflies_occ_df[, which(! colnames(restricted_butterflies_occ_df) %in% sp_to_rm_butterflies)]
restricted_trees_occ_df <- restricted_trees_occ_df[, which(! colnames(restricted_trees_occ_df) %in% sp_to_rm_trees)]

saveRDS(restricted_birds_occ_df,
        here::here("transformed_data",
                   "sp_asb_50km_restricted_BIRDS.rds"))
saveRDS(restricted_trees_occ_df,
        here::here("transformed_data",
                   "sp_asb_50km_restricted_TREES.rds"))
saveRDS(restricted_butterflies_occ_df,
        here::here("transformed_data",
                   "sp_asb_50km_restricted_BUTTERFLIES.rds"))
saveRDS(restricted_reptiles_occ_df,
        here::here("transformed_data",
                   "sp_asb_50km_restricted_REPTILES.rds"))
saveRDS(restricted_mammals_occ_df,
        here::here("transformed_data",
                   "sp_asb_50km_restricted_MAMMALS.rds"))


# 8 - Correct the species*traits df by removing species not present in any of the asb ====


# Load species-traits data:
sp_tr_TREES <- readRDS(here::here("transformed_data",
                                  "final_traits_TREES.rds"))
sp_tr_BIRDS <- readRDS(here::here("transformed_data",
                                  "final_traits_BIRDS.rds"))
sp_tr_MAMMALS <- readRDS(here::here("transformed_data",
                                  "final_traits_MAMMALS.rds"))
sp_tr_REPTILES <- readRDS(here::here("transformed_data",
                                  "final_traits_REPTILES.rds"))
sp_tr_BUTTERFLIES <- readRDS(here::here("transformed_data",
                                  "final_traits_BUTTERFLIES.rds"))

# Restrict them:
sp_tr_restricted_TREES <- sp_tr_TREES[which(! rownames(sp_tr_TREES) %in% sp_to_rm_trees), ]
sp_tr_restricted_BIRDS <- sp_tr_BIRDS[which(! rownames(sp_tr_BIRDS) %in% sp_to_rm_birds), ]
sp_tr_restricted_REPTILES <- sp_tr_REPTILES[which(! rownames(sp_tr_REPTILES) %in% sp_to_rm_reptiles), ]
sp_tr_restricted_MAMMALS <- sp_tr_MAMMALS[which(! rownames(sp_tr_MAMMALS) %in% sp_to_rm_mammals), ]
sp_tr_restricted_BUTTERFLIES <- sp_tr_BUTTERFLIES[which(! rownames(sp_tr_BUTTERFLIES) %in% sp_to_rm_butterflies), ]


# Save new:
saveRDS(sp_tr_restricted_BIRDS,
        here::here("transformed_data",
                   "final_traits_restricted_BIRDS.rds"))
saveRDS(sp_tr_restricted_TREES,
        here::here("transformed_data",
                   "final_traits_restricted_TREES.rds"))
saveRDS(sp_tr_restricted_MAMMALS,
        here::here("transformed_data",
                   "final_traits_restricted_MAMMALS.rds"))
saveRDS(sp_tr_restricted_REPTILES,
        here::here("transformed_data",
                   "final_traits_restricted_REPTILES.rds"))
saveRDS(sp_tr_restricted_BUTTERFLIES,
        here::here("transformed_data",
                   "final_traits_restricted_BUTTERFLIES.rds"))

# 9 - Correct phylogenies ======================================================


# Load data:
trees_phylogeny <- ape::read.tree(file = here::here("transformed_data",
                                                    "phylogeny_TREES.tree"))
birds_phylogeny <- ape::read.tree(file = here::here("transformed_data",
                                                    "phylogeny_BIRDS.tree"))
mammals_phylogeny <- ape::read.tree(file = here::here("transformed_data",
                                                    "phylogeny_MAMMALS.tree"))
reptiles_phylogeny <- ape::read.tree(file = here::here("transformed_data",
                                                    "phylogeny_REPTILES.tree"))
butterflies_phylogeny <- ape::read.tree(file = here::here("transformed_data",
                                                    "phylogeny_BUTTERFLIES.tree"))

# Restrict:
trees_phylogeny_restricted <- ape::drop.tip(trees_phylogeny,
                                           sp_to_rm_trees,
                                           trim.internal = TRUE)
birds_phylogeny_restricted <- ape::drop.tip(birds_phylogeny,
                                            sp_to_rm_birds,
                                            trim.internal = TRUE)
mammals_phylogeny_restricted <- ape::drop.tip(mammals_phylogeny,
                                            sp_to_rm_mammals,
                                            trim.internal = TRUE)
reptiles_phylogeny_restricted <- ape::drop.tip(reptiles_phylogeny,
                                            sp_to_rm_reptiles,
                                            trim.internal = TRUE)
butterflies_phylogeny_restricted <- ape::drop.tip(butterflies_phylogeny,
                                            sp_to_rm_butterflies,
                                            trim.internal = TRUE)

# Save:
ape::write.tree(trees_phylogeny_restricted,
                file = here::here("transformed_data",
                                  "phylogeny_restricted_TREES.tree"))
ape::write.tree(birds_phylogeny_restricted,
                file = here::here("transformed_data",
                                  "phylogeny_restricted_BIRDS.tree"))
ape::write.tree(mammals_phylogeny_restricted,
                file = here::here("transformed_data",
                                  "phylogeny_restricted_MAMMALS.tree"))
ape::write.tree(reptiles_phylogeny_restricted,
                file = here::here("transformed_data",
                                  "phylogeny_restricted_REPTILES.tree"))
ape::write.tree(butterflies_phylogeny_restricted,
                file = here::here("transformed_data",
                                  "phylogeny_restricted_BUTTERFLIES.tree"))
