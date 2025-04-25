################################################################################
##
## Script to impute traits based on taxonomic data - for TREES
##
## Camille Magneville
##
## 09/2024
##
## 4_a_NEW_Impute_traits_TREES.R
##
################################################################################

`%>%` <- magrittr::`%>%`

# 1 - Load data ========================================================

sp_tr_TREES <- readRDS(here::here("transformed_data",
                                  "raw_traits_TREES.rds"))

# See missing values:
# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(sp_tr_TREES, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

# Check traits in the right format:
str(sp_tr_TREES)


# 2 - Retrieve species Genus/Family information =============


# Load WOODIV nomenclature:
woodiv_nom <- read.csv(here::here("integradiv_db",
                                  "WOODIV_Nomenclature.csv"))

# Get Genus names in a new column:

sp_tr_TREES_df <- sp_tr_TREES %>%
  dplyr::mutate(dplyr::across("Species", stringr::str_replace, ' ', '_')) %>%
  dplyr::mutate("genus" = sub("_.*", "", Species)) %>%
  tibble::column_to_rownames("Species")
sp_tr_TREES_df$genus <- as.factor(sp_tr_TREES_df$genus)

# Link with WOODIV data based on Genus (+ same format as other taxa = output taxize pkge):
family_order_completed_nms <- sp_tr_TREES_df %>%
  tibble::rownames_to_column(var = "species") %>%
  dplyr::left_join(woodiv_nom[, c("order",
                                  "family",
                                  "genus")],
                   by = "genus") %>%
  dplyr::select(c("species", "genus", "family", "order")) %>%
  dplyr::rename(query = "species") %>%
  dplyr::mutate(db = "woodiv") %>%
  dplyr::select("db", "query", "order", "family", "genus") %>%
  dplyr::distinct()


# Save it:
saveRDS(family_order_completed_nms, file = here::here("transformed_data",
                                                      "taxo_info_TREES.rds"))

## New format traits df (and remove Genus):
sp_tr_TREES_df <- sp_tr_TREES_df %>%
  tibble::rownames_to_column(var = "Species") %>%
  dplyr::mutate(dplyr::across("Species", stringr::str_replace, ' ', '_')) %>%
  tibble::column_to_rownames("Species") %>%
  dplyr::select(-c("genus")) %>%
  dplyr::distinct()

# Get species names without "_":
species_nm <- rownames(sp_tr_TREES_df)
species_nm_corrected <- stringr::str_replace(species_nm, '_', ' ')




# 3 - Complete based on the closest taxonomic level which has information ======


# Note: Do the mean for quantitative traits and the mode for qualitative traits

imputed_sp_tr_TREES_df <- impute.missing.traits(sp_tr_NA_df = sp_tr_TREES_df,
                                                      taxo_df = family_order_completed_nms,
                                                      traits_NA_nms = c("BloomBreadth",
                                                                        "LA",
                                                                        "SeedMass",
                                                                        "SLA",
                                                                        "StemSpecDens"))

# Save this final version of traits:
saveRDS(imputed_sp_tr_TREES_df, file = here::here("transformed_data",
                                                        "final_traits_TREES.rds"))

# Still miss:
# 4 species for StemSpecDens: Chamaerops_humilis, Liquidambar_orientalis,
# ... Phoenix_theophrasti, Staphylea pinnata
# 1 species for SLA: Liquidambar_orientalis
# Will remove them while doing the analysis



