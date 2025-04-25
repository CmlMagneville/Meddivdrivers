################################################################################
##
## Script open the phylogeny from the integradiv database and check for
## ... potential issues - for BUTTERFLIES
##
## Camille Magneville
##
## 09/2024
##
## 1_a_Open_PD_data_BUTTERFLIES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_occ_db <- read.csv(here::here("integradiv_db",
                                         "INTEGRADIV_occurrences_v4.csv"))

# Only keep butterflies data:
INTEGRADIV_butterflies_occ_df <- dplyr::filter(INTEGRADIV_occ_db,
                                           Taxon == "Butterflies")

# Number of species: 233
length(unique(INTEGRADIV_butterflies_occ_df$Species))

# Only keep 50x50 assemblages ie cells:
temp_butterflies_sp_asb_50km <- dplyr::filter(INTEGRADIV_butterflies_occ_df,
                                          Grid == "50x50")

# Pivot wider so asb are columns:
butterflies_sp_asb_50km <- temp_butterflies_sp_asb_50km %>%
  # Add a new column with 1 values (used for pivoting after):
  dplyr::mutate(Value = 1) %>%
  # Only keep interesting columns: species name, asb data and value:
  dplyr::select("Species", "Idgrid", "Value") %>%
  # Remove similar rows (same species and cells name because several info sources):
  dplyr::distinct() %>%
  # Pivot wider:
  tidyr::pivot_wider(names_from = "Idgrid",
                     values_from = "Value") %>%
  # Replace NAs by 0:
  replace(is.na(.), 0) %>%
  # Species names with space instead of "_":
  dplyr::mutate(dplyr::across("Species", stringr::str_replace, '_', ' ')) %>%
  # Species as rownames:
  tibble::column_to_rownames(var = "Species") %>%
  # Transpose the df because I want asb as rows and sp as columns:
  t()


# Save it:
saveRDS(butterflies_sp_asb_50km,
        here::here("transformed_data",
                   "sp_asb_50km_BUTTERFLIES.rds"))


# 2 - Load and clean phylogenetic data ================================


# Load phylogeny data:
INTEGRADIV_phylogenies <- readRDS(here::here("integradiv_db",
                                             "INTEGRADIV_phylogenies_v3.rds"))

# Keep only the butterflies phylogeny:
butterflies_phylogeny <- INTEGRADIV_phylogenies$Butterflies
plot(butterflies_phylogeny)

# Remove "_" between genus and species and add " " (same tr and occ data):
butterflies_phylogeny$tip.label <- gsub("_", " ", butterflies_phylogeny$tip.label)


# Check if all species in the occurrence df are in the phylogeny: YES :)
setdiff(unique(INTEGRADIV_butterflies_occ_df$Species),
        butterflies_phylogeny$tip.label)

# Check phylogeny only contains species in the occurrence df: YES :)
setdiff(butterflies_phylogeny$tip.label,
        unique(INTEGRADIV_butterflies_occ_df$Species))


# Save the butterflies phylogeny:
ape::write.tree(butterflies_phylogeny,
                file = here::here("transformed_data",
                                  "phylogeny_BUTTERFLIES.tree"))

