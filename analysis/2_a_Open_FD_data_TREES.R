################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for TREES
##
## Camille Magneville
##
## 08/04/2024 - 09/2024
##
## 3_a_Open_FD_data_TREES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_trees_occ_df <- readRDS(here::here("transformed_data",
                                              "sp_asb_50km_TREES.rds"))

# Load traits data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v4.csv"))

# Only keep TREE data:
trees_traits <- dplyr::filter(INTEGRADIV_traits,
                              Taxon == "Trees")


# 2 - Load and clean traits data ================================


# Check if all species in the occurrence df are in the traits db: YES :)
setdiff(colnames(INTEGRADIV_trees_occ_df),
        unique(trees_traits$Species))

# Check traits db only contains species in the occurrence df: NO :/
setdiff(unique(trees_traits$Species),
        colnames(INTEGRADIV_trees_occ_df))

# Remove the 1 species present in the traits db but not in our occ data:
trees_traits_corrected <- dplyr::filter(trees_traits,
                                        ! Species %in% c( "Pyrus syriaca"))

# Check again: Ok :)
setdiff(unique(trees_traits_corrected$Species),
        colnames(INTEGRADIV_trees_occ_df))


# Put it in the right format (species = rows, traits = columns)
trees_traits_df <- trees_traits_corrected %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)

# Keep only the 7 traits needed:
trees_traits_df <- trees_traits_df %>%
  dplyr::select(c("Species",
                  "BloomBreadth",
                  "DispMode",
                  "HeightMax",
                  "LA",
                  "SLA",
                  "SeedMass",
                  "StemSpecDens"))


# 3 - Check extreme traits values ==============================================


# We have spotted one error for birds:
# ... the idea is then to check all extreme values for each trait as they
# ... can profoundly impact the shape of the functional space
# ... by affecting the range by which Gower distance is standardised.

# For each trait, get the identity of species bearing traits > 99% quantile
# ... and < 1% quantile:

extreme_to_check_TREES <- check.quantiles(traits_df = trees_traits_df,
                                          quant_traits_vect = c("BloomBreadth",
                                                                "HeightMax",
                                                                "LA",
                                                                "SLA",
                                                                "SeedMass",
                                                                "StemSpecDens"))

# Save:
write.csv(extreme_to_check_TREES, file = here::here("transformed_data",
                                                    "extreme_check_TREES.csv"))

# Correct extreme traits values based on expert knowledge and litterature...
# ... or NA if value is not right but no other options:
trees_traits_corrected_df <- trees_traits_df
# 1 - Arbustus andrachne: LA value 10 times too high - NA:
trees_traits_corrected_df$LA[which(trees_traits_corrected_df$Species == "Arbutus andrachne")] <- NA


# 4 - Format and save ==========================================================



# Traits with the right format:
trees_traits_corrected_df$LA <- as.numeric(trees_traits_corrected_df$LA)
trees_traits_corrected_df$SeedMass <- as.numeric(trees_traits_corrected_df$SeedMass)
trees_traits_corrected_df$SLA <- as.numeric(trees_traits_corrected_df$SLA)
trees_traits_corrected_df$StemSpecDens <- as.numeric(trees_traits_corrected_df$StemSpecDens)
trees_traits_corrected_df$HeightMax <- as.numeric(trees_traits_corrected_df$HeightMax)
trees_traits_corrected_df$BloomBreadth <- as.numeric(trees_traits_corrected_df$BloomBreadth)
trees_traits_corrected_df$DispMode <- as.factor(trees_traits_corrected_df$DispMode)

# Save the traits:
saveRDS(trees_traits_corrected_df, file = here::here("transformed_data",
                                           "raw_traits_TREES.rds"))


# 5 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(trees_traits_corrected_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)






