################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for REPTILES
##
## Camille Magneville
##
## 08/04/2024 - 09/2024
##
## 3_a_Open_FD_data_REPTILES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_reptiles_occ_df <- readRDS(here::here("transformed_data",
                                              "sp_asb_50km_REPTILES.rds"))


# 2 - Load and clean traits data ================================


# Load traits data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v4.csv"))

# Only keep TREE data:
reptiles_traits <- dplyr::filter(INTEGRADIV_traits,
                              Taxon == "Reptiles")

# Check if all species in the occurrence df are in the traits db: YES :)
setdiff(colnames(INTEGRADIV_reptiles_occ_df),
        unique(reptiles_traits$Species))

# Check traits db only contains species in the occurrence df: YES :)
setdiff(unique(reptiles_traits$Species),
        colnames(INTEGRADIV_reptiles_occ_df))


# Remove fuzzy traits:
reptiles_no_fuzzy_traits <- reptiles_traits %>%
  dplyr::filter(Trait != "Substrate")

# Put dataframes in the right format (species = rows, traits = columns)
reptiles_no_fuzzy_traits_df <- reptiles_no_fuzzy_traits %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)

# Only keep interesting traits:
reptiles_no_fuzzy_traits_df <- reptiles_no_fuzzy_traits_df %>%
  dplyr::select(c("Species",
                  "ActivityTime",
                  "LimbDev",
                  "LongevityMax",
                  "OffspringPerRepro",
                  "ReproPerYear",
                  "SVLMax",
                  "TrophicLevel"))




# 3 - Check extreme traits values ==============================================


# We have spotted one error for birds:
# ... the idea is then to check all extreme values for each trait as they
# ... can profoundly impact the shape of the functional space
# ... by affecting the range by which Gower distance is standardised.

# For each trait, get the identity of species bearing traits > 99% quantile
# ... and < 1% quantile:

extreme_to_check_REPTILES <- check.quantiles(traits_df = reptiles_no_fuzzy_traits_df,
                                             quant_traits_vect = c("LongevityMax",
                                                                   "OffspringPerRepro",
                                                                   "ReproPerYear",
                                                                   "SVLMax"))

# Save:
write.csv(extreme_to_check_REPTILES, file = here::here("transformed_data",
                                                       "extreme_check_REPTILES.csv"))

# Correct based on expert knowledge + sources:
reptiles_no_fuzzy_traits_corrected_df <- reptiles_no_fuzzy_traits_df
# Based on the Spanish Iberic Vertebrate Manual: original value was 7.5:
reptiles_no_fuzzy_traits_corrected_df$ReproPerYear[which(reptiles_no_fuzzy_traits_corrected_df$Species == "Tarentola mauritanica")] <- 3



# 4 - Format and save ==========================================================


# Create a new traits: Offspring per year:
reptiles_no_fuzzy_traits_corrected_df <- reptiles_no_fuzzy_traits_corrected_df %>%
  dplyr::mutate("OffspringPerYear" = as.numeric(OffspringPerRepro)*as.numeric(ReproPerYear)) %>%
  dplyr::select(-c("OffspringPerRepro",
                   "ReproPerYear"))

# Format the right class for the traits - non fuzzy:
reptiles_no_fuzzy_traits_corrected_df$ActivityTime <- ordered(reptiles_no_fuzzy_traits_corrected_df$ActivityTime,
                                                    levels = c("1_nocturnal",
                                                               "2_intermediate",
                                                               "3_diurnal"))
reptiles_no_fuzzy_traits_corrected_df$SVLMax <- as.numeric(reptiles_no_fuzzy_traits_corrected_df$SVLMax)
reptiles_no_fuzzy_traits_corrected_df$OffspringPerYear <- round(as.numeric(reptiles_no_fuzzy_traits_corrected_df$OffspringPerYear), 2)
reptiles_no_fuzzy_traits_corrected_df$TrophicLevel <- ordered(reptiles_no_fuzzy_traits_corrected_df$TrophicLevel,
                                                    levels = c("1_herbivorous",
                                                               "2_omnivorous",
                                                               "3_carnivorous"))
reptiles_no_fuzzy_traits_corrected_df$LimbDev <- ordered(reptiles_no_fuzzy_traits_corrected_df$LimbDev,
                                               levels = c("1_limbless",
                                                          "2_reduced",
                                                          "3_developed"))
reptiles_no_fuzzy_traits_corrected_df$LongevityMax <- as.numeric(reptiles_no_fuzzy_traits_corrected_df$LongevityMax)


# Save the traits:
saveRDS(reptiles_no_fuzzy_traits_corrected_df, file = here::here("transformed_data",
                                              "raw_traits_REPTILES.rds"))

# 3 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(reptiles_no_fuzzy_traits_corrected_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

