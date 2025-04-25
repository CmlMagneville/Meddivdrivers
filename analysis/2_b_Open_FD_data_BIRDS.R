################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for BIRDS
##
## Camille Magneville
##
## 08/04/2024 - 09/2024
##
## 3_a_Open_FD_data_BIRDS.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_birds_occ_df <- readRDS(here::here("transformed_data",
                                              "sp_asb_50km_BIRDS.rds"))


# 2 - Load and clean traits data ================================


# Load  traits data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v4.csv"))

# Only keep BIRDS data:
birds_traits <- dplyr::filter(INTEGRADIV_traits,
                              Taxon == "Birds")

# Check if all species in the occurrence df are in the traits db: YES :)
setdiff(colnames(INTEGRADIV_birds_occ_df),
        unique(birds_traits$Species))

# Check traits db only contains species in the occurrence df: YES :)
setdiff(unique(birds_traits$Species),
        colnames(INTEGRADIV_birds_occ_df))


# Put it in the right format (species = rows, traits = columns)
birds_traits_df <- birds_traits %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)


# Select the traits that will be used:
birds_traits_df <- dplyr::select(birds_traits_df,
                                 c("Species",
                                   "BeakLengthCulmen",
                                   "BeakRatio",
                                   "BodyMass",
                                   "GenerationLength",
                                   "HandWingIndex",
                                   "Migration",
                                   "OffspringPerRepro",
                                   "ReproPerYear",
                                   "TarsusLength"))

# Based on the AMNIOTE database, complete some missing values:
birds_traits_df$OffspringPerRepro[which(birds_traits_df$Species == "Iduna opaca")] <- 3.3
birds_traits_df$ReproPerYear[which(birds_traits_df$Species == "Iduna opaca")] <- 2


# 3 - Check extreme traits values ==============================================


# We have spotted one error for OffspringPerRepro for Clamator glandarius:
# ... the idea is then to check all extreme values for each trait as they
# ... can profoundly impact the shape of the functional space
# ... by affecting the range by which Gower distance is standardised.

# For each trait, get the identity of species bearing traits > 99% quantile
# ... and < 1% quantile:

extreme_to_check_BIRDS <- check.quantiles(traits_df = birds_traits_df,
                                          quant_traits_vect = c("BeakLengthCulmen", "BeakRatio",
                                                                "BodyMass",
                                                                "GenerationLength", "HandWingIndex",
                                                                "OffspringPerRepro",
                                                                "ReproPerYear","TarsusLength"))

# Save:
write.csv(extreme_to_check_BIRDS, file = here::here("transformed_data",
                                                  "extreme_check_BIRDS.csv"))


# Correct extreme values based on expert knowledge + sources:
birds_traits_corrected_df <- birds_traits_df
# Bade on the AMNIOTE db (original value - 18 probably confused with Offspring per year)
birds_traits_corrected_df$OffspringPerRepro[which(birds_traits_corrected_df$Species == "Clamator glandarius")] <- 4.8


# 4 - Format and save ==========================================================


# Create a new traits: Offspring per year:
birds_traits_corrected_df <- birds_traits_corrected_df %>%
  dplyr::mutate("OffspringPerYear" = as.numeric(OffspringPerRepro)*as.numeric(ReproPerYear)) %>%
  dplyr::select(-c("OffspringPerRepro",
                   "ReproPerYear"))


# Format the traits:
birds_traits_corrected_df$BodyMass <- as.numeric(birds_traits_corrected_df$BodyMass)
birds_traits_corrected_df$HandWingIndex <- as.numeric(birds_traits_corrected_df$HandWingIndex)
birds_traits_corrected_df$Migration <- ordered(birds_traits_corrected_df$Migration,
                                     levels = c("1_sedentary", "2_intermediate",
                                                "3_migratory"))
birds_traits_corrected_df$TarsusLength <- as.numeric(birds_traits_corrected_df$TarsusLength)
birds_traits_corrected_df$OffspringPerYear <- as.numeric(birds_traits_corrected_df$OffspringPerYear)
birds_traits_corrected_df$GenerationLength <- round(as.numeric(birds_traits_corrected_df$GenerationLength), 2)
birds_traits_corrected_df$BeakRatio <- as.numeric(birds_traits_corrected_df$BeakRatio)
birds_traits_corrected_df$BeakLengthCulmen <- as.numeric(birds_traits_corrected_df$BeakLengthCulmen)

# Save the traits:
saveRDS(birds_traits_corrected_df, file = here::here("transformed_data",
                                           "raw_traits_BIRDS.rds"))

# 4 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(birds_traits_corrected_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)






