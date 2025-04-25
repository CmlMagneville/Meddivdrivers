################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for BUTTERFLIES
##
## Camille Magneville
##
## 09/2024
##
## 3_a_Open_FD_data_BUTTERFLIES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_butterflies_occ_df <- readRDS(here::here("transformed_data",
                                                "sp_asb_50km_BUTTERFLIES.rds"))




# 2 - Load and clean traits data ================================


# Load traits data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v4.csv"))

# Load the host plant specificity trait that Manu computed (rm when new db):
host_plant_spec <- read.csv(file = here::here("integradiv_db",
                                              "INTEGRADIV_butterflies_traits.csv"))
host_plant_spec <- host_plant_spec %>%
  dplyr::filter(Trait == "HostPlantSpec") %>%
  dplyr::filter(Source == "Clarke_2022")

# Only keep butterflies data:
butterflies_traits <- dplyr::filter(INTEGRADIV_traits,
                                Taxon == "Butterflies")

# (rm when new db): rm hold Host Plant Spec to add new:
butterflies_traits <- butterflies_traits %>%
  dplyr::filter(Trait != "HostPlantSpec") %>%
  dplyr::bind_rows(host_plant_spec)

# Check if all species in the occurrence df are in the traits db: YES :)
setdiff(colnames(INTEGRADIV_butterflies_occ_df),
        unique(butterflies_traits$Species))

# Check traits db only contains species in the occurrence df: YES :)
setdiff(unique(butterflies_traits$Species),
        colnames(INTEGRADIV_butterflies_occ_df))


# Convert the OverwinteringLoc into a continuous variable (stage):
overwintering_df <- butterflies_traits %>%
  dplyr::filter(Trait == "OverwinteringStage")
# First convert classes to nb by keeping classes number only:
stringr::str_sub(overwintering_df$Value, start = 2, end = 7) <- ""
overwintering_df$Value <- as.numeric(overwintering_df$Value)
# Second compute the mean for each species:
overwintering_final_df <- overwintering_df %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise("OverwinteringStage" = mean(Value))
# Change the format of the data frame:
overwintering_final_df <- overwintering_final_df %>%
  tidyr::pivot_longer(! Species,
                      names_to = "Trait",
                      values_to = "Value")
overwintering_final_df$Value <- as.character(overwintering_final_df$Value)

# Only keep selected traits and add newly coded overwinteringstage:
butterflies_traits_df <- butterflies_traits %>%
  dplyr::filter(Trait %in% c("EggLayingType",
                             "FlyingPeriodBreadth",
                             "HostPlantSpec",
                             "VoltinismMax",
                             "WingIndex",
                             "WingSpan")) %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  dplyr::bind_rows(overwintering_final_df)

# Put it in the right format (species = rows, traits = columns)
butterflies_traits_df <- butterflies_traits_df %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)


# 3 - Check extreme traits values ==============================================


# We have spotted one error for birds:
# ... the idea is then to check all extreme values for each trait as they
# ... can profoundly impact the shape of the functional space
# ... by affecting the range by which Gower distance is standardised.

# For each trait, get the identity of species bearing traits > 99% quantile
# ... and < 1% quantile:

extreme_to_check_BUTTERFLIES <- check.quantiles(traits_df = butterflies_traits_df,
                                                quant_traits_vect = c("FlyingPeriodBreadth",
                                                                      "HostPlantSpec",
                                                                      "VoltinismMax",
                                                                      "WingIndex",
                                                                      "WingSpan"))

# Save:
write.csv(extreme_to_check_BUTTERFLIES, file = here::here("transformed_data",
                                                          "extreme_check_BUTTERFLIES.csv"))


# 4 - Format and save ==========================================================

# Format the right class for the traits:
butterflies_traits_df$EggLayingType <- ordered(butterflies_traits_df$EggLayingType,
                                                       levels = c("1_single_egg",
                                                                  "2_intermediate",
                                                                  "3_small_batch",
                                                                  "4_intermediate",
                                                                  "5_large_batch"))
butterflies_traits_df$FlyingPeriodBreadth <- as.numeric(butterflies_traits_df$FlyingPeriodBreadth)
butterflies_traits_df$WingIndex <- as.numeric(butterflies_traits_df$WingIndex)
butterflies_traits_df$WingSpan <- as.numeric(butterflies_traits_df$WingSpan)
butterflies_traits_df$OverwinteringStage <- ordered(butterflies_traits_df$OverwinteringStage,
                                                    levels = c("1",
                                                               "1.5",
                                                               "2",
                                                               "2.5",
                                                               "3",
                                                               "4"))
# For VoltinismMax, need to convert 1,5 to 1.5:
butterflies_traits_df$VoltinismMax[which(butterflies_traits_df$VoltinismMax == "1,5")] <- "1.5"
butterflies_traits_df$VoltinismMax <- as.numeric(butterflies_traits_df$VoltinismMax)
# For HostPlantSpec, show convert NULL into NA first:
butterflies_traits_df$HostPlantSpec[which(butterflies_traits_df$HostPlantSpec == "NULL")] <- NA
butterflies_traits_df$HostPlantSpec <- as.numeric(butterflies_traits_df$HostPlantSpec)


# Save the traits:
saveRDS(butterflies_traits_df, file = here::here("transformed_data",
                                                 "raw_traits_BUTTERFLIES.rds"))


# 3 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(butterflies_traits_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)






