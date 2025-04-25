################################################################################
##
## Script to impute traits based on missForests - for REPTILES
##
## Camille Magneville
##
## 18/04/2024 - 09/2024
##
## 4_a_Impute_traits_REPTILES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ========================================================


sp_tr_REPTILES <- readRDS(here::here("transformed_data",
                                  "raw_traits_REPTILES.rds"))


# 2 - Traits and table in the right format ==============================


# Check that traits are in the right format:
str(sp_tr_REPTILES)

# See missing values:
# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(sp_tr_REPTILES, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

# Species as rownames:
sp_tr_REPTILES <- tibble::column_to_rownames(sp_tr_REPTILES,
                                          "Species")

# 3 - Impute traits based on Random Forest approach =====================

# Note: remove fuzzy traits:
fuzzy_tr_df <- sp_tr_REPTILES %>%
  dplyr::select(c("terrestrial",
                  "saxicolous",
                  "arboreal",
                  "cryptic",
                  "fossorial",
                  "aquatic",
                  "semi_aquatic"))

sp_tr_REPTILES_nonfuzzy <- sp_tr_REPTILES %>%
  dplyr::select(- c("terrestrial",
                  "saxicolous",
                  "arboreal",
                  "cryptic",
                  "fossorial",
                  "aquatic",
                  "semi_aquatic"))

set.seed(42)

# Impute traits and check quality - with mice pkge:
## Check missing traits:
mice::md.pattern(sp_tr_REPTILES_nonfuzzy)
## Compute missing data - get 100 distributions:
init_test <- mice::mice(sp_tr_REPTILES_nonfuzzy, ntree = 300,
                        m = 100,
                        meth = 'rf', seed = 42)
imputed_traits <- init_test$imp

# Get the imputed values for traits with NA values:
offspring <- imputed_traits$OffspringPerRepro
svl <- imputed_traits$SVLMax
longevity <- imputed_traits$LongevityMax
repro <- imputed_traits$ReproPerYear
bodytemp <- imputed_traits$BodyTemperature
breeding <- imputed_traits$FirstBreedingAge
activity <- imputed_traits$ActivitySeasonLength

# Compute the mean over the 100 distributions:

# OffspringPerRepro:
mean_offspring <- as.data.frame(apply(offspring, 1, mean))
colnames(mean_offspring) <- "OffspringPerRepro"
mean_offspring <- tibble::rownames_to_column(mean_offspring,
                                                "Species")
# SVLMax:
mean_svl <- as.data.frame(apply(svl, 1, mean))
colnames(mean_svl) <- "SVLMax"
mean_svl <- tibble::rownames_to_column(mean_svl,
                                       "Species")
# LongevityMax:
mean_longevity <- as.data.frame(apply(longevity, 1, mean))
colnames(mean_longevity) <- "LongevityMax"
mean_longevity <- tibble::rownames_to_column(mean_longevity,
                                            "Species")
# ReproPerYear:
mean_repro <- as.data.frame(apply(repro, 1, mean))
colnames(mean_repro) <- "ReproPerYear"
mean_repro <- tibble::rownames_to_column(mean_repro,
                                      "Species")

# Body temperature:
mean_bodytemp <- as.data.frame(apply(bodytemp, 1, mean))
colnames(mean_bodytemp) <- "BodyTemperature"
mean_bodytemp <- tibble::rownames_to_column(mean_bodytemp,
                                         "Species")

# Breeding:
mean_breeding <- as.data.frame(apply(breeding, 1, mean))
colnames(mean_breeding) <- "FirstBreedingAge"
mean_breeding <- tibble::rownames_to_column(mean_breeding,
                                            "Species")

# Activity:
mean_activity <- as.data.frame(apply(activity, 1, mean))
colnames(mean_activity) <- "ActivitySeasonLength"
mean_activity <- tibble::rownames_to_column(mean_activity,
                                            "Species")

# Complete the sp_tr_REPTILES with imputed traits:
sp_tr_REPTILES <- tibble::rownames_to_column(sp_tr_REPTILES,
                                             "Species")
complete_sp_tr_REPTILES <- sp_tr_REPTILES %>%
  dplyr::rows_update(mean_offspring, by = "Species") %>%
  dplyr::rows_update(mean_svl, by = "Species") %>%
  dplyr::rows_update(mean_longevity, by = "Species") %>%
  dplyr::rows_update(mean_repro, by = "Species") %>%
  dplyr::rows_update(mean_bodytemp, by = "Species") %>%
  dplyr::rows_update(mean_breeding, by = "Species") %>%
  dplyr::rows_update(mean_activity, by = "Species")
complete_sp_tr_REPTILES <- tibble::column_to_rownames(complete_sp_tr_REPTILES,
                                                      "Species")

# 4 - Compare imputed traits with observed ones ========================

## Density plots:
# OffspringPerRepro:
ggplot2::ggplot(data = sp_tr_REPTILES) +
  ggplot2::geom_density(ggplot2::aes(x = OffspringPerRepro),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_REPTILES,
                        ggplot2::aes(x = OffspringPerRepro),
                        fill="#ce446e", alpha = 0.5)

# SVLMax:
ggplot2::ggplot(data = sp_tr_REPTILES) +
  ggplot2::geom_density(ggplot2::aes(x = SVLMax),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_REPTILES,
                        ggplot2::aes(x = SVLMax),
                        fill="#ce446e", alpha = 0.5)

# ReproPerYear:
ggplot2::ggplot(data = sp_tr_REPTILES) +
  ggplot2::geom_density(ggplot2::aes(x = ReproPerYear),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_REPTILES,
                        ggplot2::aes(x = ReproPerYear),
                        fill="#ce446e", alpha = 0.5)

# LongevityMax:
ggplot2::ggplot(data = sp_tr_REPTILES) +
  ggplot2::geom_density(ggplot2::aes(x = LongevityMax),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_REPTILES,
                        ggplot2::aes(x = LongevityMax),
                        fill="#ce446e", alpha = 0.5)

# Body Temperature:
ggplot2::ggplot(data = sp_tr_REPTILES) +
  ggplot2::geom_density(ggplot2::aes(x = BodyTemperature),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_REPTILES,
                        ggplot2::aes(x = BodyTemperature),
                        fill="#ce446e", alpha = 0.5)

# Activity length:
ggplot2::ggplot(data = sp_tr_REPTILES) +
  ggplot2::geom_density(ggplot2::aes(x = ActivitySeasonLength),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_REPTILES,
                        ggplot2::aes(x = ActivitySeasonLength),
                        fill="#ce446e", alpha = 0.5)

# First Breeding Age:
ggplot2::ggplot(data = sp_tr_REPTILES) +
  ggplot2::geom_density(ggplot2::aes(x = FirstBreedingAge),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_REPTILES,
                        ggplot2::aes(x = FirstBreedingAge),
                        fill="#ce446e", alpha = 0.5)


## Points distribution:

# Build a data frame that contains observed data:
# Only keep traits of interest (those imputed):
plot_sp_tr_REPTILES <- sp_tr_REPTILES %>%
  dplyr::select(c("Species",
                  "OffspringPerRepro",
                  "SVLMax",
                  "ReproPerYear",
                  "LongevityMax",
                  "ActivitySeasonLength",
                  "FirstBreedingAge",
                  "BodyTemperature"))
# Format as numeric for illustration purposes:
plot_sp_tr_REPTILES[, which(colnames(plot_sp_tr_REPTILES) != c("Species"))] <- apply(plot_sp_tr_REPTILES[, which(colnames(plot_sp_tr_REPTILES) != c("Species"))],
                                                                                     2,
                                                                                     as.numeric)
# Format to plot:
plot_sp_tr_REPTILES <- plot_sp_tr_REPTILES %>%
  tidyr::pivot_longer(cols = c("OffspringPerRepro",
                               "SVLMax",
                               "ReproPerYear",
                               "LongevityMax",
                               "ActivitySeasonLength",
                               "FirstBreedingAge",
                               "BodyTemperature"),
                      names_to = 'traits_nm',
                      values_to = 'values') %>%
  na.omit()

# Build a dataframe that contains imputed data:
plot_imputed_traits <- mean_bodytemp %>%
  dplyr::full_join(mean_svl) %>%
  dplyr::full_join(mean_repro) %>%
  dplyr::full_join(mean_offspring) %>%
  dplyr::full_join(mean_longevity) %>%
  dplyr::full_join(mean_breeding) %>%
  dplyr::full_join(mean_activity)
# Format as numeric for the plot:
plot_imputed_traits[, which(colnames(plot_imputed_traits) != c("Species"))] <- apply(plot_imputed_traits[, which(colnames(plot_imputed_traits) != c("Species"))],
                                                                                     2, as.numeric)
# Format for the plot:
plot_imputed_traits <- plot_imputed_traits %>%
  tidyr::pivot_longer(cols = c("OffspringPerRepro",
                               "SVLMax",
                               "ReproPerYear",
                               "LongevityMax",
                               "ActivitySeasonLength",
                               "FirstBreedingAge",
                               "BodyTemperature"),
                      names_to = 'traits_nm',
                      values_to = 'values') %>%
  na.omit()


# SVLMax
ggplot2::ggplot(data = plot_sp_tr_REPTILES[which(plot_sp_tr_REPTILES$traits_nm == "SVLMax"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "SVLMax"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# ReproPerYear
ggplot2::ggplot(data = plot_sp_tr_REPTILES[which(plot_sp_tr_REPTILES$traits_nm == "ReproPerYear"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "ReproPerYear"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# LongevityMax
ggplot2::ggplot(data = plot_sp_tr_REPTILES[which(plot_sp_tr_REPTILES$traits_nm == "LongevityMax"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "LongevityMax"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)
# ActiviySeasonLength:
ggplot2::ggplot(data = plot_sp_tr_REPTILES[which(plot_sp_tr_REPTILES$traits_nm == "ActivitySeasonLength"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "ActivitySeasonLength"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# OffspringPerRepro:
ggplot2::ggplot(data = plot_sp_tr_REPTILES[which(plot_sp_tr_REPTILES$traits_nm == "OffspringPerRepro"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "OffspringPerRepro"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)


# Check and change if necessary the class of each trait:
str(complete_sp_tr_REPTILES)


# Save imputed traits:
saveRDS(complete_sp_tr_REPTILES, here::here("transformed_data",
                                            "final_traits_REPTILES.rds"))


# 5 - Compare the functional spaces ====================================

# Note: compare functional space of observed traits and the one with
# ... imputed and observed traits: but as I have to remove all species
# ... with NA to build the one with observed traits, it changes a lot
# ... distances between species - don't mean anything


