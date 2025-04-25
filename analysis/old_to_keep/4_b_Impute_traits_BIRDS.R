################################################################################
##
## Script to impute traits based on missForests - for BIRDS
##
## Camille Magneville
##
## 15/04/2024 - 09/2024
##
## 4_a_Impute_traits_BIRDS.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ========================================================


sp_tr_BIRDS <- readRDS(here::here("transformed_data",
                                  "raw_traits_BIRDS.rds"))


# 2 - Traits and table in the right format ==============================


# Check that traits are in the right format:
str(sp_tr_BIRDS)

# See missing values:
# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(sp_tr_BIRDS, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

# Species as rownames:
sp_tr_BIRDS <- tibble::column_to_rownames(sp_tr_BIRDS,
                                          "Species")




# 3 - Impute traits based on Random Forest approach =====================

set.seed(42)

# Impute traits and check quality - with mice pkge:
## Check missing traits:
mice::md.pattern(sp_tr_BIRDS)
## Compute missing data - get 100 distributions:
init_test <- mice::mice(sp_tr_BIRDS, ntree = 300,
                        m = 100,
                        meth = 'rf', seed = 42)
imputed_traits <- init_test$imp

# Get the imputed values for traits with NA values:
offspring <- imputed_traits$OffspringPerRepro
fledging <- imputed_traits$FledgingPeriod
repro <- imputed_traits$ReproPerYear
breeding <- imputed_traits$FirstBreedingAge
longevity <- imputed_traits$LongevityMax

# Compute the mean over the 100 distributions:

# Offspring:
mean_offspring <- as.data.frame(apply(offspring, 1, mean))
colnames(mean_offspring) <- "OffspringPerRepro"
mean_offspring <- tibble::rownames_to_column(mean_offspring,
                                                "Species")
# FledgingPeriod:
mean_fledging <- as.data.frame(apply(fledging, 1, mean))
colnames(mean_fledging) <- "FledgingPeriod"
mean_fledging <- tibble::rownames_to_column(mean_fledging,
                                       "Species")
# ReproPerYear (convert as numeric to do the mean):
mean_repro <- as.data.frame(apply(repro, 2, as.numeric))
rownames(mean_repro) <- rownames(repro)
mean_repro <- as.data.frame(apply(mean_repro, 1, mean))
mean_repro <- round(mean_repro, digits = 1)
colnames(mean_repro) <- "ReproPerYear"
mean_repro <- tibble::rownames_to_column(mean_repro,
                                             "Species")

# FirstBreedingAge (convert as numeric to do the mean):
mean_breeding <- as.data.frame(apply(breeding, 2, as.numeric))
rownames(mean_breeding) <- rownames(breeding)
mean_breeding <- as.data.frame(apply(mean_breeding, 1, mean))
mean_breeding <- round(mean_breeding, digits = 1)
colnames(mean_breeding) <- "FirstBreedingAge"
mean_breeding <- tibble::rownames_to_column(mean_breeding,
                                            "Species")

# LongevityMax (convert as numeric to do the mean):
mean_longevity <- as.data.frame(apply(longevity, 1, mean))
colnames(mean_longevity) <- "LongevityMax"
mean_longevity <- tibble::rownames_to_column(mean_longevity,
                                             "Species")

# For the two ordered traits with numerical values, go to the closest
# ... category seen in the observed data:
# Breeding Age:
levels(sp_tr_BIRDS$FirstBreedingAge)
mean_breeding$FirstBreedingAge
mean_breeding$FirstBreedingAge <- rep("1", nrow(mean_breeding))
# Repro:
levels(sp_tr_BIRDS$ReproPerYear)
mean_repro$ReproPerYear
mean_repro$ReproPerYear <- c("1", "1", "1.5")

# Complete the sp_tr_BIRDS with imputed traits:
# First, need to put ReproPerYEar and FirstBreedingAgeas numeric all traits to be able to update rows:
sp_tr_BIRDS$ReproPerYear <- as.numeric(sp_tr_BIRDS$ReproPerYear)
sp_tr_BIRDS$FirstBreedingAge <- as.numeric(sp_tr_BIRDS$FirstBreedingAge)
sp_tr_BIRDS <- tibble::rownames_to_column(sp_tr_BIRDS,
                                          "Species")
complete_sp_tr_BIRDS <- sp_tr_BIRDS %>%
  dplyr::rows_update(mean_offspring, by = "Species") %>%
  dplyr::rows_update(mean_fledging, by = "Species") %>%
  dplyr::rows_update(mean_repro, by = "Species") %>%
  dplyr::rows_update(mean_breeding, by = "Species") %>%
  dplyr::rows_update(mean_longevity, by = "Species")
complete_sp_tr_BIRDS <- tibble::column_to_rownames(complete_sp_tr_BIRDS,
                                                   "Species")


# 4 - Compare imputed traits with observed ones ========================

## Density plots:
# OffspringPerRepro:
ggplot2::ggplot(data = sp_tr_BIRDS) +
  ggplot2::geom_density(ggplot2::aes(x = OffspringPerRepro),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_BIRDS,
                        ggplot2::aes(x = OffspringPerRepro),
                        fill="#ce446e", alpha = 0.5)

# FledgingPeriod:
ggplot2::ggplot(data = sp_tr_BIRDS) +
  ggplot2::geom_density(ggplot2::aes(x = FledgingPeriod),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_BIRDS,
                        ggplot2::aes(x = FledgingPeriod),
                        fill="#ce446e", alpha = 0.5)

# FirstBreedingAge (as ordered factor, values changed when as.numeric,
# ... but ok for the plot):
ggplot2::ggplot(data = sp_tr_BIRDS) +
  ggplot2::geom_density(ggplot2::aes(x = as.numeric(FirstBreedingAge)),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_BIRDS,
                        ggplot2::aes(x = as.numeric(FirstBreedingAge)),
                        fill="#ce446e", alpha = 0.5)

# ReproPerYear  (as ordered factor, values changed when as.numeric,
# ... but ok for the plot):
ggplot2::ggplot(data = sp_tr_BIRDS) +
  ggplot2::geom_density(ggplot2::aes(x = as.numeric(ReproPerYear)),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_BIRDS,
                        ggplot2::aes(x = as.numeric(ReproPerYear)),
                        fill="#ce446e", alpha = 0.5)

# Longevity Max:
ggplot2::ggplot(data = sp_tr_BIRDS) +
  ggplot2::geom_density(ggplot2::aes(x = LongevityMax),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_BIRDS,
                        ggplot2::aes(x = LongevityMax),
                        fill="#ce446e", alpha = 0.5)


## Points distribution:

# Build a data frame that contains observed data:
# Only keep traits of interest (those imputed):
plot_sp_tr_BIRDS <- sp_tr_BIRDS %>%
  dplyr::select(c("Species",
                  "OffspringPerRepro",
                  "FledgingPeriod",
                  "FirstBreedingAge",
                  "ReproPerYear",
                  "LongevityMax"))
# Format as numeric for illustration purposes:
plot_sp_tr_BIRDS[, which(colnames(plot_sp_tr_BIRDS) != c("Species"))] <- apply(plot_sp_tr_BIRDS[, which(colnames(plot_sp_tr_BIRDS) != c("Species"))],
                                                                               2,
                                                                               as.numeric)
# Format to plot:
plot_sp_tr_BIRDS <- plot_sp_tr_BIRDS %>%
  tidyr::pivot_longer(cols = c('LongevityMax',
                               'FledgingPeriod',
                               'FirstBreedingAge',
                               'ReproPerYear',
                               'OffspringPerRepro'),
                      names_to = 'traits_nm',
                      values_to = 'values') %>%
  na.omit()

# Build a dataframe that contains imputed data:
plot_imputed_traits <- mean_longevity %>%
  dplyr::full_join(mean_fledging) %>%
  dplyr::full_join(mean_breeding) %>%
  dplyr::full_join(mean_offspring) %>%
  dplyr::full_join(mean_repro)
# Format as numeric for the plot:
plot_imputed_traits[, which(colnames(plot_imputed_traits) != c("Species"))] <- apply(plot_imputed_traits[, which(colnames(plot_imputed_traits) != c("Species"))],
                                                                                     2, as.numeric)
# Format for the plot:
plot_imputed_traits <- plot_imputed_traits %>%
  tidyr::pivot_longer(cols = c('LongevityMax',
                               'FledgingPeriod',
                               'FirstBreedingAge',
                               'ReproPerYear',
                               'OffspringPerRepro'),
                      names_to = 'traits_nm',
                      values_to = 'values') %>%
  na.omit()


# FledgingPeriod
ggplot2::ggplot(data = plot_sp_tr_BIRDS[which(plot_sp_tr_BIRDS$traits_nm == "FledgingPeriod"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "FledgingPeriod"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# FirstBreedingAge
ggplot2::ggplot(data = plot_sp_tr_BIRDS[which(plot_sp_tr_BIRDS$traits_nm == "FirstBreedingAge"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "FirstBreedingAge"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# ReproPerYear
ggplot2::ggplot(data = plot_sp_tr_BIRDS[which(plot_sp_tr_BIRDS$traits_nm == "ReproPerYear"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "ReproPerYear"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)
# LongevityMax:
ggplot2::ggplot(data = plot_sp_tr_BIRDS[which(plot_sp_tr_BIRDS$traits_nm == "LongevityMax"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "LongevityMax"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# OffspringPerRepro:
ggplot2::ggplot(data = plot_sp_tr_BIRDS[which(plot_sp_tr_BIRDS$traits_nm == "OffspringPerRepro"), ]) +
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
str(complete_sp_tr_BIRDS)

# Save imputed traits:
saveRDS(complete_sp_tr_BIRDS, here::here("transformed_data",
                                         "final_traits_BIRDS.rds"))


# 5 - Compare the functional spaces ====================================

# Note: compare functional space of observed traits and the one with
# ... imputed and observed traits: not a lot of species missing,
# ... so I can w=check how much the functional spaces are similar or not

# Build the dataframe that contains traits category:
# Create traits category data frame:
traits_nm <- c("FirstBreedingAge",
               "BeakLengthCulmen",
               "BeakRatio",
               "ReproPerYear",
               "OffspringPerRepro",
               "FledgingPeriod",
               "HandWingIndex",
               "LongevityMax" ,
               "BodyMass" ,
               "Migration" ,
               "TailLength",
               "TarsusLength")
traits_cat <- c("O", "Q", "Q", "O", "Q", "Q", "Q", "Q", "Q", "O", "Q", "Q")
trait_cat_df <- data.frame(traits_nm, traits_cat)
colnames(trait_cat_df) <- c("trait_name", "trait_type")

# FOR COMPLETE TRAITS (obs + imputed):
# Compute functional distance between species:
sp_dist_comp <- mFD::funct.dist(
  sp_tr         = complete_sp_tr_BIRDS,
  tr_cat        = trait_cat_df,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)
# Get species coordinates:
fspaces_quality_comp <- mFD::quality.fspaces(
  sp_dist             = sp_dist_comp,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")
fspaces_quality_comp$quality_fspaces
sp_faxes_coord <- fspaces_quality_comp$"details_fspaces"$"sp_pc_coord"
# Plot functional space:
fct_space_comp <- mFD::funct.space.plot(
  sp_faxes_coord = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
  faxes           = NULL,
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgoldenrod2",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      =  "turquoise",
  fill_vert       =  "turquoise",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)
fct_space_comp


# FOR OBSERVED TRAITS (remove NA - so nb of species decrease +++):
sp_tr_BIRDS <- tibble::column_to_rownames(sp_tr_BIRDS,
                                          "Species")
miss_sp_tr_BIRDS <- na.omit(sp_tr_BIRDS)
# Compute functional distance between species:
sp_dist_obs <- mFD::funct.dist(
  sp_tr         = miss_sp_tr_BIRDS,
  tr_cat        = trait_cat_df,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)
# Get species coordinates:
fspaces_quality_obs <- mFD::quality.fspaces(
  sp_dist             = sp_dist_obs,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")
fspaces_quality_obs$quality_fspaces
sp_faxes_coord <- fspaces_quality_obs$"details_fspaces"$"sp_pc_coord"
# Plot functional space:
fct_space_obs <- mFD::funct.space.plot(
  sp_faxes_coord = sp_faxes_coord[, c("PC1", "PC2", "PC3", "PC4")],
  faxes           = NULL,
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgoldenrod2",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      =  "turquoise",
  fill_vert       =  "turquoise",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)
fct_space_obs

# Compare:
fct_space_comp$patchwork
fct_space_obs$patchwork





