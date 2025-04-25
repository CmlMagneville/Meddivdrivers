################################################################################
##
## Script to impute traits based on missForests - for TREES
##
## Camille Magneville
##
## 12/04/2024 - 09/2024
##
## 4_a_Impute_traits_TREES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ========================================================


sp_tr_TREES <- readRDS(here::here("transformed_data",
                                  "raw_traits_TREES.rds"))


# 2 - Traits and table in the right format ==============================

# Check traits in the right format:
class(sp_tr_TREES$BloomBreadth)
class(sp_tr_TREES$DispDist)
class(sp_tr_TREES$HeightMax)
class(sp_tr_TREES$LA)
class(sp_tr_TREES$LeafOutline)
class(sp_tr_TREES$LeafPhenology)
class(sp_tr_TREES$Pollination)
class(sp_tr_TREES$SLA)
class(sp_tr_TREES$SeedMass)
class(sp_tr_TREES$SexualSystem)
class(sp_tr_TREES$StemSpecDens)

# See missing values:
# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(sp_tr_TREES, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

# Species as rownames:
sp_tr_TREES <- tibble::column_to_rownames(sp_tr_TREES,
                                          "Species")

# 3 - Impute traits based on Random Forest approach =====================


set.seed(42)

# Impute traits and check quality - with mice pkge:
## Check missing traits:
mice::md.pattern(sp_tr_TREES)
## Compute missing data - get 100 distributions:
init_test <- mice::mice(sp_tr_TREES, ntree = 300,
                        m = 100,
                        meth = 'rf', seed = 42)
imputed_traits <- init_test$imp

# Get the imputed values for traits with NA values:
stemspecdens <- imputed_traits$StemSpecDens
sla <- imputed_traits$SLA
seedmass <- imputed_traits$SeedMass
la <- imputed_traits$LA
bloombreadth <- imputed_traits$BloomBreadth

# Compute the mean over the 100 distributions:

# StemSpecDens:
mean_stemspecdens <- as.data.frame(apply(stemspecdens, 1, mean))
colnames(mean_stemspecdens) <- "StemSpecDens"
mean_stemspecdens <- tibble::rownames_to_column(mean_stemspecdens,
                                          "Species")
# SLA:
mean_sla <- as.data.frame(apply(sla, 1, mean))
colnames(mean_sla) <- "SLA"
mean_sla <- tibble::rownames_to_column(mean_sla,
                                       "Species")
# SeedMass:
mean_seedmass <- as.data.frame(apply(seedmass, 1, mean))
colnames(mean_seedmass) <- "SeedMass"
mean_seedmass <- tibble::rownames_to_column(mean_seedmass,
                                                "Species")
# LA:
mean_la <- as.data.frame(apply(la, 1, mean))
colnames(mean_la) <- "LA"
mean_la <- tibble::rownames_to_column(mean_la,
                                      "Species")

# BloomBreadth (convert as numeric to do the mean):
mean_bloombreadth <- as.data.frame(apply(bloombreadth, 2, as.numeric))
rownames(mean_bloombreadth) <- rownames(bloombreadth)
mean_bloombreadth <- as.data.frame(apply(mean_bloombreadth, 1, mean))
mean_bloombreadth <- round(mean_bloombreadth, digits = 1)
colnames(mean_bloombreadth) <- "BloomBreadth"
mean_bloombreadth <- tibble::rownames_to_column(mean_bloombreadth,
                                                "Species")
mean_bloombreadth$BloomBreadth <- as.factor(mean_bloombreadth$BloomBreadth)


# Complete the sp_tr_TREES with imputed traits:
sp_tr_TREES <- tibble::rownames_to_column(sp_tr_TREES,
                                          "Species")
complete_sp_tr_TREES <- sp_tr_TREES %>%
  dplyr::rows_update(mean_stemspecdens, by = "Species") %>%
  dplyr::rows_update(mean_sla, by = "Species") %>%
  dplyr::rows_update(mean_seedmass, by = "Species") %>%
  dplyr::rows_update(mean_la, by = "Species") %>%
  dplyr::rows_update(mean_bloombreadth, by = "Species")
complete_sp_tr_TREES <- tibble::column_to_rownames(complete_sp_tr_TREES,
                                                   "Species")

# 4 - Compare imputed traits with observed ones ========================

## Density plots:
# StemSpecDens:
ggplot2::ggplot(data = sp_tr_TREES) +
  ggplot2::geom_density(ggplot2::aes(x = StemSpecDens),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_TREES,
                        ggplot2::aes(x = StemSpecDens),
                        fill="#ce446e", alpha = 0.5)

# SLA:
ggplot2::ggplot(data = sp_tr_TREES) +
  ggplot2::geom_density(ggplot2::aes(x = SLA),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_TREES,
                        ggplot2::aes(x = SLA),
                        fill="#ce446e", alpha = 0.5)

# LA:
ggplot2::ggplot(data = sp_tr_TREES) +
  ggplot2::geom_density(ggplot2::aes(x = LA),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_TREES,
                        ggplot2::aes(x = LA),
                        fill="#ce446e", alpha = 0.5)

# SeedMass:
ggplot2::ggplot(data = sp_tr_TREES) +
  ggplot2::geom_density(ggplot2::aes(x = SeedMass),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_TREES,
                        ggplot2::aes(x = SeedMass),
                        fill="#ce446e", alpha = 0.5)

# Bloom Breadth:
ggplot2::ggplot(data = sp_tr_TREES) +
  ggplot2::geom_density(ggplot2::aes(x = as.numeric(BloomBreadth)),
                        fill="#69b3a2", alpha = 0.5) +
  ggplot2::geom_density(data = complete_sp_tr_TREES,
                      ggplot2::aes(x = as.numeric(BloomBreadth)),
                      fill="#ce446e", alpha = 0.5)


## Points distribution:

sp_tr_TREES <- tibble::rownames_to_column(sp_tr_TREES,
                                          "Species")

# Build a data frame that contains observed data:
# Only keep traits of interest (those imputed):
plot_sp_tr_TREES <- sp_tr_TREES %>%
  dplyr::select(c("Species",
                  "StemSpecDens",
                  "SLA",
                  "LA",
                  "SeedMass",
                  "BloomBreadth"))
# Format as numeric for illustration purposes:
plot_sp_tr_TREES[, which(colnames(plot_sp_tr_TREES) != c("Species"))] <- apply(plot_sp_tr_TREES[, which(colnames(plot_sp_tr_TREES) != c("Species"))],
                                                                               2,
                                                                               as.numeric)
# Format to plot:
plot_sp_tr_TREES <- plot_sp_tr_TREES %>%
  tidyr::pivot_longer(cols = c('BloomBreadth',
                               'SLA',
                               'LA',
                               'SeedMass',
                               'StemSpecDens'),
                      names_to = 'traits_nm',
                      values_to = 'values') %>%
  na.omit()

# Build a dataframe that contains imputed data:
plot_imputed_traits <- mean_bloombreadth %>%
  dplyr::full_join(mean_sla) %>%
  dplyr::full_join(mean_la) %>%
  dplyr::full_join(mean_stemspecdens) %>%
  dplyr::full_join(mean_seedmass)
# Format as numeric for the plot:
plot_imputed_traits[, which(colnames(plot_imputed_traits) != c("Species"))] <- apply(plot_imputed_traits[, which(colnames(plot_imputed_traits) != c("Species"))],
                                                                                     2, as.numeric)
# Format for the plot:
plot_imputed_traits <- plot_imputed_traits %>%
  tidyr::pivot_longer(cols = c('BloomBreadth',
                               'SLA',
                               'LA',
                               'SeedMass',
                               'StemSpecDens'),
                      names_to = 'traits_nm',
                      values_to = 'values') %>%
  na.omit()


# SLA
ggplot2::ggplot(data = plot_sp_tr_TREES[which(plot_sp_tr_TREES$traits_nm == "SLA"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                      y = values),
                      color = "#69b3a2",
                      alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "SLA"), ],
                      ggplot2::aes(x = traits_nm,
                                   y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# LA
ggplot2::ggplot(data = plot_sp_tr_TREES[which(plot_sp_tr_TREES$traits_nm == "LA"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "LA"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# SeedMass
ggplot2::ggplot(data = plot_sp_tr_TREES[which(plot_sp_tr_TREES$traits_nm == "SeedMass"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "SeedMass"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)
# BloomBreadth:
ggplot2::ggplot(data = plot_sp_tr_TREES[which(plot_sp_tr_TREES$traits_nm == "BloomBreadth"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "BloomBreadth"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)

# StemSpecDens:
ggplot2::ggplot(data = plot_sp_tr_TREES[which(plot_sp_tr_TREES$traits_nm == "StemSpecDens"), ]) +
  ggplot2::geom_jitter(ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#69b3a2",
                       alpha = 0.6) +
  ggplot2::geom_jitter(data = plot_imputed_traits[which(plot_imputed_traits$traits_nm == "StemSpecDens"), ],
                       ggplot2::aes(x = traits_nm,
                                    y = values),
                       color = "#ce446e",
                       alpha = 0.6)



# Save imputed traits:
saveRDS(complete_sp_tr_TREES, here::here("transformed_data",
                                  "final_traits_TREES.rds"))


# 5 - Compare the functional spaces ====================================

# Note: compare functional space of observed traits and the one with
# ... imputed and observed traits: but as I have to remove all species
# ... with NA to build the one with observed traits, it changes a lot
# ... distances between species - thus can't really use this comparison

# Build the dataframe that contains traits category:
# Create traits category data frame:
traits_nm <- c("BloomBreadth",
               "DispDist",
               "HeightMax",
               "LA",
               "LeafOutline",
               "LeafPhenology",
               "Pollination",
               "SLA",
               "SeedMass",
               "SexualSystem",
               "StemSpecDens")
traits_cat <- c("N", "O", "Q", "Q", "O", "O", "O", "Q", "Q", "N", "Q")
trait_cat_df <- data.frame(traits_nm, traits_cat)
colnames(trait_cat_df) <- c("trait_name", "trait_type")

# FOR COMPLETE TRAITS (obs + imputed):
# Compute functional distance between species:
sp_dist_comp <- mFD::funct.dist(
  sp_tr         = complete_sp_tr_TREES,
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
sp_tr_TREES <- tibble::column_to_rownames(sp_tr_TREES,
                                           "Species")
miss_sp_tr_TREES <- na.omit(sp_tr_TREES)
# Compute functional distance between species:
sp_dist_obs <- mFD::funct.dist(
  sp_tr         = miss_sp_tr_TREES,
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



