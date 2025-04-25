################################################################################
##
## Script to compare the main categories importance between taxa and metrics
##
## Camille Magneville
##
## 11/2024
##
## 13_Compare_main_categories.R
##
################################################################################

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# 1 - FD - Heatmap =============================================================

# 1 - a - Load data ============================================================

# Load data for all taxa for richness
richn_trees <- readRDS(here::here("transformed_data",
                                  "std_rf_trees_FD_fric_50.rds"))
richn_butterflies <- readRDS(here::here("transformed_data",
                                        "std_rf_butterflies_FD_fric_50.rds"))
richn_birds <- readRDS(here::here("transformed_data",
                                  "std_rf_birds_FD_fric_50.rds"))
richn_reptiles <- readRDS(here::here("transformed_data",
                                     "std_rf_reptiles_FD_fric_50.rds"))
richn_mammals <- readRDS(here::here("transformed_data",
                                    "std_rf_mammals_FD_fric_50.rds"))

# Load data for all taxa for dispersion
disp_trees <- readRDS(here::here("transformed_data",
                                  "std_rf_trees_FD_fmpd_50.rds"))
disp_butterflies <- readRDS(here::here("transformed_data",
                                        "std_rf_butterflies_FD_fmpd_50.rds"))
disp_birds <- readRDS(here::here("transformed_data",
                                  "std_rf_birds_FD_fmpd_50.rds"))
disp_reptiles <- readRDS(here::here("transformed_data",
                                     "std_rf_reptiles_FD_fmpd_50.rds"))
disp_mammals <- readRDS(here::here("transformed_data",
                                    "std_rf_mammals_FD_fmpd_50.rds"))

# Load data for all taxa for originality
orig_trees <- readRDS(here::here("transformed_data",
                                  "std_rf_trees_FD_fori_50.rds"))
orig_butterflies <- readRDS(here::here("transformed_data",
                                        "std_rf_butterflies_FD_fori_50.rds"))
orig_birds <- readRDS(here::here("transformed_data",
                                  "std_rf_birds_FD_fori_50.rds"))
orig_reptiles <- readRDS(here::here("transformed_data",
                                     "std_rf_reptiles_FD_fori_50.rds"))
orig_mammals <- readRDS(here::here("transformed_data",
                                    "std_rf_mammals_FD_fori_50.rds"))

# 1 - b - Heatmap of drivers categories importance =============================

list_richness <- list("Trees" = richn_trees,
                      "Butterflies" = richn_butterflies,
                      "Birds" = richn_birds,
                      "Reptiles" = richn_reptiles,
                      "Mammals" = richn_mammals)
list_dispersion <- list("Trees" = disp_trees,
                        "Butterflies" = disp_butterflies,
                        "Birds" = disp_birds,
                        "Reptiles" = disp_reptiles,
                        "Mammals" = disp_mammals)
list_originality <- list("Trees" = orig_trees,
                         "Butterflies" = orig_butterflies,
                         "Birds" = orig_birds,
                         "Reptiles" = orig_reptiles,
                         "Mammals" = orig_mammals)

heatmap_values <- heatmap.categories(list_richness = list_richness,
                   list_dispersion = list_dispersion,
                   list_originality = list_originality,
                   facet_nm = "Functional diversity")

saveRDS(heatmap_values, here::here("transformed_data",
                                   "heatmap_values_FD.rds"))


# 2 - PD - Heatmap =============================================================
# 2 - a - Load data ================================================================

# Load data for all taxa for richness
richn_trees <- readRDS(here::here("transformed_data",
                                  "std_rf_trees_PD_faith_50.rds"))
richn_butterflies <- readRDS(here::here("transformed_data",
                                        "std_rf_butterflies_PD_faith_50.rds"))
richn_birds <- readRDS(here::here("transformed_data",
                                  "std_rf_birds_PD_faith_50.rds"))
richn_reptiles <- readRDS(here::here("transformed_data",
                                     "std_rf_reptiles_PD_faith_50.rds"))
richn_mammals <- readRDS(here::here("transformed_data",
                                    "std_rf_mammals_PD_faith_50.rds"))

# Load data for all taxa for dispersion
disp_trees <- readRDS(here::here("transformed_data",
                                 "std_rf_trees_PD_mpd_50.rds"))
disp_butterflies <- readRDS(here::here("transformed_data",
                                       "std_rf_butterflies_PD_mpd_50.rds"))
disp_birds <- readRDS(here::here("transformed_data",
                                 "std_rf_birds_PD_mpd_50.rds"))
disp_reptiles <- readRDS(here::here("transformed_data",
                                    "std_rf_reptiles_PD_mpd_50.rds"))
disp_mammals <- readRDS(here::here("transformed_data",
                                   "std_rf_mammals_PD_mpd_50.rds"))

# Load data for all taxa for originality
orig_trees <- readRDS(here::here("transformed_data",
                                 "std_rf_trees_PD_mntd_50.rds"))
orig_butterflies <- readRDS(here::here("transformed_data",
                                       "std_rf_butterflies_PD_mntd_50.rds"))
orig_birds <- readRDS(here::here("transformed_data",
                                 "std_rf_birds_PD_mntd_50.rds"))
orig_reptiles <- readRDS(here::here("transformed_data",
                                    "std_rf_reptiles_PD_mntd_50.rds"))
orig_mammals <- readRDS(here::here("transformed_data",
                                   "std_rf_mammals_PD_mntd_50.rds"))

# 2 - b - Heatmap of drivers categories importance =============================

list_richness <- list("Trees" = richn_trees,
                      "Butterflies" = richn_butterflies,
                      "Birds" = richn_birds,
                      "Reptiles" = richn_reptiles,
                      "Mammals" = richn_mammals)
list_dispersion <- list("Trees" = disp_trees,
                        "Butterflies" = disp_butterflies,
                        "Birds" = disp_birds,
                        "Reptiles" = disp_reptiles,
                        "Mammals" = disp_mammals)
list_originality <- list("Trees" = orig_trees,
                         "Butterflies" = orig_butterflies,
                         "Birds" = orig_birds,
                         "Reptiles" = orig_reptiles,
                         "Mammals" = orig_mammals)

heatmap_values <- heatmap.categories(list_richness = list_richness,
                   list_dispersion = list_dispersion,
                   list_originality = list_originality,
                   facet_nm = "Phylogenetic diversity")
saveRDS(heatmap_values, here::here("transformed_data",
                                   "heatmap_values_PD.rds"))


# 3 - PD - Violin plots and tests ==============================================

# Note: Standardisd importance is used here - and not raw relative importance

# Richness =====================================================================

# Load data  -------------------------------------------------------------------

# Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "std_rf_birds_PD_Faith_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_PD_Faith_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "std_rf_trees_PD_Faith_50.rds"))
butterflies_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_PD_Faith_50.rds"))
mammals_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_PD_Faith_50.rds"))


# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# 3 - b - Plot the importance of broad drivers categories --------------------------

# BIRDS:
cat_imp <- cat.distrib.plot(rf_df = birds_rf,
                            metric_nm = "Faith's PD - Birds",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_withstats_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# REPTILES:
cat_imp <- cat.distrib.plot(rf_df = reptiles_rf,
                            metric_nm = "Faith's PD - Reptiles",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_withstats_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# TREES:
cat_imp <- cat.distrib.plot(rf_df = trees_rf,
                            metric_nm = "Faith's PD - Trees",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_withstats_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# MAMMALS:
cat_imp <- cat.distrib.plot(rf_df = mammals_rf,
                            metric_nm = "Faith's PD - Mammals",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_withstats_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# BUTTERFLIES:
cat_imp <- cat.distrib.plot(rf_df = butterflies_rf,
                            metric_nm = "Faith's PD - Butterflies",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_Faith_withstats_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# Dispersion ===================================================================


# a - Load data ----------------------------------------------------------------

# Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "std_rf_birds_PD_MPD_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_PD_MPD_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "std_rf_trees_PD_MPD_50.rds"))
mammals_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_PD_MPD_50.rds"))
butterflies_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_PD_MPD_50.rds"))

# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# b - Plot the importance of broad drivers categories --------------------------

# BIRDS:
cat_imp <- cat.distrib.plot(rf_df = birds_rf,
                            metric_nm = "MPD PD - Birds",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_withstats_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# REPTILES:
cat_imp <- cat.distrib.plot(rf_df = reptiles_rf,
                            metric_nm = "MPD PD - Reptiles",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_withstats_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# TREES:
cat_imp <- cat.distrib.plot(rf_df = trees_rf,
                            metric_nm = "MPD PD - Trees",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_withstats_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# MAMMALS:
cat_imp <- cat.distrib.plot(rf_df = mammals_rf,
                            metric_nm = "MPD PD - Mammals",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_withstats_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# BUTTERFLIES:
cat_imp <- cat.distrib.plot(rf_df = butterflies_rf,
                            metric_nm = "MPD PD - Butterflies",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MPD_withstats_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# Originality ==================================================================


# a - Load data ----------------------------------------------------------------

# Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "std_rf_birds_PD_MNTD_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_PD_MNTD_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "std_rf_trees_PD_MNTD_50.rds"))
mammals_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_PD_MNTD_50.rds"))
butterflies_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_PD_MNTD_50.rds"))

# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# b - Plot the importance of broad drivers categories --------------------------

# BIRDS:
cat_imp <- cat.distrib.plot(rf_df = birds_rf,
                            metric_nm = "MNTD PD - Birds",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_withstats_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# REPTILES:
cat_imp <- cat.distrib.plot(rf_df = reptiles_rf,
                            metric_nm = "MNTD PD - Reptiles",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_withstats_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# TREES:
cat_imp <- cat.distrib.plot(rf_df = trees_rf,
                            metric_nm = "MNTD PD - Trees",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_withstats_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# MAMMALS:
cat_imp <- cat.distrib.plot(rf_df = mammals_rf,
                            metric_nm = "MNTD PD - Mammals",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_withstats_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
# BUTTERFLIES:
cat_imp <- cat.distrib.plot(rf_df = butterflies_rf,
                            metric_nm = "MNTD PD - Butterflies",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_PD_MNTD_withstats_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# 4 - FD - Violin Plots and test ===============================================


# Richness =====================================================================

# Load data ----------------------------------------------------------------

# Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "std_rf_birds_FD_fric_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_FD_fric_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "std_rf_trees_FD_fric_50.rds"))
butterflies_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_FD_fric_50.rds"))
mammals_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_FD_fric_50.rds"))


# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# 4 - b - Plot the importance of broad drivers categories --------------------------

# BIRDS:
cat_imp <- cat.distrib.plot(rf_df = birds_rf,
                            metric_nm = "FRic FD - Birds",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_withstats_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# REPTILES:
cat_imp <- cat.distrib.plot(rf_df = reptiles_rf,
                            metric_nm = "FRic FD - Reptiles",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_withstats_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# TREES:
cat_imp <- cat.distrib.plot(rf_df = trees_rf,
                            metric_nm = "FRic FD - Trees",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_withstats_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# MAMMALS:
cat_imp <- cat.distrib.plot(rf_df = mammals_rf,
                            metric_nm = "FRic FD - Mammals",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_withstats_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# BUTTERFLIES:
cat_imp <- cat.distrib.plot(rf_df = butterflies_rf,
                            metric_nm = "FRic FD - Butterflies",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fric_withstats_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# Dispersion ===================================================================


# a - Load data ----------------------------------------------------------------

# Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "std_rf_birds_FD_fmpd_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_FD_fmpd_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "std_rf_trees_FD_fmpd_50.rds"))
mammals_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_FD_fmpd_50.rds"))
butterflies_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_FD_fmpd_50.rds"))

# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# b - Plot the importance of broad drivers categories --------------------------

# BIRDS:
cat_imp <- cat.distrib.plot(rf_df = birds_rf,
                            metric_nm = "fmpd FD - Birds",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_withstats_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# REPTILES:
cat_imp <- cat.distrib.plot(rf_df = reptiles_rf,
                            metric_nm = "fmpd FD - Reptiles",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_withstats_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# TREES:
cat_imp <- cat.distrib.plot(rf_df = trees_rf,
                            metric_nm = "fmpd FD - Trees",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_withstats_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# MAMMALS:
cat_imp <- cat.distrib.plot(rf_df = mammals_rf,
                            metric_nm = "fmpd FD - Mammals",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_withstats_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# BUTTERFLIES:
cat_imp <- cat.distrib.plot(rf_df = butterflies_rf,
                            metric_nm = "fmpd FD - Butterflies",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fmpd_withstats_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# Originality ==================================================================


# a - Load data ----------------------------------------------------------------

# Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "std_rf_birds_FD_fori_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_FD_fori_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "std_rf_trees_FD_fori_50.rds"))
mammals_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_FD_fori_50.rds"))
butterflies_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_FD_fori_50.rds"))

# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# b - Plot the importance of broad drivers categories --------------------------

# BIRDS:
cat_imp <- cat.distrib.plot(rf_df = birds_rf,
                            metric_nm = "fori FD - Birds",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_withstats_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# REPTILES:
cat_imp <- cat.distrib.plot(rf_df = reptiles_rf,
                            metric_nm = "fori FD - Reptiles",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_withstats_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# TREES:
cat_imp <- cat.distrib.plot(rf_df = trees_rf,
                            metric_nm = "fori FD - Trees",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_withstats_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# MAMMALS:
cat_imp <- cat.distrib.plot(rf_df = mammals_rf,
                            metric_nm = "fori FD - Mammals",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_withstats_50_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
# BUTTERFLIES:
cat_imp <- cat.distrib.plot(rf_df = butterflies_rf,
                            metric_nm = "fori FD - Butterflies",
                            palette = c("#88CCEE",
                                        "#44AA99",
                                        "#117733",
                                        "#DDCC77",
                                        "#CC6677",
                                        "#882255"),
                            drivers_nm_df = drivers_nm_df)
# Plot categories importance:
cat_imp[[1]]
# Plot with stats:
cat_imp[[2]]

# Save it:
ggplot2::ggsave(plot = cat_imp[[1]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = cat_imp[[2]],
                filename = here::here("outputs",
                                      "catimp_FD_fori_withstats_50_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

