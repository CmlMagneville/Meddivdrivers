################################################################################
##
## Script to plot for each facet, an alluvial plot of drivers relative
## ... importance
##
## Camille Magneville
##
## 02/2025
##
## 11_b_Alluvial_plot_all_metrics.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# 1 - Load data ================================================================

# PD - Richness:
birds_Faith_rf <- readRDS(here::here("transformed_data", "std_rf_birds_PD_Faith_50.rds"))
reptiles_Faith_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_PD_Faith_50.rds"))
trees_Faith_rf <- readRDS(here::here("transformed_data", "std_rf_trees_PD_Faith_50.rds"))
mammals_Faith_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_PD_Faith_50.rds"))
butterflies_Faith_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_PD_Faith_50.rds"))

# PD - Originality:
birds_mntd_rf <- readRDS(here::here("transformed_data", "std_rf_birds_PD_mntd_50.rds"))
reptiles_mntd_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_PD_mntd_50.rds"))
trees_mntd_rf <- readRDS(here::here("transformed_data", "std_rf_trees_PD_mntd_50.rds"))
mammals_mntd_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_PD_mntd_50.rds"))
butterflies_mntd_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_PD_mntd_50.rds"))

# PD - Dispersion:
birds_mpd_rf <- readRDS(here::here("transformed_data", "std_rf_birds_PD_mpd_50.rds"))
reptiles_mpd_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_PD_mpd_50.rds"))
trees_mpd_rf <- readRDS(here::here("transformed_data", "std_rf_trees_PD_mpd_50.rds"))
mammals_mpd_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_PD_mpd_50.rds"))
butterflies_mpd_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_PD_mpd_50.rds"))

# FD - Richness:
birds_fric_rf <- readRDS(here::here("transformed_data", "std_rf_birds_FD_FRic_50.rds"))
reptiles_fric_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_FD_FRic_50.rds"))
trees_fric_rf <- readRDS(here::here("transformed_data", "std_rf_trees_FD_FRic_50.rds"))
mammals_fric_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_FD_FRic_50.rds"))
butterflies_fric_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_FD_FRic_50.rds"))


# FD - Originality:
birds_fori_rf <- readRDS(here::here("transformed_data", "std_rf_birds_FD_fori_50.rds"))
reptiles_fori_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_FD_fori_50.rds"))
trees_fori_rf <- readRDS(here::here("transformed_data", "std_rf_trees_FD_fori_50.rds"))
mammals_fori_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_FD_fori_50.rds"))
butterflies_fori_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_FD_fori_50.rds"))


# FD - Dispersion:
birds_fmpd_rf <- readRDS(here::here("transformed_data", "std_rf_birds_FD_fmpd_50.rds"))
reptiles_fmpd_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_FD_FMPD_50.rds"))
trees_fmpd_rf <- readRDS(here::here("transformed_data", "std_rf_trees_FD_FMPD_50.rds"))
mammals_fmpd_rf <- readRDS(here::here("transformed_data", "std_rf_mammals_FD_FMPD_50.rds"))
butterflies_fmpd_rf <- readRDS(here::here("transformed_data", "std_rf_butterflies_FD_FMPD_50.rds"))


# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# 2 - Build data frame for the alluvial plot - PD ==============================

# Note: One data frame per taxa, so we can compare drivers between
# ... diversity metrics for each taxa:

# For birds:
rf_df_list <- list("PD Richness" = birds_Faith_rf,
                   "PD Dispersion" = birds_mpd_rf,
                   "PD Originality" = birds_mntd_rf)
rf_plot_pd_birds_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For trees:
rf_df_list <- list("PD Richness" = trees_Faith_rf,
                   "PD Dispersion" = trees_mpd_rf,
                   "PD Originality" = trees_mntd_rf)
rf_plot_pd_trees_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For reptiles:
rf_df_list <- list("PD Richness" = reptiles_Faith_rf,
                   "PD Dispersion" = reptiles_mpd_rf,
                   "PD Originality" = reptiles_mntd_rf)
rf_plot_pd_reptiles_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                               var_nb = 10)

# For mammals:
rf_df_list <- list("PD Richness" = mammals_Faith_rf,
                   "PD Dispersion" = mammals_mpd_rf,
                   "PD Originality" = mammals_mntd_rf)
rf_plot_pd_mammals_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                              var_nb = 10)

# For butterflies:
rf_df_list <- list("PD Richness" = butterflies_Faith_rf,
                   "PD Dispersion" = butterflies_mpd_rf,
                   "PD Originality" = butterflies_mntd_rf)
rf_plot_pd_butterflies_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                                  var_nb = 10)


# 3 - Build data frame for alluvial plot - FD ==================================

# Note: One data frame per taxa, so we can compare drivers between
# ... diversity metrics for each taxa:

# For birds:
rf_df_list <- list("FD Richness" = birds_fric_rf,
                   "FD Dispersion" = birds_fmpd_rf,
                   "FD Originality" = birds_fori_rf)
rf_plot_fd_birds_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For reptiles:
rf_df_list <- list("FD Richness" = reptiles_fric_rf,
                   "FD Dispersion" = reptiles_fmpd_rf,
                   "FD Originality" = reptiles_fori_rf)
rf_plot_fd_reptiles_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                               var_nb = 10)

# For trees:
rf_df_list <- list("FD Richness" = trees_fric_rf,
                   "FD Dispersion" = trees_fmpd_rf,
                   "FD Originality" = trees_fori_rf)
rf_plot_fd_trees_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For mammals:
rf_df_list <- list("FD Richness" = mammals_fric_rf,
                   "FD Dispersion" = mammals_fmpd_rf,
                   "FD Originality" = mammals_fori_rf)
rf_plot_fd_mammals_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                              var_nb = 10)

# For butterflies:
rf_df_list <- list("FD Richness" = butterflies_fric_rf,
                   "FD Dispersion" = butterflies_fmpd_rf,
                   "FD Originality" = butterflies_fori_rf)
rf_plot_fd_butterflies_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                                  var_nb = 10)


# 5 - Create the alluvial plot for PD ==========================================

# Gather all taxa together:
rf_plot_pd_trees_df$Taxa <- rep("Trees", nrow(rf_plot_pd_trees_df))
rf_plot_pd_butterflies_df$Taxa <- rep("Butterflies",
                                      nrow(rf_plot_pd_butterflies_df))
rf_plot_pd_reptiles_df$Taxa <- rep("Reptiles",
                                   nrow(rf_plot_pd_reptiles_df))
rf_plot_pd_birds_df$Taxa <- rep("Birds", nrow(rf_plot_pd_birds_df))
rf_plot_pd_mammals_df$Taxa <- rep("Mammals", nrow(rf_plot_pd_mammals_df))

alluvial_plot_pd_df <- rbind(rf_plot_pd_trees_df,
                             rf_plot_pd_butterflies_df,
                             rf_plot_pd_reptiles_df,
                             rf_plot_pd_birds_df,
                             rf_plot_pd_mammals_df)

# Only keep the 8 drivers I'm interested in:
alluvial_plot_pd_final_df <- alluvial_plot_pd_df %>%
  dplyr::filter(Drivers_nm %in% c("Past_MAT_sd",
                                  "HerbCons_sum",
                                  "Depth_mean",
                                  "Past_CCVelShortTerm_mean.voccMag",
                                  "Present_MAT_mean",
                                  "Past_CCVelYoungerDryas_mean.voccMag",
                                  "Past_CCVelLGM_mean.voccMag",
                                  "HerbRichn_sum"))

# Remove all rows with relative importance below 50% - ...
# ... remove rows for which a specific driver is not that important to ...
# ... a specific diversity facet:
alluvial_plot_pd_final_df <- alluvial_plot_pd_final_df %>%
  dplyr::filter(mean_imp >= 0.50)

# Rename the Metric column to remove "PD":
alluvial_plot_pd_final_df$Div_metric <- substr(alluvial_plot_pd_final_df$Div_metric,
                                               4, 20)

# Rename the drivers based on their short names:
alluvial_plot_pd_final_df <- dplyr::left_join(x = alluvial_plot_pd_final_df,
                                              y = drivers_nm_df,
                                              by = "Drivers_nm")
alluvial_plot_pd_final_df <- alluvial_plot_pd_final_df %>%
  dplyr::select(-c("Drivers_nm")) %>%
  dplyr::rename(Drivers_nm = Drivers_short_nm)

# Link taxa and dimension:
alluvial_plot_pd_final_df <- dplyr::mutate(alluvial_plot_pd_final_df,
                                           Taxa_Dim = paste(Taxa, Div_metric,
                                                            sep = " "))

# Rename disturbance to "Present Disturbances":
alluvial_plot_pd_final_df$Drivers_cat[which(alluvial_plot_pd_final_df$Drivers_cat == "Disturbances")] <- "Present Disturbances"

# Order the Drivers category column:
cat_order <- c("Past Climate Stability",
               "Present Habitat",
               "Present Disturbances")
alluvial_plot_pd_final_df$Drivers_cat <- factor(alluvial_plot_pd_final_df$Drivers_cat,
                                                levels = cat_order)

# Order the Taxa_Dim column:
taxa_order <- c("Trees Richness",
                "Trees Dispersion",
                "Trees Originality",
                "Butterflies Richness",
                "Butterflies Dispersion",
                "Butterflies Originality",
                "Reptiles Richness",
                "Reptiles Dispersion",
                "Reptiles Originality",
                "Birds Richness",
                "Birds Dispersion",
                "Birds Originality",
                "Mammals Richness",
                "Mammals Dispersion",
                "Mammals Originality")
alluvial_plot_pd_final_df$Taxa_Dim <- factor(alluvial_plot_pd_final_df$Taxa_Dim,
                                             levels = taxa_order)

# Order the drivers:
drivers_order <- c("Past MAT sd",
                   "Cl. Velocity LGM",
                   "Cl. Velocity YD decrease",
                   "Cl. Velocity Holocene",
                   "MAT mean",
                   "Depth mean",
                   "Herbivores Consumption",
                   "Herbivores Richness")
alluvial_plot_pd_final_df$Drivers_nm <- factor(alluvial_plot_pd_final_df$Drivers_nm,
                                               levels = drivers_order)

# Create the alluvial plot:
alluvial_plot_pd <- ggplot2::ggplot(data = alluvial_plot_pd_final_df,
                ggplot2::aes(axis1 = Drivers_nm, axis2 = Taxa_Dim,
                             y = mean_imp)) +
  ggalluvial::geom_alluvium(ggplot2::aes(fill = Drivers_cat),
                            curve_type = "cubic",
                            alpha = 0.6) +
  ggalluvial::geom_stratum(color = "grey40") +
  ggplot2::geom_text(stat = "stratum",
            ggplot2::aes(label = after_stat(stratum)),
            color = "grey40",
            size = 5) +
  ggplot2::scale_x_discrete(limits = c("Drivers", "Diversity dimension"),
                   expand = c(0.15, 0.05)) +
  ggplot2::scale_fill_manual(values = c("#88CCEE",
                                        "#44AA99",
                                        "#DDCC77")) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = guide_legend(title = "Driver's categories")) +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_text(colour = "grey40",
                                                      size = 15),
                 legend.text = ggplot2::element_text(colour = "grey40",
                                                     size = 15),
                 axis.text.x = ggplot2::element_text(colour = "grey40",
                                                     size = 15,
                                                     face = "bold"))

alluvial_plot_pd
# Save it:
ggplot2::ggsave(plot = alluvial_plot_pd,
                filename = here::here("outputs",
                                      "alluvial_plot_50_PD.pdf"),
                device = "pdf",
                scale = 1,
                height = 6000,
                width = 7000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = alluvial_plot_pd,
                filename = here::here("outputs",
                                      "alluvial_plot_50_PD.jpeg"),
                device = "jpeg",
                scale = 1,
                height = 6000,
                width = 7000,
                units = "px",
                dpi = 600)


# 6 - Create the alluvial plot for FD ==========================================

# Gather all taxa together:
rf_plot_fd_trees_df$Taxa <- rep("Trees", nrow(rf_plot_fd_trees_df))
rf_plot_fd_butterflies_df$Taxa <- rep("Butterflies",
                                      nrow(rf_plot_fd_butterflies_df))
rf_plot_fd_reptiles_df$Taxa <- rep("Reptiles",
                                   nrow(rf_plot_fd_reptiles_df))
rf_plot_fd_birds_df$Taxa <- rep("Birds", nrow(rf_plot_fd_birds_df))
rf_plot_fd_mammals_df$Taxa <- rep("Mammals", nrow(rf_plot_fd_mammals_df))

alluvial_plot_fd_df <- rbind(rf_plot_fd_trees_df,
                             rf_plot_fd_butterflies_df,
                             rf_plot_fd_reptiles_df,
                             rf_plot_fd_birds_df,
                             rf_plot_fd_mammals_df)

# Only keep the 9 drivers I'm interested in (over 50%):
alluvial_plot_fd_final_df <- alluvial_plot_fd_df %>%
  dplyr::filter(Drivers_nm %in% c("Past_MAT_sd",
                                  "HerbCons_sum",
                                  "Past_CCVelShortTerm_mean.voccMag",
                                  "Past_CCVelYoungerDryas_mean.voccMag",
                                  "Past_CCVelLGM_mean.voccMag",
                                  "Present_MAT_mean",
                                  "Present_AI_mean",
                                  "Elv_mean",
                                  "Pr_RatePop_2020_mean"))

# Remove all rows with relative importance below 50% - ...
# ... remove rows for which a specific driver is not that important to ...
# ... a specific diversity facet:
alluvial_plot_fd_final_df <- alluvial_plot_fd_final_df %>%
  dplyr::filter(mean_imp >= 0.5)

# Rename the Metric column to remove "FD":
alluvial_plot_fd_final_df$Div_metric <- substr(alluvial_plot_fd_final_df$Div_metric,
                                               4, 20)

# Rename the drivers based on their short names:
alluvial_plot_fd_final_df <- dplyr::left_join(x = alluvial_plot_fd_final_df,
                                              y = drivers_nm_df,
                                              by = "Drivers_nm")
alluvial_plot_fd_final_df <- alluvial_plot_fd_final_df %>%
  dplyr::select(-c("Drivers_nm")) %>%
  dplyr::rename(Drivers_nm = Drivers_short_nm)

# Link taxa and dimension:
alluvial_plot_fd_final_df <- dplyr::mutate(alluvial_plot_fd_final_df,
                                           Taxa_Dim = paste(Taxa, Div_metric,
                                                            sep = " "))

# Rename disturbance to "Present Disturbances" and human impact:
alluvial_plot_fd_final_df$Drivers_cat[which(alluvial_plot_fd_final_df$Drivers_cat == "Disturbances")] <- "Present Disturbances"
alluvial_plot_fd_final_df$Drivers_cat[which(alluvial_plot_fd_final_df$Drivers_cat == "Present Human Direct Impact")] <- "Present Direct Human Impact"

# Order the Drivers category column:
cat_order <- c("Past Climate Stability",
               "Present Habitat",
               "Present Disturbances",
               "Present Direct Human Impact")
alluvial_plot_fd_final_df$Drivers_cat <- factor(alluvial_plot_fd_final_df$Drivers_cat,
                                                levels = cat_order)

# Order the Taxa_Dim column:
taxa_order <- c("Trees Richness",
               "Trees Dispersion",
               "Trees Originality",
               "Butterflies Richness",
               "Butterflies Dispersion",
               "Butterflies Originality",
               "Reptiles Richness",
               "Reptiles Dispersion",
               "Reptiles Originality",
               "Birds Richness",
               "Birds Dispersion",
               "Birds Originality",
               "Mammals Richness",
               "Mammals Dispersion",
               "Mammals Originality")
alluvial_plot_fd_final_df$Taxa_Dim <- factor(alluvial_plot_fd_final_df$Taxa_Dim,
                                                levels = taxa_order)

# Order the drivers:
drivers_order <- c("Past MAT sd",
                   "Cl. Velocity LGM",
                   "Cl. Velocity YD decrease",
                   "Cl. Velocity Holocene",
                   "MAT mean",
                   "AI mean",
                   "Elevation mean",
                   "Herbivores Consumption",
                   "Growth rate pop")
alluvial_plot_fd_final_df$Drivers_nm <- factor(alluvial_plot_fd_final_df$Drivers_nm,
                                               levels = drivers_order)

# Create the alluvial plot:
alluvial_plot_fd <- ggplot2::ggplot(data = alluvial_plot_fd_final_df,
                                    ggplot2::aes(axis1 = Drivers_nm, axis2 = Taxa_Dim,
                                                 y = mean_imp)) +
  ggalluvial::geom_alluvium(ggplot2::aes(fill = Drivers_cat),
                            curve_type = "cubic",
                            alpha = 0.6) +
  ggalluvial::geom_stratum(color = "grey40") +
  ggplot2::geom_text(stat = "stratum",
                     ggplot2::aes(label = after_stat(stratum)),
                     color = "grey40",
                     size = 5) +
  ggplot2::scale_x_discrete(limits = c("Drivers", "Diversity dimension"),
                            expand = c(0.15, 0.05)) +
  ggplot2::scale_fill_manual(values = c("#88CCEE",
                                        "#44AA99",
                                        "#DDCC77",
                                        "#882255")) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = guide_legend(title = "Driver's categories")) +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_text(colour = "grey40",
                                                      size = 15),
                 legend.text = ggplot2::element_text(colour = "grey40",
                                                     size = 15),
                 axis.text.x = ggplot2::element_text(colour = "grey40",
                                                     size = 15,
                                                     face = "bold"))

alluvial_plot_fd
# Save it:
ggplot2::ggsave(plot = alluvial_plot_fd,
                filename = here::here("outputs",
                                      "alluvial_plot_50_FD.pdf"),
                device = "pdf",
                scale = 1,
                height = 6000,
                width = 7000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = alluvial_plot_fd,
                filename = here::here("outputs",
                                      "alluvial_plot_50_FD.jpeg"),
                device = "jpeg",
                scale = 1,
                height = 6000,
                width = 7000,
                units = "px",
                dpi = 600)
