################################################################################
##
## Script to get number used in th Result section
##
## Camille Magneville
##
## 01/2025
##
## 15_Divers_number_analyses.R
##
################################################################################

# Get mean and sd R2 of random forests =====================================

# Load mean data:

# Birds:
meanr2_rf_birds_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_birds_FD_fmpd_50.rds"))
meanr2_rf_birds_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_birds_FD_fori_50.rds"))
meanr2_rf_birds_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_birds_FD_fric_50.rds"))
meanr2_rf_birds_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_birds_PD_Faith_50.rds"))
meanr2_rf_birds_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                  "meanr2_rf_birds_PD_mntd_50.rds"))
meanr2_rf_birds_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_birds_PD_mpd_50.rds"))
# Butterflies:
meanr2_rf_butterflies_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                       "meanr2_rf_butterflies_FD_fmpd_50.rds"))
meanr2_rf_butterflies_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                       "meanr2_rf_butterflies_FD_fori_50.rds"))
meanr2_rf_butterflies_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                       "meanr2_rf_butterflies_FD_fric_50.rds"))
meanr2_rf_butterflies_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                        "meanr2_rf_butterflies_PD_Faith_50.rds"))
meanr2_rf_butterflies_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                       "meanr2_rf_butterflies_PD_mntd_50.rds"))
meanr2_rf_butterflies_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                      "meanr2_rf_butterflies_PD_mpd_50.rds"))
# Mammals:
meanr2_rf_mammals_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                   "meanr2_rf_mammals_FD_fmpd_50.rds"))
meanr2_rf_mammals_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                   "meanr2_rf_mammals_FD_fori_50.rds"))
meanr2_rf_mammals_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                   "meanr2_rf_mammals_FD_fric_50.rds"))
meanr2_rf_mammals_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                    "meanr2_rf_mammals_PD_Faith_50.rds"))
meanr2_rf_mammals_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                   "meanr2_rf_mammals_PD_mntd_50.rds"))
meanr2_rf_mammals_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                  "meanr2_rf_mammals_PD_mpd_50.rds"))
# Reptiles:
meanr2_rf_reptiles_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                    "meanr2_rf_reptiles_FD_fmpd_50.rds"))
meanr2_rf_reptiles_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                    "meanr2_rf_reptiles_FD_fori_50.rds"))
meanr2_rf_reptiles_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                    "meanr2_rf_reptiles_FD_fric_50.rds"))
meanr2_rf_reptiles_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                     "meanr2_rf_reptiles_PD_Faith_50.rds"))
meanr2_rf_reptiles_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                    "meanr2_rf_reptiles_PD_mntd_50.rds"))
meanr2_rf_reptiles_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                   "meanr2_rf_reptiles_PD_mpd_50.rds"))
# Trees:
meanr2_rf_trees_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_trees_FD_fmpd_50.rds"))
meanr2_rf_trees_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_trees_FD_fori_50.rds"))
meanr2_rf_trees_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_trees_FD_fric_50.rds"))
meanr2_rf_trees_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                  "meanr2_rf_trees_PD_Faith_50.rds"))
meanr2_rf_trees_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                 "meanr2_rf_trees_PD_mntd_50.rds"))
meanr2_rf_trees_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                "meanr2_rf_trees_PD_mpd_50.rds"))

# Load sd data:
# Birds:
sd_meanr2_rf_birds_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                    "sd_meanr2_rf_birds_FD_fmpd_50.rds"))
sd_meanr2_rf_birds_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                    "sd_meanr2_rf_birds_FD_fori_50.rds"))
sd_meanr2_rf_birds_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                    "sd_meanr2_rf_birds_FD_fric_50.rds"))
sd_meanr2_rf_birds_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                     "sd_meanr2_rf_birds_PD_Faith_50.rds"))
sd_meanr2_rf_birds_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                    "sd_meanr2_rf_birds_PD_mntd_50.rds"))
sd_meanr2_rf_birds_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                   "sd_meanr2_rf_birds_PD_mpd_50.rds"))
# Butterflies:
sd_meanr2_rf_butterflies_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                          "sd_meanr2_rf_butterflies_FD_fmpd_50.rds"))
sd_meanr2_rf_butterflies_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                          "sd_meanr2_rf_butterflies_FD_fori_50.rds"))
sd_meanr2_rf_butterflies_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                          "sd_meanr2_rf_butterflies_FD_fric_50.rds"))
sd_meanr2_rf_butterflies_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                           "sd_meanr2_rf_butterflies_PD_Faith_50.rds"))
sd_meanr2_rf_butterflies_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                          "sd_meanr2_rf_butterflies_PD_mntd_50.rds"))
sd_meanr2_rf_butterflies_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                         "sd_meanr2_rf_butterflies_PD_mpd_50.rds"))
# Mammals:
sd_meanr2_rf_mammals_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                      "sd_meanr2_rf_mammals_FD_fmpd_50.rds"))
sd_meanr2_rf_mammals_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                      "sd_meanr2_rf_mammals_FD_fori_50.rds"))
sd_meanr2_rf_mammals_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                      "sd_meanr2_rf_mammals_FD_fric_50.rds"))
sd_meanr2_rf_mammals_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                       "sd_meanr2_rf_mammals_PD_Faith_50.rds"))
sd_meanr2_rf_mammals_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                      "sd_meanr2_rf_mammals_PD_mntd_50.rds"))
sd_meanr2_rf_mammals_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                     "sd_meanr2_rf_mammals_PD_mpd_50.rds"))
# Reptiles:
sd_meanr2_rf_reptiles_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                       "sd_meanr2_rf_reptiles_FD_fmpd_50.rds"))
sd_meanr2_rf_reptiles_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                       "sd_meanr2_rf_reptiles_FD_fori_50.rds"))
sd_meanr2_rf_reptiles_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                       "sd_meanr2_rf_reptiles_FD_fric_50.rds"))
sd_meanr2_rf_reptiles_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                        "sd_meanr2_rf_reptiles_PD_Faith_50.rds"))
sd_meanr2_rf_reptiles_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                       "sd_meanr2_rf_reptiles_PD_mntd_50.rds"))
sd_meanr2_rf_reptiles_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                      "sd_meanr2_rf_reptiles_PD_mpd_50.rds"))
# Trees:
sd_meanr2_rf_trees_FD_fmpd_50 <- readRDS(here::here("transformed_data",
                                                    "sd_meanr2_rf_trees_FD_fmpd_50.rds"))
sd_meanr2_rf_trees_FD_fori_50 <- readRDS(here::here("transformed_data",
                                                    "sd_meanr2_rf_trees_FD_fori_50.rds"))
sd_meanr2_rf_trees_FD_fric_50 <- readRDS(here::here("transformed_data",
                                                    "sd_meanr2_rf_trees_FD_fric_50.rds"))
sd_meanr2_rf_trees_PD_Faith_50 <- readRDS(here::here("transformed_data",
                                                     "sd_meanr2_rf_trees_PD_Faith_50.rds"))
sd_meanr2_rf_trees_PD_mntd_50 <- readRDS(here::here("transformed_data",
                                                    "sd_meanr2_rf_trees_PD_mntd_50.rds"))
sd_meanr2_rf_trees_PD_mpd_50 <- readRDS(here::here("transformed_data",
                                                   "sd_meanr2_rf_trees_PD_mpd_50.rds"))


# Compute correlation between mean relat imp and sd (Figure 3) =================


# Load heatmap values for FD and PD:
heatmap_FD <- readRDS(here::here("transformed_data",
                                 "heatmap_values_FD.rds"))
heatmap_PD <- readRDS(here::here("transformed_data",
                                 "heatmap_values_PD.rds"))

# Compute overall correlation between mean and sd for FD and PD:
cor_FD <- cor.test(heatmap_FD$mean, heatmap_FD$sd, method = c("pearson"))
cor_FD
cor_PD <- cor.test(heatmap_PD$mean, heatmap_PD$sd, method = c("pearson"))
cor_PD

# Compute correlation between herbivory and mammals species richness ===========

# Load mammals species richness:
mammals_richness <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "FD_FRic_50km_MAMMALS.rds"))
mammals_richness_values <- mammals_richness$functional_diversity_indices

# Put Idgrid as a column:
mammals_richness_values <- mammals_richness_values %>%
  tibble::rownames_to_column("Idgrid")

# Load the drivers' df and restrict the env df to herbivory:
env_drivers_df <- readRDS(here::here("transformed_data",
                                     "env_db",
                                     "env_drivers_final_db.rds"))
herb_df <- env_drivers_df %>%
  dplyr::select(c("Idgrid", "HerbCons_sum"))

# Link the two df:
herb_richn_df <- dplyr::right_join(herb_df, mammals_richness_values,
                                   by = "Idgrid")

# Compute correlation between mammals richness and herbivory:
cor.test(herb_richn_df$sp_richn, herb_richn_df$HerbCons_sum,
         method = c("pearson"))
plot(x = herb_richn_df$sp_richn,
     y = herb_richn_df$HerbCons_sum)


# Compute correlation between all drivers ###############


# Load the environmental data:
env_drivers_df <- readRDS(here::here("transformed_data",
                                     "env_db",
                                     "env_drivers_final_restricted_db.rds"))
# Remove Idgrid to compute correlation matrix:
env_drivers_df <- env_drivers_df[, -1]

# Check that all columns are numeric:
sapply(env_drivers_df, is.numeric)

# Put those which are not as numeric:
for (col in c("Depth_mean", "Depth_stdev", "Elv_mean",
              "Elv_stdev", "OC_mean", "OC_stdev",
              "pH_mean", "pH_stdev",
              "Pr_FCon_percentage_percentage",
              "VWC_mean", "VWC_stdev")) {
  env_drivers_df[[col]] <- as.numeric(env_drivers_df[[col]])
}

# Compute correlation matrix:
cor_matrix <- round(cor(env_drivers_df), 1)
colnames(cor_matrix) <- colnames(env_drivers_df)
rownames(cor_matrix) <- colnames(env_drivers_df)

# Plot the correlation matrix:
correl_drivers_plot <- ggcorrplot::ggcorrplot(cor_matrix,
                                              hc.order = TRUE,
                                              type = "lower")+
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7,
                                                     angle = 90,
                                                     hjust = 1),
        axis.text.y = ggplot2::element_text(size = 7))
correl_drivers_plot

# Save the plot:
ggplot2::ggsave(plot = correl_drivers_plot,
                filename = here::here("outputs",
                                      "drivers_correlations.jpg"),
                device = "jpg",
                scale = 1,
                height = 5000,
                width = 5000,
                units = "px",
                dpi = 600)
