################################################################################
##
## Script to plot per taxa, a circular barplot illustrating the main drivers
## ... of each facet of diversity
##
## Camille Magneville
##
## 04/06/2024 - 09/2024
##
## 11_a_Circular_plots_all_metrics.R
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


# 2 - Build data frame for the circular plot - PD ==============================


# Note: One data frame per taxa, so we can compare drivers between
# ... diversity metrics for each taxa:

# For birds:
rf_df_list <- list("PD Richness" = birds_Faith_rf,
                   "PD Dispersion" = birds_mpd_rf,
                   "PD Originality" = birds_mntd_rf)
rf_plot_birds_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For trees:
rf_df_list <- list("PD Richness" = trees_Faith_rf,
                   "PD Dispersion" = trees_mpd_rf,
                   "PD Originality" = trees_mntd_rf)
rf_plot_trees_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For reptiles:
rf_df_list <- list("PD Richness" = reptiles_Faith_rf,
                   "PD Dispersion" = reptiles_mpd_rf,
                   "PD Originality" = reptiles_mntd_rf)
rf_plot_reptiles_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For mammals:
rf_df_list <- list("PD Richness" = mammals_Faith_rf,
                   "PD Dispersion" = mammals_mpd_rf,
                   "PD Originality" = mammals_mntd_rf)
rf_plot_mammals_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                              var_nb = 10)

# For butterflies:
rf_df_list <- list("PD Richness" = butterflies_Faith_rf,
                   "PD Dispersion" = butterflies_mpd_rf,
                   "PD Originality" = butterflies_mntd_rf)
rf_plot_butterflies_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                                  var_nb = 10)

# 3 - Plot the circular plots - PD =============================================


# For birds: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("#88CCEE",
             "#44AA99",
             "#117733",
             "#DDCC77",
             "#CC6677",
             "#882255")
circular_plot_birds <- circular.drivers.plot(taxa_plot_df = rf_plot_birds_df,
                                             drivers_nm_df = drivers_nm_df,
                                             palette = palette,
                                             div_facet = "PD")
circular_plot_birds

# Save it:
ggplot2::ggsave(plot = circular_plot_birds,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_BIRDS.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_birds,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)


# For reptiles: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("#88CCEE",
             "#44AA99",
             "#117733",
             "#DDCC77",
             "#CC6677",
             "#882255")
circular_plot_reptiles <- circular.drivers.plot(taxa_plot_df = rf_plot_reptiles_df,
                                             drivers_nm_df = drivers_nm_df,
                                             palette = palette,
                                             div_facet = "PD")
circular_plot_reptiles

# Save it:
ggplot2::ggsave(plot = circular_plot_reptiles,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_REPTILES.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_reptiles,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)

# For trees: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
unique(rf_plot_trees_df$Drivers_cat)
palette <- c("#88CCEE",
             "#44AA99",
             "#117733",
             "#DDCC77",
             "#CC6677",
             "#882255")
circular_plot_trees <- circular.drivers.plot(taxa_plot_df = rf_plot_trees_df,
                                             drivers_nm_df = drivers_nm_df,
                                             palette = palette,
                                             div_facet = "PD")
circular_plot_trees

# Save it:
ggplot2::ggsave(plot = circular_plot_trees,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_TREES.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_trees,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_TREES.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)

# For mammals: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
unique(rf_plot_mammals_df$Drivers_cat)
palette <- c("#88CCEE",
             "#44AA99",
             "#DDCC77",
             "#CC6677")
circular_plot_mammals <- circular.drivers.plot(taxa_plot_df = rf_plot_mammals_df,
                                               drivers_nm_df = drivers_nm_df,
                                               palette = palette,
                                               div_facet = "PD")
circular_plot_mammals

# Save it:
ggplot2::ggsave(plot = circular_plot_mammals,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_MAMMALS.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_mammals,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)


# For butterflies: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
unique(rf_plot_butterflies_df$Drivers_cat)
palette <- c("#88CCEE",
             "#44AA99",
             "#DDCC77",
             "#CC6677",
             "#882255")
circular_plot_butterflies <- circular.drivers.plot(taxa_plot_df = rf_plot_butterflies_df,
                                                   drivers_nm_df = drivers_nm_df,
                                                   palette = palette,
                                                   div_facet = "PD")
circular_plot_butterflies

# Save it:
ggplot2::ggsave(plot = circular_plot_butterflies,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_BUTTERFLIES.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_butterflies,
                filename = here::here("outputs",
                                      "circular_plot_50_PD_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)


# 4 - Build data frame for the circular plot - FD ==============================


# Note: One data frame per taxa, so we can compare drivers between
# ... diversity metrics for each taxa:


# For birds:
rf_df_list <- list("FD Richness" = birds_fric_rf,
                   "FD Dispersion" = birds_fmpd_rf,
                   "FD Originality" = birds_fori_rf)
rf_plot_birds_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For reptiles:
rf_df_list <- list("FD Richness" = reptiles_fric_rf,
                   "FD Dispersion" = reptiles_fmpd_rf,
                   "FD Originality" = reptiles_fori_rf)
rf_plot_reptiles_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                               var_nb = 10)

# For trees:
rf_df_list <- list("FD Richness" = trees_fric_rf,
                   "FD Dispersion" = trees_fmpd_rf,
                   "FD Originality" = trees_fori_rf)
rf_plot_trees_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 10)

# For mammals:
rf_df_list <- list("FD Richness" = mammals_fric_rf,
                   "FD Dispersion" = mammals_fmpd_rf,
                   "FD Originality" = mammals_fori_rf)
rf_plot_mammals_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                              var_nb = 10)

# For butterflies:
rf_df_list <- list("FD Richness" = butterflies_fric_rf,
                   "FD Dispersion" = butterflies_fmpd_rf,
                   "FD Originality" = butterflies_fori_rf)
rf_plot_butterflies_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                                  var_nb = 10)


# 5 - Plot the circular plots - FD =============================================


# For birds: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("#88CCEE",
             "#44AA99",
             "#117733",
             "#CC6677",
             "#882255")

circular_plot_birds <- circular.drivers.plot(taxa_plot_df = rf_plot_birds_df,
                                             drivers_nm_df = drivers_nm_df,
                                             palette = palette,
                                             div_facet = "FD")
circular_plot_birds

# Save it:
ggplot2::ggsave(plot = circular_plot_birds,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_BIRDS.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_birds,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)

# For REPTILES: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("#88CCEE",
             "#44AA99",
             "#117733",
             "#DDCC77",
             "#CC6677",
             "#882255")

circular_plot_reptiles <- circular.drivers.plot(taxa_plot_df = rf_plot_reptiles_df,
                                                drivers_nm_df = drivers_nm_df,
                                                palette = palette,
                                                div_facet = "FD")
circular_plot_reptiles

# Save it:
ggplot2::ggsave(plot = circular_plot_reptiles,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_REPTILES.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_reptiles,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)

# For TREES: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("#88CCEE",
             "#44AA99",
             "#117733",
             "#DDCC77",
             "#882255")

circular_plot_trees <- circular.drivers.plot(taxa_plot_df = rf_plot_trees_df,
                                             drivers_nm_df = drivers_nm_df,
                                             palette = palette,
                                             div_facet = "FD")
circular_plot_trees

# Save it:
ggplot2::ggsave(plot = circular_plot_trees,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_TREES.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_trees,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_TREES.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)

# For MAMMALS: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("#88CCEE",
             "#44AA99",
             "#117733",
             "#DDCC77",
             "#CC6677",
             "#882255")

circular_plot_mammals <- circular.drivers.plot(taxa_plot_df = rf_plot_mammals_df,
                                               drivers_nm_df = drivers_nm_df,
                                               palette = palette,
                                               div_facet = "FD")
circular_plot_mammals

# Save it:
ggplot2::ggsave(plot = circular_plot_mammals,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_MAMMALS.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_mammals,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_MAMMALS.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)

# For BUTTERFLIES: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("#88CCEE",
             "#44AA99",
             "#117733",
             "#DDCC77",
             "#CC6677",
             "#882255")

circular_plot_butterflies <- circular.drivers.plot(taxa_plot_df = rf_plot_butterflies_df,
                                                   drivers_nm_df = drivers_nm_df,
                                                   palette = palette,
                                                   div_facet = "FD")
circular_plot_butterflies

# Save it:
ggplot2::ggsave(plot = circular_plot_butterflies,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_BUTTERFLIES.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_butterflies,
                filename = here::here("outputs",
                                      "circular_plot_50_FD_BUTTERFLIES.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
