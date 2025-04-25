################################################################################
##
## Script to run all the analysis from data collection and cleaning to ...
## ... models and graphs
##
## Camille Magneville
##
## 03/04/2024
##
## make.R
##
################################################################################


# Clean the environnement:
rm(list = ls(all.names = TRUE), envir = .GlobalEnv)

# Install dependencies:
devtools::install_deps()

# Load the functions so make them available for use:
devtools::load_all()

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# Install from local the sobol-MDA package for random forests:
remotes::install_gitlab("DRTI/sobolMDA")
install.packages(path_to_file = here::here("sobolmda-master.tar.gz"),
                            repos = NULL,
                            type = "source")
install.packages(here::here("sobolmda-master.zip"),
                  repos = NULL,
                  type = "source")
devtools::install_gitlab(repo = "drti/sobolmda")

# Load the functions so make them available for use:
source(here::here("R", "Plot_div_maps_fcts.R"))
source(here::here("R", "Drivers_categories_fcts.R"))
source(here::here("R", "Randomforests_fcts.R"))
source(here::here("R", "Main_drivers_effects_fcts.R"))
source(here::here("R", "Check_residuals_and_correlation_fct.R"))
source(here::here("R", "Null_models_fcts.R"))
source(here::here("R", "Check_traits_fcts.R"))
source(here::here("R", "Impute_missing_traits_fcts.R"))
source(here::here("R", "Help_fcts.R"))
source(here::here("R", "Compute_metrics_landuse_fcts.R"))
source(here::here("R", "Compute_div_per_cell_fcts.R"))


## START DATA PREPARATION AND ANAYLSES

# 1 - Open and check the phylogenetic data:
source(here::here("analysis", "1_a_Open_PD_data_TREES.R"))
source(here::here("analysis", "1_b_Open_PD_data_BIRDS.R"))
source(here::here("analysis", "1_c_Open_PD_data_REPTILES.R"))
source(here::here("analysis", "1_d_Open_PD_data_MAMMALS.R"))
source(here::here("analysis", "1_e_Open_PD_data_BUTTERFLIES.R"))

# 2 - Open and check the functional data:
source(here::here("analysis", "2_a_Open_FD_data_TREES.R"))
source(here::here("analysis", "2_b_Open_FD_data_BIRDS.R"))
source(here::here("analysis", "2_c_Open_FD_data_REPTILES.R"))
source(here::here("analysis", "2_d_Open_FD_data_MAMMALS.R"))
source(here::here("analysis", "2_e_Open_FD_data_BUTTERFLIES.R"))

# 3 - Impute new traits based on taxonomic information:
source(here::here("analysis", "3_a_Impute_traits_TREES.R"))
source(here::here("analysis", "3_b_Impute_traits_BIRDS.R"))
source(here::here("analysis", "3_c_Impute_traits_REPTILES.R"))
source(here::here("analysis", "3_d_Impute_traits_MAMMALS.R"))
source(here::here("analysis", "3_e_Impute_traits_BUTTERFLIES.R"))

# 4 - Get a clean db with environmental drivers:
source(here::here("analysis", "4_Clean_environmental_var.R"))

# 5 - Subset the cells on which analysis will be done:
## ... land > 50%, no islands, data on drivers and all taxa:
source(here::here("analysis", "5_Subset_cells_on_which_analyses_done.R"))

# 6 - Run null models for PD, compute SES:
source(here::here("analysis", "6_a_PD_Nulls_models_TREES.R"))
source(here::here("analysis", "6_b_PD_Nulls_models_BIRDS.R"))
source(here::here("analysis", "6_c_PD_Nulls_models_REPTILES.R"))
source(here::here("analysis", "6_d_PD_Nulls_models_MAMMALS.R"))
source(here::here("analysis", "6_e_PD_Nulls_models_BUTTERFLIES.R"))

# 7 - Run null models for FD, compute SES:
source(here::here("analysis", "7_a_PD_Nulls_models_TREES.R"))
source(here::here("analysis", "7_b_PD_Nulls_models_BIRDS.R"))
source(here::here("analysis", "7_c_PD_Nulls_models_REPTILES.R"))
source(here::here("analysis", "7_d_PD_Nulls_models_MAMMALS.R"))
source(here::here("analysis", "7_e_PD_Nulls_models_BUTTERFLIES.R"))

# 8 - Map FD and PD dimensions for all taxa (SES values from null models):
source(here::here("analysis/8_Maps_FD_PD_all_taxas.R"))

# 9 - Run random forests for PD to get drivers' relative importance:
## Richness:
source(here::here("analysis", "9_a_Random_forests_all_taxa_PD_Faith.R"))
## Dispersion:
source(here::here("analysis", "9_b_Random_forests_all_taxa_PD_MPD.R"))
## Originality:
source(here::here("analysis", "9_c_Random_forests_all_taxa_PD_MNTD.R"))

# 10 - Run random forests for FD to get drivers' relative importance:
## Richness:
source(here::here("analysis", "10_a_Random_forests_all_taxa_FD_FRic.R"))
## Dispersion:
source(here::here("analysis", "10_b_Random_forests_all_taxa_FD_FMPD.R"))
## Originality:
source(here::here("analysis", "10_c_Random_forests_all_taxa_FD_FOri.R"))

# 11 - Create circular plots and alluvial plots to represent main drivers:
## Circular plots (finally not included in the manuscript - still interesting):
source(here::here("analysis", "11_a_Circular_plot_all_metrics.R"))
## Alluvial Plot (Figure 4)
source(here::here("analysis", "11_b_Alluvial_plot_all_metrics.R"))

# 12 - Get the effect of the main drivers:
## Contingency tables and mosaic plots (Figure 5+6):
source(here::here("analysis", "12_a_Contingency_tables_main_drivers.R"))
## Regressions (not included in the manuscript as hard to get message - still interesting):
source(here::here("analysis", "12_b_Regressions_main_drivers.R"))

# 13 - Compare the effect of the broad categories of drivers (Figure 3):
source(here::here("analysis", "13_Compare_main_categories.R"))

# 14 - Get maps of certain drivers (in Appendices for instance):
source(here::here("analysis", "14_Maps_drivers.R"))

# 15 - Check residuals from the Random Forests models - spatial autocorrelation?:
source(here::here("analysis", "15_Checks_residuals_and_diversity.R"))

# 16 - Compute diverse numbers needed for writing the results:
source(here::here("analysis", "16_Divers_numbers_analyses.R"))








