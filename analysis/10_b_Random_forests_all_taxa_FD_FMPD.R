################################################################################
##
## Script to compute one random forest with all variables for each taxas
## ... - 5 random forests - to see which variable drive the most FMPD FD.
## ... Also compute partial dependance plots for each driver.
##
## Camille Magneville
##
## 01/07/2024 - 08/2024
##
## 8_e_Random_forests_all_taxa_FD_FMPD.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load environmental data and FD ==================================


# Load environmental drivers (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# Load SES FD - FMPD
fmpd_ses_birds_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FMPD_null_models_metrics_50km_BIRDS.rds"))
fmpd_ses_trees_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_FMPD_null_models_metrics_50km_TREES.rds"))
fmpd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FMPD_null_models_metrics_50km_REPTILES.rds"))
fmpd_ses_mammals_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FMPD_null_models_metrics_50km_MAMMALS.rds"))
fmpd_ses_butterflies_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_FMPD_null_models_metrics_50km_BUTTERFLIES.rds"))

# Load grid data(for locating grid cells):
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)


# 2 - Subset diversity db and link the two databases (diversity + drivers) =====


# Link the two tables (drivers + diversity):
rf_fmpd_birds_df <- dplyr::inner_join(envdriv_full_db,
                                      fmpd_ses_birds_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")
rf_fmpd_trees_df <- dplyr::inner_join(envdriv_full_db,
                                      fmpd_ses_trees_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")
rf_fmpd_reptiles_df <- dplyr::inner_join(envdriv_full_db,
                                         fmpd_ses_reptiles_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")
rf_fmpd_mammals_df <- dplyr::inner_join(envdriv_full_db,
                                        fmpd_ses_mammals_df[, c("Idgrid", "ses")],
                                        by = "Idgrid")
rf_fmpd_butterflies_df <- dplyr::inner_join(envdriv_full_db,
                                        fmpd_ses_butterflies_df[, c("Idgrid", "ses")],
                                        by = "Idgrid")

# Put Idgrid as rownames:
rf_fmpd_birds_df <- rf_fmpd_birds_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_fmpd_trees_df <- rf_fmpd_trees_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_fmpd_reptiles_df <- rf_fmpd_reptiles_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_fmpd_mammals_df <- rf_fmpd_mammals_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_fmpd_butterflies_df <- rf_fmpd_butterflies_df %>%
  tibble::column_to_rownames(var = "Idgrid")


# 3 - Random forest for birds ==================================================


# Check types of variables
str(rf_fmpd_birds_df)

# Check that diversity metric doesn't have NA:
rownames(rf_fmpd_birds_df[which(is.na(rf_fmpd_birds_df$ses) == TRUE), ])

# Remove cells with NA:
rf_fmpd_birds_df <- rf_fmpd_birds_df[which(! is.na(rf_fmpd_birds_df$ses)), ]
# Check which cells are kept:
locate.cells(cell_vect = rownames(rf_fmpd_birds_df),
             grid = grid_50km)

# Change SES from names num to num:
rf_fmpd_birds_df$ses <- as.numeric(rf_fmpd_birds_df$ses)

# Set seed for randomisation:
set.seed(42)

# See if 300 trees and mtry = 17 ok:
# Run the random forest model mtry = 16 and 300 trees:
rf_birds <- randomForest::randomForest(ses~.,
                                       data = rf_fmpd_birds_df,
                                       ntree = 300,
                                       mtry = 17,
                                       importance = TRUE)
# ntree:
plot(rf_birds)

# mtry:
mtry <- randomForest::tuneRF(rf_fmpd_birds_df[-ncol(rf_fmpd_birds_df)],
                             rf_fmpd_birds_df$ses,
                             mtryStart = 17,
                             ntreeTry = 300,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 17 seems ok (after a few tries)


# Compute 100 random forests and mean importance of each variable + ALE plots:
varimp_birds <- test.rf.model(rf_data = rf_fmpd_birds_df,
                              iteration_nb = 100,
                              metric_nm = "FD_fmpd",
                              taxa_nm = "BIRDS",
                              drivers_nm_df = drivers_nm_df,
                              plot = FALSE)
# Variable importance:
varimp_birds[[1]]
# Std Variable importance:
varimp_birds[[2]]
# Mean R-squared:
varimp_birds[[3]]
# Sd R-squared:
varimp_birds[[4]]

# Save variable importance:
saveRDS(varimp_birds[[1]], here::here("transformed_data",
                                      "rf_birds_FD_fmpd_50.rds"))

# Save standardised variable importance:
saveRDS(varimp_birds[[2]], here::here("transformed_data",
                                      "std_rf_birds_FD_fmpd_50.rds"))

# Save mean R2 and sd:
saveRDS(varimp_birds[[3]], here::here("transformed_data",
                                      "meanr2_rf_birds_FD_fmpd_50.rds"))
saveRDS(varimp_birds[[4]], here::here("transformed_data",
                                      "sd_meanr2_rf_birds_FD_fmpd_50.rds"))

# Save residuals:
saveRDS(varimp_birds[[5]], here::here("transformed_data",
                                      "residuals_rf_birds_FD_fmpd_50.rds"))


# Plot variable importance (std importance):
# Variable importance standardised between 0-1: 1 most important ...
# ... and negative values = 0:
varimp_plot_birds <- varimp.plot(varimp_birds[[2]])

# Save it:
ggplot2::ggsave(plot = varimp_plot_birds,
                filename = here::here("outputs",
                                      "varimp_FD_fmpd_50_BIRDS.pdf"),
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)



# 4 - Random forest for reptiles ==================================================


# Check types of variables
str(rf_fmpd_reptiles_df)

# Check that diversity metric doesn't have NA:
rownames(rf_fmpd_reptiles_df[which(is.na(rf_fmpd_reptiles_df$ses) == TRUE), ])

# Remove cells with NA:
rf_fmpd_reptiles_df <- rf_fmpd_reptiles_df[which(! is.na(rf_fmpd_reptiles_df$ses)), ]
# Check which cells are kept:
locate.cells(cell_vect = rownames(rf_fmpd_reptiles_df),
             grid = grid_50km)

# Change SES from names num to num:
rf_fmpd_reptiles_df$ses <- as.numeric(rf_fmpd_reptiles_df$ses)

# Set seed for randomisation:
set.seed(42)

# See if 300 reptiles and mtry = 17 ok:
# Run the random forest model mtry = 16 and 300 reptiles:
rf_reptiles <- randomForest::randomForest(ses~.,
                                          data = rf_fmpd_reptiles_df,
                                          ntree = 300,
                                          mtry = 17,
                                          importance = TRUE)
# ntree:
plot(rf_reptiles)

# mtry:
mtry <- randomForest::tuneRF(rf_fmpd_reptiles_df[-ncol(rf_fmpd_reptiles_df)],
                             rf_fmpd_reptiles_df$ses,
                             mtryStart = 17,
                             ntreeTry = 300,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 17 seems ok (after a few tries)


# Compute 100 random forests and mean importance of each variable + ALE plots:
varimp_reptiles <- test.rf.model(rf_data = rf_fmpd_reptiles_df,
                                 iteration_nb = 100,
                                 metric_nm = "FD_fmpd",
                                 taxa_nm = "REPTILES",
                                 drivers_nm_df = drivers_nm_df,
                                 plot = FALSE)
# Variable importance:
varimp_reptiles[[1]]
# Std Variable importance:
varimp_reptiles[[2]]
# Mean R-squared:
varimp_reptiles[[3]]
# Sd R-squared:
varimp_reptiles[[4]]

# Save variable importance:
saveRDS(varimp_reptiles[[1]], here::here("transformed_data",
                                         "rf_reptiles_FD_fmpd_50.rds"))

# Save standardised variable importance:
saveRDS(varimp_reptiles[[2]], here::here("transformed_data",
                                         "std_rf_reptiles_FD_fmpd_50.rds"))

# Save mean R2 and sd:
saveRDS(varimp_reptiles[[3]], here::here("transformed_data",
                                         "meanr2_rf_reptiles_FD_fmpd_50.rds"))
saveRDS(varimp_reptiles[[4]], here::here("transformed_data",
                                         "sd_meanr2_rf_reptiles_FD_fmpd_50.rds"))

# Save residuals:
saveRDS(varimp_reptiles[[5]], here::here("transformed_data",
                                         "residuals_rf_reptiles_FD_fmpd_50.rds"))


# Plot variable importance (std importance):
# Variable importance standardised between 0-1: 1 most important ...
# ... and negative values = 0:
varimp_plot_reptiles <- varimp.plot(varimp_reptiles[[2]])

# Save it:
ggplot2::ggsave(plot = varimp_plot_reptiles,
                filename = here::here("outputs",
                                      "varimp_FD_fmpd_50_REPTILES.pdf"),
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# 5 - Random forest for trees ==================================================


# Check types of variables
str(rf_fmpd_trees_df)

# Check that diversity metric doesn't have NA:
rownames(rf_fmpd_trees_df[which(is.na(rf_fmpd_trees_df$ses) == TRUE), ])

# Remove cells with NA:
rf_fmpd_trees_df <- rf_fmpd_trees_df[which(! is.na(rf_fmpd_trees_df$ses)), ]
# Check which cells are kept:
locate.cells(cell_vect = rownames(rf_fmpd_trees_df),
             grid = grid_50km)

# Change SES from names num to num:
rf_fmpd_trees_df$ses <- as.numeric(rf_fmpd_trees_df$ses)

# Set seed for randomisation:
set.seed(42)

# See if 300 trees and mtry = 17 ok:
# Run the random forest model mtry = 16 and 300 trees:
rf_trees <- randomForest::randomForest(ses~.,
                                       data = rf_fmpd_trees_df,
                                       ntree = 300,
                                       mtry = 17,
                                       importance = TRUE)
# ntree:
plot(rf_trees)

# mtry:
mtry <- randomForest::tuneRF(rf_fmpd_trees_df[-ncol(rf_fmpd_trees_df)],
                             rf_fmpd_trees_df$ses,
                             mtryStart = 17,
                             ntreeTry = 300,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 17 seems ok (after a few tries)


# Compute 100 random forests and mean importance of each variable + ALE plots:
varimp_trees <- test.rf.model(rf_data = rf_fmpd_trees_df,
                              iteration_nb = 100,
                              metric_nm = "FD_fmpd",
                              taxa_nm = "TREES",
                              drivers_nm_df = drivers_nm_df,
                              plot = FALSE)
# Variable importance:
varimp_trees[[1]]
# Std Variable importance:
varimp_trees[[2]]
# Mean R-squared:
varimp_trees[[3]]
# Sd R-squared:
varimp_trees[[4]]

# Save variable importance:
saveRDS(varimp_trees[[1]], here::here("transformed_data",
                                      "rf_trees_FD_fmpd_50.rds"))

# Save standardised variable importance:
saveRDS(varimp_trees[[2]], here::here("transformed_data",
                                      "std_rf_trees_FD_fmpd_50.rds"))

# Save mean R2 and sd:
saveRDS(varimp_trees[[3]], here::here("transformed_data",
                                      "meanr2_rf_trees_FD_fmpd_50.rds"))
saveRDS(varimp_trees[[4]], here::here("transformed_data",
                                      "sd_meanr2_rf_trees_FD_fmpd_50.rds"))

# Save residuals:
saveRDS(varimp_trees[[5]], here::here("transformed_data",
                                      "residuals_rf_trees_FD_fmpd_50.rds"))


# Plot variable importance (std importance):
# Variable importance standardised between 0-1: 1 most important ...
# ... and negative values = 0:
varimp_plot_trees <- varimp.plot(varimp_trees[[2]])

# Save it:
ggplot2::ggsave(plot = varimp_plot_trees,
                filename = here::here("outputs",
                                      "varimp_FD_fmpd_50_TREES.pdf"),
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# 6 - Random forest for mammals ==================================================


# Check types of variables
str(rf_fmpd_mammals_df)

# Check that diversity metric doesn't have NA:
rownames(rf_fmpd_mammals_df[which(is.na(rf_fmpd_mammals_df$ses) == TRUE), ])

# Remove cells with NA:
rf_fmpd_mammals_df <- rf_fmpd_mammals_df[which(! is.na(rf_fmpd_mammals_df$ses)), ]
# Check which cells are kept:
locate.cells(cell_vect = rownames(rf_fmpd_mammals_df),
             grid = grid_50km)

# Change SES from names num to num:
rf_fmpd_mammals_df$ses <- as.numeric(rf_fmpd_mammals_df$ses)

# Set seed for randomisation:
set.seed(42)

# See if 300 mammals and mtry = 17 ok:
# Run the random forest model mtry = 16 and 300 mammals:
rf_mammals <- randomForest::randomForest(ses~.,
                                         data = rf_fmpd_mammals_df,
                                         ntree = 300,
                                         mtry = 17,
                                         importance = TRUE)
# ntree:
plot(rf_mammals)

# mtry:
mtry <- randomForest::tuneRF(rf_fmpd_mammals_df[-ncol(rf_fmpd_mammals_df)],
                             rf_fmpd_mammals_df$ses,
                             mtryStart = 17,
                             ntreeTry = 300,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 17 seems ok (after a few tries)


# Compute 100 random forests and mean importance of each variable + ALE plots:
varimp_mammals <- test.rf.model(rf_data = rf_fmpd_mammals_df,
                                iteration_nb = 100,
                                metric_nm = "FD_fmpd",
                                taxa_nm = "MAMMALS",
                                drivers_nm_df = drivers_nm_df,
                                plot = FALSE)
# Variable importance:
varimp_mammals[[1]]
# Std Variable importance:
varimp_mammals[[2]]
# Mean R-squared:
varimp_mammals[[3]]
# Sd R-squared:
varimp_mammals[[4]]

# Save variable importance:
saveRDS(varimp_mammals[[1]], here::here("transformed_data",
                                        "rf_mammals_FD_fmpd_50.rds"))

# Save standardised variable importance:
saveRDS(varimp_mammals[[2]], here::here("transformed_data",
                                        "std_rf_mammals_FD_fmpd_50.rds"))

# Save mean R2 and sd:
saveRDS(varimp_mammals[[3]], here::here("transformed_data",
                                        "meanr2_rf_mammals_FD_fmpd_50.rds"))
saveRDS(varimp_mammals[[4]], here::here("transformed_data",
                                        "sd_meanr2_rf_mammals_FD_fmpd_50.rds"))

# Save residuals:
saveRDS(varimp_mammals[[5]], here::here("transformed_data",
                                        "residuals_rf_mammals_FD_fmpd_50.rds"))


# Plot variable importance (std importance):
# Variable importance standardised between 0-1: 1 most important ...
# ... and negative values = 0:
varimp_plot_mammals <- varimp.plot(varimp_mammals[[2]])

# Save it:
ggplot2::ggsave(plot = varimp_plot_mammals,
                filename = here::here("outputs",
                                      "varimp_FD_fmpd_50_MAMMALS.pdf"),
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# 7 - Random forest for butterflies ==================================================


# Check types of variables
str(rf_fmpd_butterflies_df)

# Check that diversity metric doesn't have NA:
rownames(rf_fmpd_butterflies_df[which(is.na(rf_fmpd_butterflies_df$ses) == TRUE), ])

# Remove cells with NA:
rf_fmpd_butterflies_df <- rf_fmpd_butterflies_df[which(! is.na(rf_fmpd_butterflies_df$ses)), ]
# Check which cells are kept:
locate.cells(cell_vect = rownames(rf_fmpd_butterflies_df),
             grid = grid_50km)

# Change SES from names num to num:
rf_fmpd_butterflies_df$ses <- as.numeric(rf_fmpd_butterflies_df$ses)

# Set seed for randomisation:
set.seed(42)

# See if 300 butterflies and mtry = 17 ok:
# Run the random forest model mtry = 16 and 300 butterflies:
rf_butterflies <- randomForest::randomForest(ses~.,
                                             data = rf_fmpd_butterflies_df,
                                             ntree = 300,
                                             mtry = 17,
                                             importance = TRUE)
# ntree:
plot(rf_butterflies)

# mtry:
mtry <- randomForest::tuneRF(rf_fmpd_butterflies_df[-ncol(rf_fmpd_butterflies_df)],
                             rf_fmpd_butterflies_df$ses,
                             mtryStart = 17,
                             ntreeTry = 300,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 17 seems ok (after a few tries)


# Compute 100 random forests and mean importance of each variable + ALE plots:
varimp_butterflies <- test.rf.model(rf_data = rf_fmpd_butterflies_df,
                                    iteration_nb = 100,
                                    metric_nm = "FD_fmpd",
                                    taxa_nm = "BUTTERFLIES",
                                    drivers_nm_df = drivers_nm_df,
                                    plot = FALSE)
# Variable importance:
varimp_butterflies[[1]]
# Std Variable importance:
varimp_butterflies[[2]]
# Mean R-squared:
varimp_butterflies[[3]]
# Sd R-squared:
varimp_butterflies[[4]]

# Save variable importance:
saveRDS(varimp_butterflies[[1]], here::here("transformed_data",
                                            "rf_butterflies_FD_fmpd_50.rds"))

# Save standardised variable importance:
saveRDS(varimp_butterflies[[2]], here::here("transformed_data",
                                            "std_rf_butterflies_FD_fmpd_50.rds"))

# Save mean R2 and sd:
saveRDS(varimp_butterflies[[3]], here::here("transformed_data",
                                            "meanr2_rf_butterflies_FD_fmpd_50.rds"))
saveRDS(varimp_butterflies[[4]], here::here("transformed_data",
                                            "sd_meanr2_rf_butterflies_FD_fmpd_50.rds"))

# Save residuals:
saveRDS(varimp_butterflies[[5]], here::here("transformed_data",
                                            "residuals_rf_butterflies_FD_fmpd_50.rds"))


# Plot variable importance (std importance):
# Variable importance standardised between 0-1: 1 most important ...
# ... and negative values = 0:
varimp_plot_butterflies <- varimp.plot(varimp_butterflies[[2]])

# Save it:
ggplot2::ggsave(plot = varimp_plot_butterflies,
                filename = here::here("outputs",
                                      "varimp_FD_fmpd_50_BUTTERFLIES.pdf"),
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# 6 - Plot a heatmap comparing variables importance across taxa ================


# a - Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "std_rf_birds_FD_fmpd_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "std_rf_reptiles_FD_fmpd_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "std_rf_trees_FD_fmpd_50.rds"))

rf_all_taxa_list <- list("birds_rf" = birds_rf,
                         "reptiles_rf" = reptiles_rf,
                         "trees_rf" = trees_rf)

# Plot and save (only colors, no nb):
FD_heatmap_nonb <- heatmap.varimp(rf_all_taxa_list,
                                  metric_nm = "fmpd PD",
                                  plot_nb = FALSE)
# Plot and save (plot also nb):
FD_heatmap_nb <- heatmap.varimp(rf_all_taxa_list,
                                metric_nm = "fmpd PD",
                                plot_nb = TRUE)


# 7 - Broad categories importance: plot and test ===============================


# a - Load data ----------------------------------------------------------------

# Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "rf_birds_FD_fmpd_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "rf_reptiles_FD_fmpd_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "rf_trees_FD_fmpd_50.rds"))

# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# b - Plot the importance of broad drivers categories --------------------------

# BIRDS:
cat_imp <- cat.distrib.plot(rf_df = birds_rf,
                            metric_nm = "FMPD FD - Birds",
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
                            metric_nm = "FMPD FD - Reptiles",
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
                            metric_nm = "FMPD FD - Trees",
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


# 8 - Direction of the effect for each category ================================

# Note: For each driver's category, we chose the first variables that ...
# ... had the highest importance to study in which direction they are driving
# ... FD fmpd

# Load data --------------------------------------------------------------------

# Load env drivers (with no NA for predictors and only cells which
# .. have values for all the studied taxa)
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_noNA_db.rds"))

# Load SES FD - fmpd:
fmpd_ses_birds_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_fmpd_null_models_metrics_50km_BIRDS.rds"))
fmpd_ses_trees_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_fmpd_null_models_metrics_50km_TREES.rds"))
fmpd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_fmpd_null_models_metrics_50km_REPTILES.rds"))

# Load grid data(for locating grid cells):
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

# Subset diversity db and link the two databases (diversity + drivers) ---------


# NOTE: If joining drivers db and diversity db, the final db would have
# ... 671 rows (grid cells) but for some of these grid cells, we don't have
# ... occurrence data for now : (birds 664 grid cells, reptiles 624 grid cells,
# ... trees 667 grid cells)
# ... SO: Only keep the grid cells for which I have occ information for all taxa
# ... already done for the environmental db (cf 7_Clean_environmental_var.R)

# Load SES FD - fmpd:
fmpd_ses_birds_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_fmpd_null_models_metrics_50km_BIRDS.rds"))
fmpd_ses_trees_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "FD_fmpd_null_models_metrics_50km_TREES.rds"))
fmpd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                           "div_values_null_models",
                                           "FD_fmpd_null_models_metrics_50km_REPTILES.rds"))

# Get the names of the Idgrid to keep (diversity data for all taxa):
cells_ok_birds <- unique(fmpd_ses_birds_df$Idgrid)
cells_ok_reptiles <- unique(fmpd_ses_reptiles_df$Idgrid)
cells_ok_trees <- unique(fmpd_ses_trees_df$Idgrid)
cells_to_keep <- intersect(intersect(cells_ok_birds,
                                     cells_ok_reptiles),
                           cells_ok_trees)

# Only keep these cells in the diversity df:
fmpd_ses_birds_df <- fmpd_ses_birds_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)
fmpd_ses_reptiles_df <- fmpd_ses_reptiles_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)
fmpd_ses_trees_df <- fmpd_ses_trees_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)


# Link the two tables (drivers + diversity):
var_fmpd_birds_df <- dplyr::left_join(envdriv_full_db,
                                      fmpd_ses_birds_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")
var_fmpd_trees_df <- dplyr::left_join(envdriv_full_db,
                                      fmpd_ses_trees_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")
var_fmpd_reptiles_df <- dplyr::left_join(envdriv_full_db,
                                         fmpd_ses_reptiles_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")

# Put Idgrid as rownames:
var_fmpd_birds_df <- var_fmpd_birds_df %>%
  tibble::column_to_rownames(var = "Idgrid")
var_fmpd_trees_df <- var_fmpd_trees_df %>%
  tibble::column_to_rownames(var = "Idgrid")
var_fmpd_reptiles_df <- var_fmpd_reptiles_df %>%
  tibble::column_to_rownames(var = "Idgrid")

# For BIRDS --------------------------------------------------------------------

# Note: The idea is to focus on the n variable(s) that impact the most each
# ... category to see in which direction it impacts diversity values:
# ... simply plot the data and try to fit a linear model
direction_plots <- relationships.plot(ses_var_df = var_fmpd_birds_df,
                                      metric_nm =  "FMPD FD - Birds",
                                      palette =  c("#88CCEE",
                                                   "#44AA99",
                                                   "#117733",
                                                   "#DDCC77",
                                                   "#CC6677",
                                                   "#882255"),
                                      drivers_nm_df = drivers_nm_df)
direction_plots$past_stab
direction_plots$present_hab
direction_plots$present_hab_heterog
direction_plots$disturb
direction_plots$past_lu1
direction_plots$past_lu2
direction_plots$pr_hum_imp1
direction_plots$pr_hum_imp2

# Save them:
ggplot2::ggsave(plot = direction_plots$past_stab,
                filename = here::here("outputs",
                                      "direction_past_stab_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$present_hab,
                filename = here::here("outputs",
                                      "direction_pres_hab_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$present_hab_heterog,
                filename = here::here("outputs",
                                      "direction_pres_hab_het_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$disturb,
                filename = here::here("outputs",
                                      "direction_disturb_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$past_lu1,
                filename = here::here("outputs",
                                      "direction_past_lu1_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$past_lu2,
                filename = here::here("outputs",
                                      "direction_past_lu2_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$pr_hum_imp1,
                filename = here::here("outputs",
                                      "direction_pr_hum_imp1_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$pr_hum_imp2,
                filename = here::here("outputs",
                                      "direction_pr_hum_imp2_FD_fmpd_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# For REPTILES --------------------------------------------------------------------

# Note: The idea is to focus on the n variable(s) that impact the most each
# ... category to see in which direction it impacts diversity values:
# ... simply plot the data and try to fit a linear model
direction_plots <- relationships.plot(ses_var_df = var_fmpd_reptiles_df,
                                      metric_nm =  "FMPD FD - Reptiles",
                                      palette =  c("#88CCEE",
                                                   "#44AA99",
                                                   "#117733",
                                                   "#DDCC77",
                                                   "#CC6677",
                                                   "#882255"),
                                      drivers_nm_df = drivers_nm_df)
direction_plots$past_stab
direction_plots$present_hab
direction_plots$present_hab_heterog
direction_plots$disturb
direction_plots$past_lu1
direction_plots$past_lu2
direction_plots$pr_hum_imp1
direction_plots$pr_hum_imp2

# Save them:
ggplot2::ggsave(plot = direction_plots$past_stab,
                filename = here::here("outputs",
                                      "direction_past_stab_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$present_hab,
                filename = here::here("outputs",
                                      "direction_pres_hab_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$present_hab_heterog,
                filename = here::here("outputs",
                                      "direction_pres_hab_het_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$disturb,
                filename = here::here("outputs",
                                      "direction_disturb_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$past_lu1,
                filename = here::here("outputs",
                                      "direction_past_lu1_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$past_lu2,
                filename = here::here("outputs",
                                      "direction_past_lu2_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$pr_hum_imp1,
                filename = here::here("outputs",
                                      "direction_pr_hum_imp1_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$pr_hum_imp2,
                filename = here::here("outputs",
                                      "direction_pr_hum_imp2_FD_fmpd_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# For TREES --------------------------------------------------------------------

# Note: The idea is to focus on the n variable(s) that impact the most each
# ... category to see in which direction it impacts diversity values:
# ... simply plot the data and try to fit a linear model
direction_plots <- relationships.plot(ses_var_df = var_fmpd_trees_df,
                                      metric_nm =  "FMPD FD - Trees",
                                      palette =  c("#88CCEE",
                                                   "#44AA99",
                                                   "#117733",
                                                   "#DDCC77",
                                                   "#CC6677",
                                                   "#882255"),
                                      drivers_nm_df = drivers_nm_df)
direction_plots$past_stab
direction_plots$present_hab
direction_plots$present_hab_heterog
direction_plots$disturb
direction_plots$past_lu1
direction_plots$past_lu2
direction_plots$pr_hum_imp1
direction_plots$pr_hum_imp2

# Save them:
ggplot2::ggsave(plot = direction_plots$past_stab,
                filename = here::here("outputs",
                                      "direction_past_stab_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$present_hab,
                filename = here::here("outputs",
                                      "direction_pres_hab_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$present_hab_heterog,
                filename = here::here("outputs",
                                      "direction_pres_hab_het_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$disturb,
                filename = here::here("outputs",
                                      "direction_disturb_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$past_lu1,
                filename = here::here("outputs",
                                      "direction_past_lu1_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$past_lu2,
                filename = here::here("outputs",
                                      "direction_past_lu2_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$pr_hum_imp1,
                filename = here::here("outputs",
                                      "direction_pr_hum_imp1_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = direction_plots$pr_hum_imp2,
                filename = here::here("outputs",
                                      "direction_pr_hum_imp2_FD_fmpd_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.8,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)
