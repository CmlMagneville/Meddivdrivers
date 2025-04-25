################################################################################
##
## Function(s) to do the random forest analyses - n iteration of the rf model to
## ... test its performance
##
## Camille Magneville
##
## 23/05/2024
##
## Randomforest_fcts.R
##
################################################################################



#' Compute n iteration of the random forest model to test its accuracy and
#' variable importance (standardised and untandardised one) and compute
#' ALE Plots.
#'
#' @param rf_data a data frame containing data to run the random forest - ie
#' predictor variables in columns with the last column being the ses of a given
#' diversity metric. This last column must be named \code{ses}.
#'
#' @param iteration_nb the number of time the random forest should be repeated.
#'
#' @param metric_nm a character string referring to the name of the metric studied
#' (used for saving partial dependance files)
#'
#' @param taxa_nm a character string referring to the name of the taxa studied
#' (used for saving partial dependance files)
#'
#' @param plot a TRUE/FALSE value according to whether or not partial dependance
#' are to be plotted and saved (it takes a lot of time)
#'
#' @param  drivers_nm_df a data frame containing shortened names of drivers
#'
#' @return a data frame with variable importance for each rf, another
#' dataframe in the same format with std importance (so that the most
#' important variable has a value of 1 and negative importances equal 0).
#' These two dataframes contains the mean and sd for each variable importance
#' across the n iterations of random forests. The function also returns the
#' mean and sd R squared (explained variance of the model computed on the
#' OOB data). Lastly, the funtion returns ALE plots for each driver studied.
#'
#' @export
#'

test.rf.model <- function(rf_data,
                       iteration_nb,
                       metric_nm,
                       taxa_nm,
                       plot,
                       drivers_nm_df) {

  # Set seed for randomisation:
  set.seed(42)

  # Create one final db that will be returned:
  var_imp_final_df <- as.data.frame(matrix(ncol = 1,
                                           nrow = ncol(rf_data) - 1,
                                           NA))
  var_imp_final_df[, 1] <- colnames(rf_data)[-ncol(rf_data)]
  colnames(var_imp_final_df) <- "drivers"

  # Same data frame but for standardised importance:
  std_var_imp_final_df <- var_imp_final_df

  # create a list that will contains all rf results (from the n iterations):
  rf_models <- vector("list", iteration_nb)

  # create a dataframe that will contain the residuals of each rf (from the n iter):
  rf_resid <- as.data.frame(matrix(ncol = 2,
                                   nrow = nrow(rf_data),
                                   NA))
  rf_resid[, 1] <- rownames(rf_data)
  colnames(rf_resid) <- "Idgrid"

  # create a vector that will contain models R squared:
  rsq_vect <- c()

  # Make sure that rf_data is numerical:
  rf_data <- rf_data %>%
    dplyr::mutate_if(is.character, as.numeric)

  # Running random forest n times:
  for (i in c(1:iteration_nb)) {

    # Split into training and testing sets:
    # data_split <- rsample::initial_split(rf_data,
    #                                      prop = 0.8)
    # train <- rsample::training(data_split)
    # test <- rsample::testing(data_split)

    # Run the rf model:
    rf_mod <- sobolMDA::ranger(ses ~.,
                             data = rf_data,
                             num.trees = 300,
                             importance = 'sobolMDA',
                             mtry = 17)

    # Put the output of the model in the rf vect:
    rf_models[[i]] <- rf_mod

    # Put the residuals of model in the rf vect:
    rf_resid[, i+1] <- rf_data$ses - rf_mod$predictions

    # Get the variables importance:
    var_imp_rf <- rf_mod$variable.importance
    names(var_imp_rf) <- colnames(rf_data)[-ncol(rf_data)]
    var_imp_rf_df <- as.data.frame(var_imp_rf) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename("drivers" = rowname)

    # Standardise the importance between 0 and 1:
    std_var_imp_rf_df <- var_imp_rf_df
    std_var_imp_rf_df$var_imp_rf <- std_var_imp_rf_df$var_imp_rf/max(std_var_imp_rf_df$var_imp_rf)
    std_var_imp_rf_df$var_imp_rf[which(std_var_imp_rf_df$var_imp_rf < 0)] <- 0

    # Get R squared - explained variance computed on OOB dataset:
    Rsq <- rf_mod$r.squared

    # Print the model details:
    print(rf_mod)

    # Update the dataframes and lists:
    # Variable importance:
    var_imp_final_df <- dplyr::left_join(var_imp_final_df,
                                        var_imp_rf_df,
                                        by = "drivers")
    # Standardised variable importance:
    std_var_imp_final_df <- dplyr::left_join(std_var_imp_final_df,
                                         std_var_imp_rf_df,
                                         by = "drivers")

    # R squared:
    rsq_vect <- append(rsq_vect, Rsq)


    # Rename columns and remove NA rows if needed:
    if (i == iteration_nb) {
      colnames(var_imp_final_df)[-1] <- paste0("PropVar", sep = "_",
                                               "rfmod", sep = "_",
                                               c(1:iteration_nb))
      colnames(std_var_imp_final_df)[-1] <- paste0("PropVar", sep = "_",
                                               "rfmod", sep = "_",
                                               c(1:iteration_nb))
    }

    # IF first iteration (first rf model), create an empty list:
    if (i == 1) {
      # It will contain ALE data for each variable:
      ale_list <- list()
    }
    # Compute ALE plots for all the variables (drivers) and the given rf model:
    print(paste0("ALE data being retrived for random forest #",
                 sep = " ",
                 i))
    ale_list <- get.ale.data(rf_mod = rf_mod,
                             rf_data = rf_data,
                             chosen_var = colnames(rf_data)[-ncol(rf_data)],
                             i = i,
                             ale_list = ale_list)

  }

  # Plot the ALE plot (line = mean +- sd):
  if (plot == TRUE) {
    plot.ale(ale_list = ale_list,
             chosen_var = colnames(rf_data)[-ncol(rf_data)],
             metric_nm = metric_nm,
             taxa_nm = taxa_nm,
             drivers_nm_df = drivers_nm_df)
  }

  # For each variable, get the mean importance (also sd) and percentage:
  var_imp_mean_df <- var_imp_final_df %>%
    tibble::column_to_rownames("drivers")
  var_imp_mean_df$mean_imp <- apply(var_imp_mean_df, 1, mean)
  var_imp_mean_df$sd_imp <- apply(var_imp_mean_df, 1, sd)

  # Same for standardised importances:
  std_var_imp_mean_df <- std_var_imp_final_df %>%
    tibble::column_to_rownames("drivers")
  std_var_imp_mean_df$mean_imp <- apply(std_var_imp_mean_df, 1, mean)
  std_var_imp_mean_df$sd_imp <- apply(std_var_imp_mean_df, 1, sd)

  # Get the mean Rsquared among all models:
  mean_Rsq <- mean(rsq_vect)
  sd_Rsq <- sd(rsq_vect)

  return(list(var_imp_mean_df,
              std_var_imp_mean_df,
              mean_Rsq,
              sd_Rsq,
              rf_resid))

}



#' Take a list of models, a predictor variable name, and the training data,
#' then computes and combines the partial dependence plots for the given
#' variable across all models.
#'
#' @param models
#' @param var_name
#' @param data
#'
#' @return
#' @export
#'

plot.partial.dependence <- function(models, var_name, data, iteration_nb) {

  # Generate partial dependence plots for each model and the studied variable:
  partial_plots <- lapply(models, function(model) {
    pdp::partial(object = model, pred.var = var_name, train = data,
                 plot = FALSE)
  })

  # Combine results into a single data frame:
  combined_pd <- do.call(rbind, lapply(partial_plots, function(p) data.frame(p)))
  combined_pd$Iteration <- rep(1:iteration_nb, each = nrow(partial_plots[[1]]))

  return(combined_pd)

}



#' Get data to plot ALE plots (one data for each 100 model)
#'
#' @param rf_mod a given random forest (one of the 100 run)
#' @param rf_data data used to run the random forest
#' @param chosen_var a vector containing the names of the variables
#' for which the ALE plot should be used
#' @param i the iteration number (nuber of the rf model to be studied
#' based on the 100 iterations)
#' @param ale_list the list containing coordinates for ALE plots for
#' all variables (updated at each round of new rf model)
#'
#' @return
#' @export
#'

get.ale.data <- function(rf_mod,
                         rf_data,
                         chosen_var,
                         ale_list,
                         i) {

  # Get the function to make prediction:
  pfun <- function(object, newdata) predict(object, data = newdata)$predictions

  # Create an object which holds holds the rf model and the data to be used:
  model <- iml::Predictor$new(rf_mod, data = rf_data[, -ncol(rf_data)],
                              y = rf_data$ses,
                              predict.fun = pfun)

  # For each variable to be plotted - retrieve ALE data:
  for (var in chosen_var) {

    # Create a dataframe that will contain values to plot ...
    # ... ALE data for the given model (will be updated for each model)
    # ... IF first model to be studied
    if (i == 1) {
      var_coord_df <- as.data.frame(matrix(ncol = 4,
                                           nrow = 1,
                                           NA))
      colnames(var_coord_df) <- c("var_nm",
                                  "rep_nb",
                                  "x",
                                  "y")
    }

    # IF not the first iteration (ie first rf model), retrieve
    # ... the df associated with var which already has values from
    # ... previous ALE plots:
    if (i != 1) {
      var_coord_df <- ale_list[[var]][[1]]
    }

    # Compute the ALE plot for var and given rf model:
    ale_var_data <- iml::FeatureEffects$new(model,
                                            method = "ale",
                                            features = var)
    # Retrieve x and y values and put them in a df with right col nms:
    ale_var_coord <- ale_var_data$results[[1]]
    ale_var_coord_df <- as.data.frame(matrix(ncol = 4,
                                             nrow = nrow(ale_var_coord),
                                             NA))
    colnames(ale_var_coord_df) <- c("var_nm",
                                "rep_nb",
                                "x",
                                "y")
    ale_var_coord_df$var_nm <- rep(var, nrow(ale_var_coord_df))
    ale_var_coord_df$rep_nb <- rep(i, nrow(ale_var_coord_df))
    ale_var_coord_df$x <- ale_var_coord$.borders
    ale_var_coord_df$y <- ale_var_coord$.value

    # Update the coordinates df with data from this new ALE plot:
    var_coord_df <- dplyr::bind_rows(var_coord_df,
                                    ale_var_coord_df)

    # Update the ale_list() with the updated dataframe:
    ale_list[[var]][[1]] <- var_coord_df

  }

  return(ale_list)

}



#' Plot ALE plots for all variables (mean and sd)
#'
#' @param ale_list a list containing for each variable (driver), a dataframe
#' with coordinates of points of the ALE plot
#' @param chosen_var a vector containing the names of all the variables to study
#' @param metric_nm a character string refering to the metric studied
#' @param taxa_nm a character string refering to the taxa studied
#' @param drivers_nm_df a dataframe linking drivers raw names and nice names
#'
#' @return
#' @export
#'
plot.ale <- function(ale_list,
                     chosen_var,
                     metric_nm,
                     taxa_nm,
                     drivers_nm_df) {

  # For each variable (driver):
  for (var in chosen_var) {

    # Retrieve the associated dataframe:
    var_coord_df <- ale_list[[var]][[1]]
    # Remove the first row (NAs):
    var_coord_df <- var_coord_df[-1, ]

    # Compute the mean of y values for each x:
    mean_line_df <- dplyr::group_by(var_coord_df,
                                    x)
    mean_line_df <- dplyr::summarise(mean_line_df,
                                     mean(y))
    colnames(mean_line_df) <- c("x", "mean_y")

    # Compute the sd of y values for each x:
    sd_line_df <- dplyr::group_by(var_coord_df,
                                    x)
    sd_line_df <- dplyr::summarise(sd_line_df,
                                     sd(y))
    colnames(sd_line_df) <- c("x", "sd_y")

    # Join the two df:
    plot_ALE_df <- dplyr::left_join(mean_line_df,
                                    sd_line_df,
                                    by = "x")

    # Retrieve the color of the line based on the variable:
    if (unique(var_coord_df$var_nm) %in% c("Past_CCVelHolocene_mean.voccMag",
                                           "Past_CCVelLGM_mean.voccMag",
                                           "Past_CCVelShortTerm_mean.voccMag",
                                           "Past_CCVelYoungerDryas_mean.voccMag",
                                           "Past_MAT_sd",
                                           "Past_TAP_sd")) {
      line_col <- "#88CCEE"
    }
    if (unique(var_coord_df$var_nm) %in% c("Depth_mean",
                                           "Elv_mean",
                                           "OC_mean",
                                           "pH_mean",
                                           "VWC_mean",
                                           "Present_AI_mean",
                                           "Present_MAT_mean",
                                           "Present_TAP_mean")) {
      line_col <- "#44AA99"
    }
    if (unique(var_coord_df$var_nm) %in% c("Depth_stdev",
                                           "Elv_stdev",
                                           "OC_stdev",
                                           "pH_stdev",
                                           "VWC_stdev",
                                           "Present_AI_stdev",
                                           "Present_MAT_stdev",
                                           "Present_TAP_stdev")) {
      line_col <- "#117733"
    }
    if (unique(var_coord_df$var_nm) %in% c("HerbCons_sum",
                                           "HerbRichn_sum",
                                           "Pr_FInt_2000_2023_mean",
                                           "Pr_FInt_2000_2023_sd",
                                           "Pr_FSurf_2000_2023_pixels")) {
      line_col <- "#DDCC77"
    }
    if (unique(var_coord_df$var_nm) %in% c("Pr_FCon_percentage_percentage",
                                           "Past_Perc_wild_lands_Weighted_Mean",
                                           "Past_Perc_wild_lands_Weighted_Sd",
                                           "Present_Perc_croplands_Weighted_Mean",
                                           "Present_Perc_croplands_Weighted_Sd",
                                           "Present_Perc_dense_settlements_Weighted_Mean",
                                           "Present_Perc_dense_settlements_Weighted_Sd",
                                           "Present_Perc_rangelands_Weighted_Mean",
                                           "Present_Perc_rangelands_Weighted_Sd",
                                           "Present_Perc_seminatural_lands_Weighted_Mean",
                                           "Present_Perc_seminatural_lands_Weighted_Sd",
                                           "Present_Perc_villages_Weighted_Mean",
                                           "Present_Perc_villages_Weighted_Sd",
                                           "Present_Perc_wild_lands_Weighted_Mean",
                                           "Present_Perc_wild_lands_Weighted_Sd",
                                           "Pr_Pop_2020_mean",
                                           "Pr_RatePop_2020_mean")) {
      line_col <- "#CC6677"
    }
    if (unique(var_coord_df$var_nm) %in% c("Past_Perc_croplands_Weighted_Mean",
                                           "Past_Perc_croplands_Weighted_Sd",
                                           "Past_Perc_dense_settlements_Weighted_Mean",
                                           "Past_Perc_dense_settlements_Weighted_Sd",
                                           "Past_Perc_rangelands_Weighted_Mean",
                                           "Past_Perc_rangelands_Weighted_Sd",
                                           "Past_Perc_seminatural_lands_Weighted_Mean",
                                           "Past_Perc_seminatural_lands_Weighted_Sd",
                                           "Past_Perc_villages_Weighted_Mean",
                                           "Past_Perc_villages_Weighted_Sd",
                                           "Past_Perc_wild_lands_Weighted_Mean",
                                           "Past_Perc_wild_lands_Weighted_Sd")) {
      line_col <- "#882255"
    }

    # Get the nice drivers names:
    plot_ALE_df <- plot_ALE_df %>%
      dplyr::mutate(Drivers_nm = var)
    plot_ALE_df <- dplyr::left_join(plot_ALE_df,
                                    drivers_nm_df,
                                    by = "Drivers_nm")

    # Plot the ALE plot for the given variable:
    ALE_var_plot <- ggplot2::ggplot(plot_ALE_df,
                                    ggplot2::aes(x = x,
                                                 y = mean_y)) +
      # add a horizontal line at 0:
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                          color = "grey70",
                          linetype = 2,
                          size = 0.5) +
      # add the confidence interval (+- sd)
      ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_y - sd_y,
                                        ymax = mean_y + sd_y),
                           fill = "grey90",
                           alpha = 0.6) +
      # add the mean line:
      ggplot2::geom_line(color = line_col,
                         size = 1,
                         alpha = 0.7) +
      # style background and grid:
      ggplot2::theme_minimal() +
      # titles:
      ggplot2::xlab(unique(plot_ALE_df$Drivers_short_nm)) +
      ggplot2::ylab("Effect on diversity")

    print(ALE_var_plot)

    ggplot2::ggsave(plot = ALE_var_plot,
                    filename = here::here("outputs",
                                          "ALE_plots",
                                          paste0("ALE", sep = "_",
                                                 var, sep = "_",
                                                 metric_nm, sep = "_",
                                                 "50", sep = "_", taxa_nm,
                                                 ".jpeg")),
                    device = "jpeg",
                    scale = 1.7,
                    height = 1400,
                    width = 1800,
                    units = "px",
                    dpi = 600)
    ggplot2::ggsave(plot = ALE_var_plot,
                    filename = here::here("outputs",
                                          "ALE_plots",
                                          paste0("ALE", sep = "_",
                                                 var, sep = "_",
                                                 metric_nm, sep = "_",
                                                 "50", sep = "_", taxa_nm,
                                                 ".pdf")),
                    device = "pdf",
                    scale = 1.7,
                    height = 1400,
                    width = 1800,
                    units = "px",
                    dpi = 600)

  } # end loop on each variable


}

#' Plot the variable importance for a given taxa and metric (lollipop plot)
#'
#' @param var_imp_df output of the \code{test.rf.model} function.
#'
#' @return a lollipop plot with drivers on columns and mean %IncMSE on x axis
#' with colors referring to drivers category
#'
#' @export
#'

varimp.plot <- function(var_imp_df) {


  # Add a new column that will refer to drivers categories:
  var_imp_plot_df <- var_imp_df
  var_imp_plot_df$cat <- rep("Past Land Use", nrow(var_imp_df))

  # Fill this new column:
  for (i in (1:nrow(var_imp_plot_df))) {

    if (rownames(var_imp_plot_df)[i] %in% c("Past_CCVelHolocene_mean.voccMag",
                                       "Past_CCVelLGM_mean.voccMag",
                                       "Past_CCVelShortTerm_mean.voccMag",
                                       "Past_CCVelYoungerDryas_mean.voccMag",
                                       "Past_MAT_sd",
                                       "Past_TAP_sd"
    )) {
      var_imp_plot_df$cat[i] <- "Past Climate Stability"
    }

    if (rownames(var_imp_plot_df)[i] %in% c( "pH_mean",
                                        "OC_mean",
                                        "Elv_mean",
                                        "Depth_mean",
                                        "VWC_mean",
                                        "Present_AI_mean",
                                        "Present_MAT_mean",
                                        "Present_TAP_mean",
                                        "Pr_FCon_percentage_percentage"
    )) {
      var_imp_plot_df$cat[i] <- "Present Habitat"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Present_AI_stdev",
                                       "Present_MAT_stdev",
                                       "Present_TAP_stdev",
                                       "pH_stdev",
                                       "OC_stdev",
                                       "Elv_stdev",
                                       "Depth_stdev",
                                       "VWC_stdev"
    )) {
      var_imp_plot_df$cat[i] <- "Present Habitat Heterogeneity"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Pr_FInt_2000_2023_mean",
                                       "Pr_FInt_2000_2023_sd",
                                       "Pr_FSurf_2000_2023_pixels",
                                       "HerbCons_sum",
                                       "HerbRichn_sum")) {
      var_imp_plot_df$cat[i] <- "Disturbances"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Past_Perc_croplands_Weighted_Mean",
                                       "Past_Perc_croplands_Weighted_Sd",
                                       "Past_Perc_dense_settlements_Weighted_Mean",
                                       "Past_Perc_dense_settlements_Weighted_Sd",
                                       "Past_Perc_rangelands_Weighted_Mean",
                                       "Past_Perc_rangelands_Weighted_Sd",
                                       "Past_Perc_seminatural_lands_Weighted_Mean",
                                       "Past_Perc_seminatural_lands_Weighted_Sd",
                                       "Past_Perc_villages_Weighted_Mean",
                                       "Past_Perc_villages_Weighted_Sd",
                                       "Past_Perc_wild_lands_Weighted_Mean",
                                       "Past_Perc_wild_lands_Weighted_Sd" )) {
      var_imp_plot_df$cat[i] <- "Past Land Use"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Present_Perc_croplands_Weighted_Mean",
                                       "Present_Perc_croplands_Weighted_Sd",
                                       "Present_Perc_dense_settlements_Weighted_Mean",
                                       "Present_Perc_dense_settlements_Weighted_Sd",
                                       "Present_Perc_rangelands_Weighted_Mean",
                                       "Present_Perc_rangelands_Weighted_Sd",
                                       "Present_Perc_seminatural_lands_Weighted_Mean",
                                       "Present_Perc_seminatural_lands_Weighted_Sd",
                                       "Present_Perc_villages_Weighted_Mean",
                                       "Present_Perc_villages_Weighted_Sd",
                                       "Present_Perc_wild_lands_Weighted_Mean",
                                       "Present_Perc_wild_lands_Weighted_Sd",
                                       "Pr_Pop_2020_mean",
                                       "Pr_RatePop_2020_mean")) {
      var_imp_plot_df$cat[i] <- "Present Human Direct Impact"
    }

  }

  # Add a new column about whether it's sd or mean:
  var_imp_plot_df$var <- rep("sd", nrow(var_imp_df))

  # Fill this new column:
  for (i in (1:nrow(var_imp_plot_df))) {

    if (grepl("mean", rownames(var_imp_plot_df)[i])) {
      var_imp_plot_df$var[i] <- "mean"
    }

  }

  # Put drivers as column:
  var_imp_plot_df <- var_imp_plot_df %>%
    tibble::rownames_to_column("drivers")

  # Order drivers column:
  var_imp_plot_df$cat <- factor(var_imp_plot_df$cat,
                                  levels = c("Past Climate Stability",
                                             "Present Habitat",
                                             "Present Habitat Heterogeneity",
                                             "Disturbances",
                                             "Past Land Use",
                                             "Present Human Direct Impact"))


  # Plot:
  var_plot <- ggpubr::ggdotchart(var_imp_plot_df,
                                 x = "drivers",
                                 y = "mean_imp",
                                 color = "cat",
                                 palette = c("#88CCEE",
                                             "#44AA99",
                                             "#117733",
                                             "#DDCC77",
                                             "#CC6677",
                                             "#882255"),
                                 shape = "var",
                                 sorting = "descending",
                                 rotate = TRUE,
                                 add = "segments",
                                 dot.size = 3.5,
                                 alpha = 0.6) +

    ggplot2::ylim(0, 1) +
    ggplot2::ylab("mean standardised importance over 100 repetitions") +

    ggplot2::theme(ggpubr::theme_pubr()) +

    ggplot2::labs(color = "Drivers category",
                  shape = "Metric")

    print(var_plot)


}



#' Plot a heatmap illustrating the importance of each variable for the three taxa
#'
#' @param rf_all_taxa_list a list containing the result of the
#' \code{test.rf.model} function for all the taxa studied.
#' @param metric_nm a vector containing the name of the studied metric
#' @param plot_nb a TRUE/FALSE value to indicate whether the mean%IncMSE is to
#' be plotted or not on the heatmap
#'
#' @return a heatmap with species on columns and drivers as rows,
#' gathered by type and colors refelecting variable importance.
#'
#' @export
#'

heatmap.varimp <- function(rf_all_taxa_list,
                           metric_nm,
                           plot_nb) {


  # Create a big data frame containing data for all the taxa studied:
  var_imp_df <- as.data.frame(matrix(ncol = 4, nrow = 1, NA))
  colnames(var_imp_df) <- c("Driver_nm", "Taxa", "Driver_cat", "Mean_std_imp")


  # Fill it:
  for (i in c(1:length(rf_all_taxa_list))) {

    # Retrieve the name of taxa studied:
    taxa_nm <- gsub("_rf", "", names(rf_all_taxa_list)[i])

    # Get the associated df, put drivers as a column, select col and new col:
    taxa_rf_df <- rf_all_taxa_list[[i]] %>%
      tibble::rownames_to_column("Driver_nm") %>%
      dplyr::select("Driver_nm", "mean_imp")
    taxa_rf_df$Driver_cat <- rep(NA, nrow(taxa_rf_df))

    # Add drivers category:
    for (j in (1:nrow(taxa_rf_df))) {

      if (taxa_rf_df$Driver_nm[j] %in% c("Past_CCVelHolocene_mean.voccMag",
                                              "Past_CCVelLGM_mean.voccMag",
                                              "Past_CCVelShortTerm_mean.voccMag",
                                              "Past_CCVelYoungerDryas_mean.voccMag",
                                              "Past_MAT_sd",
                                              "Past_TAP_sd")) {
        taxa_rf_df$Driver_cat[j] <- "Past Climate Stability"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Past_CCVelHolocene_mean.voccMag",
                                          "Past_CCVelLGM_mean.voccMag",
                                          "Past_CCVelShortTerm_mean.voccMag",
                                          "Past_CCVelYoungerDryas_mean.voccMag",
                                          "Past_MAT_sd",
                                          "Past_TAP_sd"
      )) {
        taxa_rf_df$Driver_cat[j] <- "Past Climate Stability"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c( "pH_mean",
                                           "OC_mean",
                                           "Elv_mean",
                                           "Depth_mean",
                                           "VWC_mean",
                                           "Present_AI_mean",
                                           "Present_MAT_mean",
                                           "Present_TAP_mean",
                                           "Pr_FCon_percentage_percentage"
      )) {
        taxa_rf_df$Driver_cat[j] <- "Present Habitat"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Present_AI_stdev",
                                          "Present_MAT_stdev",
                                          "Present_TAP_stdev",
                                          "pH_stdev",
                                          "OC_stdev",
                                          "Elv_stdev",
                                          "Depth_stdev",
                                          "VWC_stdev"
      )) {
        taxa_rf_df$Driver_cat[j] <- "Present Habitat Heterogeneity"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Pr_FInt_2000_2023_mean",
                                          "Pr_FInt_2000_2023_sd",
                                          "Pr_FSurf_2000_2023_pixels",
                                          "HerbCons_sum",
                                          "HerbRichn_sum")) {
        taxa_rf_df$Driver_cat[j] <- "Disturbances"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Past_Perc_croplands_Weighted_Mean",
                                          "Past_Perc_croplands_Weighted_Sd",
                                          "Past_Perc_dense_settlements_Weighted_Mean",
                                          "Past_Perc_dense_settlements_Weighted_Sd",
                                          "Past_Perc_rangelands_Weighted_Mean",
                                          "Past_Perc_rangelands_Weighted_Sd",
                                          "Past_Perc_seminatural_lands_Weighted_Mean",
                                          "Past_Perc_seminatural_lands_Weighted_Sd",
                                          "Past_Perc_villages_Weighted_Mean",
                                          "Past_Perc_villages_Weighted_Sd",
                                          "Past_Perc_wild_lands_Weighted_Mean",
                                          "Past_Perc_wild_lands_Weighted_Sd" )) {
        taxa_rf_df$Driver_cat[j] <- "Past Land Use"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Present_Perc_croplands_Weighted_Mean",
                                          "Present_Perc_croplands_Weighted_Sd",
                                          "Present_Perc_dense_settlements_Weighted_Mean",
                                          "Present_Perc_dense_settlements_Weighted_Sd",
                                          "Present_Perc_rangelands_Weighted_Mean",
                                          "Present_Perc_rangelands_Weighted_Sd",
                                          "Present_Perc_seminatural_lands_Weighted_Mean",
                                          "Present_Perc_seminatural_lands_Weighted_Sd",
                                          "Present_Perc_villages_Weighted_Mean",
                                          "Present_Perc_villages_Weighted_Sd",
                                          "Present_Perc_wild_lands_Weighted_Mean",
                                          "Present_Perc_wild_lands_Weighted_Sd",
                                          "Pr_Pop_2020_mean",
                                          "Pr_RatePop_2020_mean")) {
        taxa_rf_df$Driver_cat[j] <- "Present Human Direct Impact"
      }


    } # end loop drivers category


    # Put taxa_rf_df in the right format so can be added to var_imp_df:
    taxa_rf_df$Taxa <- rep(taxa_nm, nrow(taxa_rf_df))
    taxa_rf_df <- taxa_rf_df %>%
      dplyr::rename("Mean_std_imp" = "mean_imp") %>%
      dplyr::select("Driver_nm", "Taxa", "Driver_cat", "Mean_std_imp")

    # Add it to the final df:
    var_imp_df <- rbind(var_imp_df, taxa_rf_df)

  } # end loop on all the taxa


  # Remove the first row which is NA:
  var_imp_df <- var_imp_df[-1, ]

  # Class drivers category/taxa as factor:
  var_imp_df$Driver_cat <- factor(var_imp_df$Driver_cat,
                                  levels = c("Past Climate Stability",
                                             "Present Habitat",
                                             "Present Habitat Heterogeneity",
                                             "Disturbances",
                                             "Past Land Use",
                                             "Present Human Direct Impact"))
  var_imp_df$Taxa <- as.factor(var_imp_df$Taxa)


  # For each driver, compute the mean imp value over all taxa (to order plot):
  mean_impval_taxa <- var_imp_df %>%
    dplyr::group_by(Driver_nm) %>%
    dplyr::summarise(mean_over_taxa = mean(`Mean_std_imp`))

  # Link with the final db:
  var_imp_df <- dplyr::left_join(var_imp_df, mean_impval_taxa)

  # Order mean std var imp per driver per taxa according to mean values over taxa:
  var_imp_df <- dplyr::arrange(var_imp_df,
                               by = desc(mean_over_taxa))


  # Plot the heatmap with numbers:
  if (plot_nb == TRUE) {

    heatmap_plot <- ggplot2::ggplot(data = var_imp_df,
                                    ggplot2::aes(x = `Taxa`,
                                                 y = `Driver_nm`,
                                                 fill = `Mean_std_imp`)) +
      ggplot2::geom_raster() +

      ggplot2::geom_text(ggplot2::aes(label = round(`Mean_std_imp`, 2)), color = "white",
                         size = 3) +

      ggplot2::scale_fill_viridis_c(limits = c(min(var_imp_df$`Mean_std_imp`),
                                               max(var_imp_df$`Mean_std_imp`))) +

      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey83"),
                     panel.grid.major = ggplot2::element_line(colour = "grey83"),
                     legend.title = ggplot2::element_text(size = 10),
                     legend.text = ggplot2::element_text(size = 10),
                     axis.title.x = ggplot2::element_text(colour = "grey55",
                                                          size = 9),
                     strip.text.y = ggplot2::element_text(angle = 0, size = 9,
                                                          colour = "grey55"),
                     strip.background = ggplot2::element_rect(fill = NA,
                                                              colour = NA)) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +

      ggplot2::facet_grid(rows = ggplot2::vars(`Driver_cat`),
                          scales = "free", space = "free_y") +

      ggplot2::ggtitle(metric_nm)


    # Save it:
    ggplot2::ggsave(plot = heatmap_plot,
                    filename = here::here("outputs",
                                          paste0("varimp",
                                                 sep = "_",
                                                 metric_nm,
                                                 sep = "_",
                                                 "alltaxa_nb_50.pdf")),
                    device = "pdf",
                    scale = 0.9,
                    height = 5500,
                    width = 6000,
                    units = "px",
                    dpi = 600)

  }

  # Plot the heatmap without numbers:
  if (plot_nb == FALSE) {

    heatmap_plot <- ggplot2::ggplot(data = var_imp_df,
                                    ggplot2::aes(x = `Taxa`,
                                                 y = `Driver_nm`,
                                                 fill = `Mean_std_imp`)) +
      ggplot2::geom_raster() +

      ggplot2::scale_fill_viridis_c(limits = c(min(var_imp_df$`Mean_std_imp`),
                                               max(var_imp_df$`Mean_std_imp`))) +

      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey83"),
                     panel.grid.major = ggplot2::element_line(colour = "grey83"),
                     legend.title = ggplot2::element_text(size = 10),
                     legend.text = ggplot2::element_text(size = 10),
                     axis.title.x = ggplot2::element_text(colour = "grey55",
                                                          size = 9),
                     strip.text.y = ggplot2::element_text(angle = 0, size = 9,
                                                          colour = "grey55"),
                     strip.background = ggplot2::element_rect(fill = NA,
                                                              colour = NA)) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +

      ggplot2::facet_grid(rows = ggplot2::vars(`Driver_cat`),
                          scales = "free", space = "free_y") +

      ggplot2::ggtitle(metric_nm)


    # Save it:
    ggplot2::ggsave(plot = heatmap_plot,
                    filename = here::here("outputs",
                                          paste0("varimp",
                                                 sep = "_",
                                                 metric_nm,
                                                 sep = "_",
                                                 "alltaxa_50.pdf")),
                    device = "pdf",
                    scale = 0.9,
                    height = 5500,
                    width = 6000,
                    units = "px",
                    dpi = 600)

  }

  print(heatmap_plot)

}



#' Create a data frame to plot the circular plot of drivers for each taxa
#'
#' @param rf_df_list a list containing the rf results from the
#' \code{test.rf.model} function - each element should be given the name of the
#' metric studied.
#'
#' @param var_nb a number referring to the number of variables that should be
#' kept in the graph - variables ordered according to their importance.
#'
#' @return a data frame with the following columns: Driver_nm, Div_metric,
#' Driver_imp, Driv_cat
#'
#' @export
#'

create.df.circular.plot <- function(rf_df_list,
                                    var_nb) {


  # Build the final df that will be the one to return - fill it with first rf df:
  final_df <- rf_df_list[[1]]
  metric_nm <- names(rf_df_list)[1]
  # Remove unused variables and rename column based on metric name:
  final_df <- final_df %>%
    tibble::rownames_to_column("Drivers_nm") %>%
    dplyr::select(c("Drivers_nm", "mean_imp"))
  # Add a new column with the diversity metric name:
  final_df$Div_metric <- rep(metric_nm, nrow(final_df))

  # Add Drivers category:
  final_df$Drivers_cat <- rep("Past Climate Stability", nrow(final_df))
  # Fill this new column:
  for (i in (1:nrow(final_df))) {

    if (final_df$Drivers_nm[i] %in% c("Past_CCVelHolocene_mean.voccMag",
                                     "Past_CCVelLGM_mean.voccMag",
                                     "Past_CCVelShortTerm_mean.voccMag",
                                     "Past_CCVelYoungerDryas_mean.voccMag",
                                     "Past_MAT_sd",
                                     "Past_TAP_sd"
    )) {
      final_df$Drivers_cat[i] <- "Past Climate Stability"
    }

    if (final_df$Drivers_nm[i] %in% c( "pH_mean",
                                      "OC_mean",
                                      "Elv_mean",
                                      "Depth_mean",
                                      "VWC_mean",
                                      "Present_AI_mean",
                                      "Present_MAT_mean",
                                      "Present_TAP_mean",
                                      "Pr_FCon_percentage_percentage"
    )) {
      final_df$Drivers_cat[i] <- "Present Habitat"
    }

    if (final_df$Drivers_nm[i] %in% c("Present_AI_stdev",
                                     "Present_MAT_stdev",
                                     "Present_TAP_stdev",
                                     "pH_stdev",
                                     "OC_stdev",
                                     "Elv_stdev",
                                     "Depth_stdev",
                                     "VWC_stdev"
    )) {
      final_df$Drivers_cat[i] <- "Present Habitat Heterogeneity"
    }

    if (final_df$Drivers_nm[i] %in% c("Pr_FInt_2000_2023_mean",
                                     "Pr_FInt_2000_2023_sd",
                                     "Pr_FSurf_2000_2023_pixels",
                                     "HerbCons_sum",
                                     "HerbRichn_sum")) {
      final_df$Drivers_cat[i] <- "Disturbances"
    }

    if (final_df$Drivers_nm[i] %in% c("Past_Perc_croplands_Weighted_Mean",
                                     "Past_Perc_croplands_Weighted_Sd",
                                     "Past_Perc_dense_settlements_Weighted_Mean",
                                     "Past_Perc_dense_settlements_Weighted_Sd",
                                     "Past_Perc_rangelands_Weighted_Mean",
                                     "Past_Perc_rangelands_Weighted_Sd",
                                     "Past_Perc_seminatural_lands_Weighted_Mean",
                                     "Past_Perc_seminatural_lands_Weighted_Sd",
                                     "Past_Perc_villages_Weighted_Mean",
                                     "Past_Perc_villages_Weighted_Sd",
                                     "Past_Perc_wild_lands_Weighted_Mean",
                                     "Past_Perc_wild_lands_Weighted_Sd" )) {
      final_df$Drivers_cat[i] <- "Past Land Use"
    }

    if (final_df$Drivers_nm[i] %in% c("Present_Perc_croplands_Weighted_Mean",
                                     "Present_Perc_croplands_Weighted_Sd",
                                     "Present_Perc_dense_settlements_Weighted_Mean",
                                     "Present_Perc_dense_settlements_Weighted_Sd",
                                     "Present_Perc_rangelands_Weighted_Mean",
                                     "Present_Perc_rangelands_Weighted_Sd",
                                     "Present_Perc_seminatural_lands_Weighted_Mean",
                                     "Present_Perc_seminatural_lands_Weighted_Sd",
                                     "Present_Perc_villages_Weighted_Mean",
                                     "Present_Perc_villages_Weighted_Sd",
                                     "Present_Perc_wild_lands_Weighted_Mean",
                                     "Present_Perc_wild_lands_Weighted_Sd",
                                     "Pr_Pop_2020_mean",
                                     "Pr_RatePop_2020_mean")) {
      final_df$Drivers_cat[i] <- "Present Human Direct Impact"
    }

  }

  # Only keep the first n variables:
  final_df <- dplyr::arrange(final_df, desc(mean_imp))
  # Only keep the first "var_nb" variables:
  final_df <- final_df[c(1:var_nb), ]


  # Now do a loop on the other data frames (one for each diversity metric left):
  for (j in (2:length(rf_df_list))) {

    # Build a temp df that will be rowbin with the final_df one:
    temp_df <- rf_df_list[[j]]
    metric_nm <- names(rf_df_list)[j]
    # Remove unused variables and rename column based on metric name:
    temp_df <- temp_df %>%
      tibble::rownames_to_column("Drivers_nm") %>%
      dplyr::select(c("Drivers_nm", "mean_imp"))
    # Add a new column with the diversity metric name:
    temp_df$Div_metric <- rep(metric_nm, nrow(temp_df))

    # Add Drivers category:
    temp_df$Drivers_cat <- rep("Past Climate Stability", nrow(temp_df))
    # Fill this new column:
    for (i in (1:nrow(temp_df))) {

      if (temp_df$Drivers_nm[i] %in% c("Past_CCVelHolocene_mean.voccMag",
                                          "Past_CCVelLGM_mean.voccMag",
                                          "Past_CCVelShortTerm_mean.voccMag",
                                          "Past_CCVelYoungerDryas_mean.voccMag",
                                          "Past_MAT_sd",
                                          "Past_TAP_sd"
      )) {
        temp_df$Drivers_cat[i] <- "Past Climate Stability"
      }

      if (temp_df$Drivers_nm[i] %in% c( "pH_mean",
                                           "OC_mean",
                                           "Elv_mean",
                                           "Depth_mean",
                                           "VWC_mean",
                                           "Present_AI_mean",
                                           "Present_MAT_mean",
                                           "Present_TAP_mean",
                                           "Pr_FCon_percentage_percentage"
      )) {
        temp_df$Drivers_cat[i] <- "Present Habitat"
      }

      if (temp_df$Drivers_nm[i] %in% c("Present_AI_stdev",
                                          "Present_MAT_stdev",
                                          "Present_TAP_stdev",
                                          "pH_stdev",
                                          "OC_stdev",
                                          "Elv_stdev",
                                          "Depth_stdev",
                                          "VWC_stdev"
      )) {
        temp_df$Drivers_cat[i] <- "Present Habitat Heterogeneity"
      }

      if (temp_df$Drivers_nm[i] %in% c("Pr_FInt_2000_2023_mean",
                                          "Pr_FInt_2000_2023_sd",
                                          "Pr_FSurf_2000_2023_pixels",
                                          "HerbCons_sum",
                                          "HerbRichn_sum")) {
        temp_df$Drivers_cat[i] <- "Disturbances"
      }

      if (temp_df$Drivers_nm[i] %in% c("Past_Perc_croplands_Weighted_Mean",
                                          "Past_Perc_croplands_Weighted_Sd",
                                          "Past_Perc_dense_settlements_Weighted_Mean",
                                          "Past_Perc_dense_settlements_Weighted_Sd",
                                          "Past_Perc_rangelands_Weighted_Mean",
                                          "Past_Perc_rangelands_Weighted_Sd",
                                          "Past_Perc_seminatural_lands_Weighted_Mean",
                                          "Past_Perc_seminatural_lands_Weighted_Sd",
                                          "Past_Perc_villages_Weighted_Mean",
                                          "Past_Perc_villages_Weighted_Sd",
                                          "Past_Perc_wild_lands_Weighted_Mean",
                                          "Past_Perc_wild_lands_Weighted_Sd" )) {
        temp_df$Drivers_cat[i] <- "Past Land Use"
      }

      if (temp_df$Drivers_nm[i] %in% c("Present_Perc_croplands_Weighted_Mean",
                                          "Present_Perc_croplands_Weighted_Sd",
                                          "Present_Perc_dense_settlements_Weighted_Mean",
                                          "Present_Perc_dense_settlements_Weighted_Sd",
                                          "Present_Perc_rangelands_Weighted_Mean",
                                          "Present_Perc_rangelands_Weighted_Sd",
                                          "Present_Perc_seminatural_lands_Weighted_Mean",
                                          "Present_Perc_seminatural_lands_Weighted_Sd",
                                          "Present_Perc_villages_Weighted_Mean",
                                          "Present_Perc_villages_Weighted_Sd",
                                          "Present_Perc_wild_lands_Weighted_Mean",
                                          "Present_Perc_wild_lands_Weighted_Sd",
                                          "Pr_Pop_2020_mean",
                                          "Pr_RatePop_2020_mean")) {
        temp_df$Drivers_cat[i] <- "Present Human Direct Impact"
      }

    }

    # Only keep the first n variables:
    temp_df <- dplyr::arrange(temp_df, desc(mean_imp))
    # Only keep the first "var_nb" variables:
    temp_df <- temp_df[c(1:var_nb), ]

    final_df <- rbind(final_df, temp_df)

  } # loop on the diversity df

  return(final_df)

}




#' Plot n drivers for a given taxa and all diversity metrics
#'
#' @param taxa_plot_df data frame from the \code{create.df.circular.plot} function
#' @param drivers_nm_df a data frame containing shortened names of drivers
#' @param palette a color palette containing the color names of the category present
#' in the \code{taxa_plot_df}
#' @param div_facet a character string referring to whether FD of PD is studied here.
#' It could be either \code{"FD"} or \code{"PD"}.
#'
#' @return
#'
#' @export
#'


circular.drivers.plot <- function(taxa_plot_df,
                                  drivers_nm_df,
                                  palette,
                                  div_facet) {


  # Set classes:
  taxa_plot_df$Div_metric <- as.factor(taxa_plot_df$Div_metric)
  taxa_plot_df$Drivers_cat <- as.factor(taxa_plot_df$Drivers_cat)

  # Order drivers column:
  taxa_plot_df$Drivers_cat <- factor(taxa_plot_df$Drivers_cat,
                                     levels = c("Past Climate Stability",
                                                "Present Habitat",
                                                "Present Habitat Heterogeneity",
                                                "Disturbances",
                                                "Past Land Use",
                                                "Present Human Direct Impact"))

  # Decreasing variable importance:
  taxa_plot_df <- taxa_plot_df %>%
    dplyr::arrange(dplyr::desc(mean_imp))

  # Add shortened names of drivers:
  taxa_plot_df <- dplyr::left_join(taxa_plot_df,
                                   drivers_nm_df,
                                   by = "Drivers_nm")

  # Set a number of 'empty bars' to add at the end of each group:
  empty_bar <- 3
  to_add <- data.frame(matrix(NA, empty_bar*nlevels(taxa_plot_df$Div_metric),
                              ncol(taxa_plot_df)))
  colnames(to_add) <- colnames(taxa_plot_df)
  to_add$Div_metric <- rep(levels(taxa_plot_df$Div_metric), each = empty_bar)
  taxa_plot_df <- rbind(taxa_plot_df, to_add)
  taxa_plot_df <- taxa_plot_df %>%
    dplyr::arrange(Div_metric)
  taxa_plot_df$ind <- seq(1, nrow(taxa_plot_df))


  # Get the name and the y position of each label
  label_data <- taxa_plot_df
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$ind-0.5)/number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)


  # Prepare a data frame for base lines
  base_data <- taxa_plot_df %>%
    dplyr::group_by(Div_metric) %>%
    dplyr::summarize(start = min(ind), end = max(ind) - empty_bar) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title = mean(c(start, end)))

  # Prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]

  # Prepare a dataframe for metrics labels
  if (div_facet == "PD") {
    data_labmetric <- data.frame(x = c(8, 26, 44),
                                 y = c(-0.3, -0.3, -0.3),
                                 label = c("PD Dispersion",
                                           "PD Originality",
                                           "PD Richness"))
  }
  if (div_facet == "FD") {
    data_labmetric <- data.frame(x = c(8, 26, 44),
                                 y = c(-0.3, -0.3, -0.3),
                                 label = c("FD Dispersion",
                                           "FD Originality",
                                           "FD Richness"))
  }




  # Make the plot
  drivers_circ_plot <- ggplot2::ggplot(data = taxa_plot_df,
                                       ggplot2::aes(x = as.factor(ind),
                                                    y = mean_imp,
                                                    fill = Div_metric)) +

    ggplot2::geom_bar(ggplot2::aes(x = as.factor(ind),
                                   y = mean_imp,
                                   fill = Drivers_cat),
                      stat = "identity", alpha = 0.5) +

    ggplot2::scale_fill_manual(values = palette) +

    # Add a lines. I do it at the beginning to make sure barplots are OVER it.
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = end, y = 0.5, xend = start, yend = 0.5),
                          colour = "grey", alpha = 1, size = 0.3 ,
                          inherit.aes = FALSE ) +
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = end, y = 1, xend = start, yend = 1),
                          colour = "grey", alpha = 1, size = 0.3 ,
                          inherit.aes = FALSE ) +

    # Add text showing the value of lines
    ggplot2::annotate("text", x = c(max(taxa_plot_df$ind), max(taxa_plot_df$ind)),
                      y = c(0.5, 1),
                      label = c("0.5", "1"), color = "grey",
                      size = 3, angle = 0, fontface = "bold", hjust = 1) +

    ggplot2::ylim(-2,1.5) +

    ggplot2::labs(fill = "Drivers category") +

    ggplot2::theme_minimal() +

    ggplot2::theme(
      legend.position = "none",
      legend.text = ggplot2::element_text(size = 7),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(rep(-1,4), "cm")) +

    ggplot2::coord_polar() +

    ggplot2::geom_text(data = label_data, ggplot2::aes(x = ind,
                                                       y = mean_imp + 0.1,
                                                       label = Drivers_short_nm,
                                                       hjust = hjust),
                       color = "black", alpha = 0.6, fontface = "bold",
                       size = 2.5, angle = label_data$angle,
                       inherit.aes = FALSE) +

    # Add metrics retangles and name:
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 16,
                                    ymin = -0.5, ymax = -0.05),
                       fill = "grey80",
                       alpha = 0.7,
                       color = "white",
                       size = 2) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 18, xmax = 34,
                                  ymin = -0.5, ymax = -0.05),
                      fill = "grey80",
                      alpha = 0.7,
                      color = "white",
                      size = 2) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 36, xmax = 52,
                                    ymin = -0.5, ymax = -0.05),
                      fill = "grey80",
                      alpha = 0.7,
                      color = "white",
                      size = 2) +

    geomtextpath::geom_textpath(data = data_labmetric,
                                ggplot2::aes(x = x, y = y, label = label),
                                size = 4,
                                color = "white",
                                fontface = "bold",
                                inherit.aes = FALSE)

  return(drivers_circ_plot)

}




#' Plot six groups of relationships between drivers and diversity - one group per
#' category of drivers
#'
#' @param ses_var_df the data frame for each cell, values of each driver and
#' value of studied SES.
#' @param metric_nm character string referring to the studied metric
#' @param palette a color palette containing the color names of the broad
#' categories of the drivers present in the \code{taxa_plot_df}
#' @param drivers_nm_df a data frame containing shortened names of drivers
#'
#' @return eight ggplot objects in a list - in each object are represented the
#' relationships between diversity facets and specific drivers belonging to a
#' given category (but 2 plots for past land use and present human impact as
#' too many variables otherwise), each individual driver being represented as a
#' ggplot2 facet
#'
#' @export
#'

relationships.plot <- function(ses_var_df,
                               metric_nm,
                               palette,
                               drivers_nm_df) {

  # Make sure each variable is numeric:
  ses_var_df <- ses_var_df %>%
    dplyr::mutate_if(is.character,as.numeric) %>%
    dplyr::mutate_if(is.double, as.numeric)

  # Format longer the rf data frame:
  ses_longer_df <- ses_var_df %>%
    tibble::rownames_to_column(var = "Idgrid") %>%
    tidyr::pivot_longer(cols = colnames(ses_var_df)[-ncol(ses_var_df)],
                        names_to = "Drivers_nm",
                        values_to = "Drivers_value")


  # Now add the broad category for each driver:
  ses_plot_df <- ses_longer_df
  ses_plot_df$Drivers_cat <- rep(NA, nrow(ses_plot_df))

  # Fill this Category column:
  for (i in (1:nrow(ses_plot_df))) {


    if (ses_plot_df$Drivers_nm[i] %in% c("Past_CCVelHolocene_mean.voccMag",
                                        "Past_CCVelLGM_mean.voccMag",
                                        "Past_CCVelShortTerm_mean.voccMag",
                                        "Past_CCVelYoungerDryas_mean.voccMag",
                                        "Past_MAT_sd",
                                        "Past_TAP_sd"
    )) {
      ses_plot_df$Drivers_cat[i] <- "Past Climate Stability"
    }

    if (ses_plot_df$Drivers_nm[i] %in% c( "pH_mean",
                                         "OC_mean",
                                         "Elv_mean",
                                         "Depth_mean",
                                         "VWC_mean",
                                         "Present_AI_mean",
                                         "Present_MAT_mean",
                                         "Present_TAP_mean",
                                         "Pr_FCon_percentage_percentage"
    )) {
      ses_plot_df$Drivers_cat[i] <- "Present Habitat"
    }

    if (ses_plot_df$Drivers_nm[i] %in% c("Present_AI_stdev",
                                        "Present_MAT_stdev",
                                        "Present_TAP_stdev",
                                        "pH_stdev",
                                        "OC_stdev",
                                        "Elv_stdev",
                                        "Depth_stdev",
                                        "VWC_stdev"
    )) {
      ses_plot_df$Drivers_cat[i] <- "Present Habitat Heterogeneity"
    }

    if (ses_plot_df$Drivers_nm[i] %in% c("Pr_FInt_2000_2023_mean",
                                        "Pr_FInt_2000_2023_sd",
                                        "Pr_FSurf_2000_2023_pixels",
                                        "HerbCons_sum",
                                        "HerbRichn_sum")) {
      ses_plot_df$Drivers_cat[i] <- "Disturbances"
    }

    if (ses_plot_df$Drivers_nm[i] %in% c("Past_Perc_croplands_Weighted_Mean",
                                        "Past_Perc_croplands_Weighted_Sd",
                                        "Past_Perc_dense_settlements_Weighted_Mean",
                                        "Past_Perc_dense_settlements_Weighted_Sd",
                                        "Past_Perc_rangelands_Weighted_Mean",
                                        "Past_Perc_rangelands_Weighted_Sd",
                                        "Past_Perc_seminatural_lands_Weighted_Mean",
                                        "Past_Perc_seminatural_lands_Weighted_Sd",
                                        "Past_Perc_villages_Weighted_Mean",
                                        "Past_Perc_villages_Weighted_Sd",
                                        "Past_Perc_wild_lands_Weighted_Mean",
                                        "Past_Perc_wild_lands_Weighted_Sd" )) {
      ses_plot_df$Drivers_cat[i] <- "Past Land Use"
    }

    if (ses_plot_df$Drivers_nm[i] %in% c("Present_Perc_croplands_Weighted_Mean",
                                        "Present_Perc_croplands_Weighted_Sd",
                                        "Present_Perc_dense_settlements_Weighted_Mean",
                                        "Present_Perc_dense_settlements_Weighted_Sd",
                                        "Present_Perc_rangelands_Weighted_Mean",
                                        "Present_Perc_rangelands_Weighted_Sd",
                                        "Present_Perc_seminatural_lands_Weighted_Mean",
                                        "Present_Perc_seminatural_lands_Weighted_Sd",
                                        "Present_Perc_villages_Weighted_Mean",
                                        "Present_Perc_villages_Weighted_Sd",
                                        "Present_Perc_wild_lands_Weighted_Mean",
                                        "Present_Perc_wild_lands_Weighted_Sd",
                                        "Pr_Pop_2020_mean",
                                        "Pr_RatePop_2020_mean")) {
      ses_plot_df$Drivers_cat[i] <- "Present Human Direct Impact"
    }

  }

  # Add shortened names of drivers and remove old ones:
  ses_plot_df <- dplyr::left_join(ses_plot_df,
                                   drivers_nm_df,
                                   by = "Drivers_nm") %>%
    dplyr::select(-c(Drivers_nm)) %>%
    dplyr::rename(Drivers_nm = Drivers_short_nm)

  # Order drivers column:
  ses_plot_df$Drivers_cat <- factor(ses_plot_df$Drivers_cat,
                                   levels = c("Past Climate Stability",
                                              "Present Habitat",
                                              "Present Habitat Heterogeneity",
                                              "Disturbances",
                                              "Past Land Use",
                                              "Present Human Direct Impact"))

  # Plot smooth line for each driver - category past climate stability:
  past_stability_plot <- ggplot2::ggplot(data = ses_plot_df[which(ses_plot_df$Drivers_cat == "Past Climate Stability"), ]) +

    ggplot2::geom_point(ggplot2::aes(x = Drivers_value,
                                     y = ses),
                        color = "grey80",
                        alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = Drivers_value,
                                      y = ses),
                         color = palette[1],
                         fill = "grey60",
                         method = "lm") +

    ggplot2::facet_grid(cols = ggplot2::vars(Drivers_nm),
                        scales = "free")  +

    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::ggtitle(metric_nm)
  past_stability_plot


  # Plot smooth line for each driver - category present habitat:
  present_hab_plot <- ggplot2::ggplot(data = ses_plot_df[which(ses_plot_df$Drivers_cat == "Present Habitat"), ]) +

    ggplot2::geom_point(ggplot2::aes(x = Drivers_value,
                                     y = ses),
                        color = "grey80",
                        alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = Drivers_value,
                                      y = ses),
                         color = palette[2],
                         fill = "grey60",
                         method = "lm") +

    ggplot2::facet_grid(cols = ggplot2::vars(Drivers_nm),
                        scales = "free") +

    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::ggtitle(metric_nm)
  present_hab_plot


  # Plot smooth line for each driver - category present habitat heterogeneity:
  present_hab_heterog_plot <- ggplot2::ggplot(data = ses_plot_df[which(ses_plot_df$Drivers_cat == "Present Habitat Heterogeneity"), ]) +

    ggplot2::geom_point(ggplot2::aes(x = Drivers_value,
                                     y = ses),
                        color = "grey80",
                        alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = Drivers_value,
                                      y = ses),
                         color = palette[3],
                         fill = "grey60",
                         method = "lm") +

    ggplot2::facet_grid(cols = ggplot2::vars(Drivers_nm),
                        scales = "free") +

    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::ggtitle(metric_nm)
  present_hab_heterog_plot

  # Plot smooth line for each driver - category disturb:
  disturb_plot <- ggplot2::ggplot(data = ses_plot_df[which(ses_plot_df$Drivers_cat == "Disturbances"), ]) +

    ggplot2::geom_point(ggplot2::aes(x = Drivers_value,
                                     y = ses),
                        color = "grey80",
                        alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = Drivers_value,
                                      y = ses),
                         color = palette[4],
                         fill = "grey60",
                         method = "lm") +

    ggplot2::facet_grid(cols = ggplot2::vars(Drivers_nm),
                        scales = "free") +

    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::ggtitle(metric_nm)
  disturb_plot

  # Plot smooth line for each driver - category past land use:
  past_lu_plot1 <- ggplot2::ggplot(data = ses_plot_df[which(ses_plot_df$Drivers_cat == "Past Land Use" &
                                                            ses_plot_df$Drivers_nm %in%
                                                              c("Past % croplands mean",
                                                                "Past % settlements mean",
                                                                "Past % rangelands mean",
                                                                "Past % seminat mean",
                                                                "Past % villages mean",
                                                                "Past % wildlands mean")), ]) +

    ggplot2::geom_point(ggplot2::aes(x = Drivers_value,
                                     y = ses),
                        color = "grey80",
                        alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = Drivers_value,
                                      y = ses),
                         color = palette[5],
                         fill = "grey60",
                         method = "lm") +

    ggplot2::facet_grid(cols = ggplot2::vars(Drivers_nm),
                        scales = "free") +

    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::ggtitle(metric_nm)
  past_lu_plot1

  past_lu_plot2 <- ggplot2::ggplot(data = ses_plot_df[which(ses_plot_df$Drivers_cat == "Past Land Use" &
                                                              ses_plot_df$Drivers_nm %in%
                                                              c("Past % croplands sd",
                                                                "Past % settlements sd",
                                                                "Past % rangelands sd",
                                                                "Past % seminat sd",
                                                                "Past % villages sd",
                                                                "Past % wildlands sd")), ]) +

    ggplot2::geom_point(ggplot2::aes(x = Drivers_value,
                                     y = ses),
                        color = "grey80",
                        alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = Drivers_value,
                                      y = ses),
                         color = palette[5],
                         fill = "grey60",
                         method = "lm") +

    ggplot2::facet_grid(cols = ggplot2::vars(Drivers_nm),
                        scales = "free") +

    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::ggtitle(metric_nm)
  past_lu_plot2

  # Plot smooth line for each driver - category past land use:
  pr_hum_plot1 <- ggplot2::ggplot(data = ses_plot_df[which(ses_plot_df$Drivers_cat == "Present Human Direct Impact" &
                                                              ses_plot_df$Drivers_nm %in%
                                                              c("Pres. % croplands mean",
                                                                "Pres.% settlements mean",
                                                                "Pres.% rangelands mean",
                                                                "Pres.% seminat mean",
                                                                "Pres.% villages mean",
                                                                "Pres.% wildlands mean",
                                                                "Human pop ")), ]) +

    ggplot2::geom_point(ggplot2::aes(x = Drivers_value,
                                     y = ses),
                        color = "grey80",
                        alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = Drivers_value,
                                      y = ses),
                         color = palette[6],
                         fill = "grey60",
                         method = "lm") +

    ggplot2::facet_grid(cols = ggplot2::vars(Drivers_nm),
                        scales = "free") +

    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::ggtitle(metric_nm)
  pr_hum_plot1

  pr_hum_plot2 <- ggplot2::ggplot(data = ses_plot_df[which(ses_plot_df$Drivers_cat == "Present Human Direct Impact" &
                                                             ses_plot_df$Drivers_nm %in%
                                                             c("Pres.% croplands sd",
                                                               "Pres.% settlements sd",
                                                               "Pres.% rangelands sd",
                                                               "Pres.% seminat sd",
                                                               "Pres.% villages sd",
                                                               "Pres.% wildlands sd",
                                                               "Growth rate pop")), ]) +

    ggplot2::geom_point(ggplot2::aes(x = Drivers_value,
                                     y = ses),
                        color = "grey80",
                        alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = Drivers_value,
                                      y = ses),
                         color = palette[6],
                         fill = "grey60",
                         method = "lm") +

    ggplot2::facet_grid(cols = ggplot2::vars(Drivers_nm),
                        scales = "free") +

    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey90"),
                   panel.grid.major = ggplot2::element_line(colour = "grey90")) +
    ggplot2::ggtitle(metric_nm)
  pr_hum_plot2

  return(list("past_stab" = past_stability_plot,
         "present_hab" = present_hab_plot,
         "present_hab_heterog" = present_hab_heterog_plot,
         "disturb" = disturb_plot,
         "past_lu1" = past_lu_plot1, "past_lu2" = past_lu_plot2,
         "pr_hum_imp1" = pr_hum_plot1, "pr_hum_imp2" = pr_hum_plot2))

}
