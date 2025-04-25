################################################################################
##
## Script to gather functions on categories of drivers
##
## Camille Magneville
##
## 11/2024
##
## Drivers_categories_fcts.R
##
################################################################################



#' Function to plot violin plots of variables importance based on their category
#' and return one plot with statistics and Kruskall Wallis test
#'
#' @param rf_df the data frame gathering random forests results from the
#' \code{test.rf.model} function.
#' @param metric_nm character string referring to the studied metric
#' @param palette a color palette containing the color names of the broad
#' categories of the drivers present in the \code{taxa_plot_df}
#' @param drivers_nm_df a data frame containing shortened names of drivers
#'
#' @return two plots in a list, one with stats and mean and the other without
#' and print the result of a Kruskall-Wallis test to see if there is a
#' significant difference of median variables importance between drivers categories
#'
#' @export
#'

cat.distrib.plot <- function(rf_df,
                             metric_nm,
                             palette,
                             drivers_nm_df) {


  # Format longer the rf data frame:
  rf_longer_df <- rf_df %>%
    tibble::rownames_to_column(var = "Drivers_nm") %>%
    tidyr::pivot_longer(cols = colnames(rf_df)[-c(ncol(rf_df), ncol(rf_df) - 1)],
                        names_to = "Repetition_nb",
                        values_to = "Value") %>%
    dplyr::select(-c("mean_imp", "sd_imp"))

  # Now add the broad category for each driver:
  rf_plot_df <- rf_longer_df
  rf_plot_df$Drivers_cat <- rep(NA, nrow(rf_plot_df))

  # Fill this Category column:
  for (i in (1:nrow(rf_plot_df))) {


    if (rf_plot_df$Drivers_nm[i] %in% c("Past_CCVelHolocene_mean.voccMag",
                                        "Past_CCVelLGM_mean.voccMag",
                                        "Past_CCVelShortTerm_mean.voccMag",
                                        "Past_CCVelYoungerDryas_mean.voccMag",
                                        "Past_MAT_sd",
                                        "Past_TAP_sd"
    )) {
      rf_plot_df$Drivers_cat[i] <- "Past Climate Stability"
    }

    if (rf_plot_df$Drivers_nm[i] %in% c( "pH_mean",
                                         "OC_mean",
                                         "Elv_mean",
                                         "Depth_mean",
                                         "VWC_mean",
                                         "Present_AI_mean",
                                         "Present_MAT_mean",
                                         "Present_TAP_mean",
                                         "Pr_FCon_percentage_percentage"
    )) {
      rf_plot_df$Drivers_cat[i] <- "Present Habitat"
    }

    if (rf_plot_df$Drivers_nm[i] %in% c("Present_AI_stdev",
                                        "Present_MAT_stdev",
                                        "Present_TAP_stdev",
                                        "pH_stdev",
                                        "OC_stdev",
                                        "Elv_stdev",
                                        "Depth_stdev",
                                        "VWC_stdev"
    )) {
      rf_plot_df$Drivers_cat[i] <- "Present Habitat Heterogeneity"
    }

    if (rf_plot_df$Drivers_nm[i] %in% c("Pr_FInt_2000_2023_mean",
                                        "Pr_FInt_2000_2023_sd",
                                        "Pr_FSurf_2000_2023_pixels",
                                        "HerbCons_sum",
                                        "HerbRichn_sum")) {
      rf_plot_df$Drivers_cat[i] <- "Disturbances"
    }

    if (rf_plot_df$Drivers_nm[i] %in% c("Past_Perc_croplands_Weighted_Mean",
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
      rf_plot_df$Drivers_cat[i] <- "Past Land Use"
    }

    if (rf_plot_df$Drivers_nm[i] %in% c("Present_Perc_croplands_Weighted_Mean",
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
      rf_plot_df$Drivers_cat[i] <- "Present Human Direct Impact"
    }

  }

  # Order drivers column:
  rf_plot_df$Drivers_cat <- factor(rf_plot_df$Drivers_cat,
                                   levels = c("Past Climate Stability",
                                              "Present Habitat",
                                              "Present Habitat Heterogeneity",
                                              "Disturbances",
                                              "Past Land Use",
                                              "Present Human Direct Impact"))

  # Remove all 0 values (negative importance):
  rf_posit_plot_df <- rf_plot_df[which(rf_plot_df$Value > 0), ]

  # Create the plot:
  distrib_plot <- ggplot2::ggplot(data = rf_posit_plot_df,
                                  ggplot2::aes(x = Drivers_cat,
                                               y = Value,
                                               fill = Drivers_cat)) +
    ggplot2::geom_jitter(alpha = 0.1, size = 1,
                         ggplot2::aes(color = Drivers_cat)) +

    ggplot2::geom_violin(alpha = 0.7,
                         ggplot2::aes(color = Drivers_cat,
                                      fill = Drivers_cat)) +

    ggplot2::geom_boxplot(width=0.1, color="white",
                          alpha = 0.2) +

    ggplot2::scale_color_manual(values = palette) +

    ggplot2::scale_fill_manual(values = palette) +

    ggplot2::ylab("Drivers standardised importance") +

    ggplot2::ggtitle(metric_nm) +

    ggplot2::theme_minimal() +

    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.title.x = ggplot2::element_blank())

  # Compute statistics for the statistical plot (Dunn test btw cat):
  # using `pairwise_comparisons()` function to create a data frame with results
  stat_df <- ggstatsplot::pairwise_comparisons(rf_posit_plot_df, Drivers_cat, Value,
                                               type = "np") %>%
    dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
    dplyr::arrange(group1)
  print(stat_df)

  # Create a vector to gather y positions of significant bracket:
  y_pos_values <- c(max(rf_posit_plot_df$Value))
  # Create an index to fill this vector (as some pairs are not signif
  # ... i can not be used)
  j <- 1
  # Create a column for asterisks labels and fill it + y positions:
  stat_df$asterisk_label <- rep(NA, nrow(stat_df))
  for (i in c(1:nrow(stat_df))){
    if (stat_df$p.value[i] < 0.05) {
      stat_df$asterisk_label[i] <- "*"
      if (i >= 2) {
        j <- j + 1
        y_pos_values[j] <- y_pos_values[j-1] + 0.03
      }
    }
    if (stat_df$p.value[i] < 0.01) {
      stat_df$asterisk_label[i] <- "**"
      if (i >= 2) {
        j <- j + 1
        y_pos_values[j] <- y_pos_values[j-1] + 0.03
      }
    }
    if (stat_df$p.value[i] < 0.001) {
      stat_df$asterisk_label[i] <- "***"
      if (i >= 2) {
        j <- j + 1
        y_pos_values[j] <- y_pos_values[j-1] + 0.03
      }
    }
  }

  stat_plot <- ggstatsplot::ggbetweenstats(data = rf_posit_plot_df,
                                           x = Drivers_cat,
                                           y = Value,
                                           type = "np",
                                           mean.size = 2,
                                           mean.color = "grey60",
                                           title = metric_nm,
                                           xlab = "",
                                           ylab = "Variables importance",
                                           results.subtitle = FALSE,
                                           pairwise.display = "none") +
    ggplot2::scale_color_manual(values = palette) +

    # Add asterisks:
    ggsignif::geom_signif(
      comparisons = stat_df$groups,
      map_signif_level = TRUE,
      annotations = stat_df$asterisk_label,
      y_position = y_pos_values,
      vjust = 0.50,
      test = NULL,
      color = "grey60",
      na.rm = TRUE)

  # Do a Kruskall Wallis test and print result:
  krustall_test <- rstatix::kruskal_test(data = rf_plot_df,
                                         Value ~ Drivers_cat)
  print(krustall_test)

  return(list(distrib_plot, stat_plot, stat_df))

}




#' Plot a heatmap of effect of borad categories of drivers
#'
#' @param list_richness
#' @param list_dispersion
#' @param list_originality
#' @param facet_nm
#'
#' @return
#' @export
#'

heatmap.categories <- function(list_richness,
                               list_dispersion,
                               list_originality,
                               facet_nm) {


  # For richness:

  # Loop on each taxa:
  for (i in c(1:length(list_richness))) {

    # Get the df of the studied taxa:
    taxa_df <- list_richness[[i]]

    # Create a vect which contains equivalence btw drivers and their category:
    drivers_to_cat_vect <- c(
      "Depth_mean" = "Present Habitat",
      "Depth_stdev" = "Present Habitat Heterogeneity",
      "Elv_mean" = "Present Habitat",
      "Elv_stdev" = "Present Habitat Heterogeneity",
      "OC_mean" = "Present Habitat",
      "OC_stdev" = "Present Habitat Heterogeneity",
      "pH_mean" = "Present Habitat",
      "pH_stdev" = "Present Habitat Heterogeneity",
      "Pr_FCon_percentage_percentage" = "Present Habitat",
      "VWC_mean" = "Present Habitat",
      "VWC_stdev" = "Present Habitat Heterogeneity",
      "HerbCons_sum" = "Present Disturbances",
      "HerbRichn_sum" = "Present Disturbances",
      "Pr_FInt_2000_2023_mean" = "Present Disturbances",
      "Pr_FInt_2000_2023_sd" = "Present Disturbances",
      "Pr_FSurf_2000_2023_pixels" = "Present Disturbances",
      "Past_CCVelHolocene_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelLGM_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelShortTerm_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelYoungerDryas_mean.voccMag" = "Past Climate Stability",
      "Past_MAT_sd" = "Past Climate Stability",
      "Past_TAP_sd" = "Past Climate Stability",
      "Present_AI_mean" = "Present Habitat",
      "Present_AI_stdev" = "Present Habitat Heterogeneity",
      "Present_MAT_mean" = "Present Habitat",
      "Present_MAT_stdev" = "Present Habitat Heterogeneity",
      "Present_TAP_mean" = "Present Habitat",
      "Present_TAP_stdev" = "Present Habitat Heterogeneity",
      "Past_Perc_croplands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_croplands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_dense_settlements_Weighted_Mean" = "Past Land Use",
      "Past_Perc_dense_settlements_Weighted_Sd" = "Past Land Use",
      "Past_Perc_rangelands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_rangelands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_seminatural_lands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_seminatural_lands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_villages_Weighted_Mean" = "Past Land Use",
      "Past_Perc_villages_Weighted_Sd" = "Past Land Use",
      "Past_Perc_wild_lands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_wild_lands_Weighted_Sd" = "Past Land Use",
      "Present_Perc_croplands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_croplands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_dense_settlements_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_dense_settlements_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_rangelands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_rangelands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_seminatural_lands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_seminatural_lands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_villages_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_villages_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_wild_lands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_wild_lands_Weighted_Sd" = "Present Direct Human Impact",
      "Pr_Pop_2020_mean" = "Present Direct Human Impact",
      "Pr_RatePop_2020_mean" = "Present Direct Human Impact"
      )

    # Add a new column to the taxa_df with drivers categories and select columns:
    taxa_subset_df <- taxa_df %>%
      tibble::rownames_to_column(var = "Driver_nm") %>%
      dplyr::mutate(Driver_cat = drivers_to_cat_vect[Driver_nm]) %>%
      dplyr::select(c("Driver_nm", "Driver_cat", "mean_imp", "sd_imp"))

    # For every category, compute mean and sd or mean_imp:
    summ_taxa_df <- taxa_subset_df %>%
      dplyr::group_by(Driver_cat) %>%
      dplyr::summarise(mean = mean(mean_imp),
                       sd = sd(mean_imp))

    # Add a column which refers to the taxa studied:
    summ_taxa_df <- summ_taxa_df %>%
      dplyr::mutate(Taxa_nm = names(list_richness[i]))


    # Store these values:
    if (i == 1){
      richn_all_taxa_df <- summ_taxa_df
    }
    if (i != 1){
      richn_all_taxa_df <- dplyr::bind_rows(richn_all_taxa_df, summ_taxa_df)
    }

  } # end loop on each taxa


  # For dispersion:

  # Loop on each taxa:
  for (i in c(1:length(list_dispersion))) {

    # Get the df of the studied taxa:
    taxa_df <- list_dispersion[[i]]

    # Create a vect which contains equivalence btw drivers and their category:
    drivers_to_cat_vect <- c(
      "Depth_mean" = "Present Habitat",
      "Depth_stdev" = "Present Habitat Heterogeneity",
      "Elv_mean" = "Present Habitat",
      "Elv_stdev" = "Present Habitat Heterogeneity",
      "OC_mean" = "Present Habitat",
      "OC_stdev" = "Present Habitat Heterogeneity",
      "pH_mean" = "Present Habitat",
      "pH_stdev" = "Present Habitat Heterogeneity",
      "Pr_FCon_percentage_percentage" = "Present Habitat",
      "VWC_mean" = "Present Habitat",
      "VWC_stdev" = "Present Habitat Heterogeneity",
      "HerbCons_sum" = "Present Disturbances",
      "HerbRichn_sum" = "Present Disturbances",
      "Pr_FInt_2000_2023_mean" = "Present Disturbances",
      "Pr_FInt_2000_2023_sd" = "Present Disturbances",
      "Pr_FSurf_2000_2023_pixels" = "Present Disturbances",
      "Past_CCVelHolocene_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelLGM_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelShortTerm_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelYoungerDryas_mean.voccMag" = "Past Climate Stability",
      "Past_MAT_sd" = "Past Climate Stability",
      "Past_TAP_sd" = "Past Climate Stability",
      "Present_AI_mean" = "Present Habitat",
      "Present_AI_stdev" = "Present Habitat Heterogeneity",
      "Present_MAT_mean" = "Present Habitat",
      "Present_MAT_stdev" = "Present Habitat Heterogeneity",
      "Present_TAP_mean" = "Present Habitat",
      "Present_TAP_stdev" = "Present Habitat Heterogeneity",
      "Past_Perc_croplands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_croplands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_dense_settlements_Weighted_Mean" = "Past Land Use",
      "Past_Perc_dense_settlements_Weighted_Sd" = "Past Land Use",
      "Past_Perc_rangelands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_rangelands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_seminatural_lands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_seminatural_lands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_villages_Weighted_Mean" = "Past Land Use",
      "Past_Perc_villages_Weighted_Sd" = "Past Land Use",
      "Past_Perc_wild_lands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_wild_lands_Weighted_Sd" = "Past Land Use",
      "Present_Perc_croplands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_croplands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_dense_settlements_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_dense_settlements_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_rangelands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_rangelands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_seminatural_lands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_seminatural_lands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_villages_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_villages_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_wild_lands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_wild_lands_Weighted_Sd" = "Present Direct Human Impact",
      "Pr_Pop_2020_mean" = "Present Direct Human Impact",
      "Pr_RatePop_2020_mean" = "Present Direct Human Impact"
    )

    # Add a new column to the taxa_df with drivers categories and select columns:
    taxa_subset_df <- taxa_df %>%
      tibble::rownames_to_column(var = "Driver_nm") %>%
      dplyr::mutate(Driver_cat = drivers_to_cat_vect[Driver_nm]) %>%
      dplyr::select(c("Driver_nm", "Driver_cat", "mean_imp", "sd_imp"))

    # For every category, compute mean and sd or mean_imp:
    summ_taxa_df <- taxa_subset_df %>%
      dplyr::group_by(Driver_cat) %>%
      dplyr::summarise(mean = mean(mean_imp),
                       sd = sd(mean_imp))

    # Add a column which refers to the taxa studied:
    summ_taxa_df <- summ_taxa_df %>%
      dplyr::mutate(Taxa_nm = names(list_dispersion[i]))


    # Store these values:
    if (i == 1){
      disp_all_taxa_df <- summ_taxa_df
    }
    if (i != 1){
      disp_all_taxa_df <- dplyr::bind_rows(disp_all_taxa_df, summ_taxa_df)
    }

  } # end loop on each taxa


  # For originality:

  # Loop on each taxa:
  for (i in c(1:length(list_originality))) {

    # Get the df of the studied taxa:
    taxa_df <- list_originality[[i]]

    # Create a vect which contains equivalence btw drivers and their category:
    drivers_to_cat_vect <- c(
      "Depth_mean" = "Present Habitat",
      "Depth_stdev" = "Present Habitat Heterogeneity",
      "Elv_mean" = "Present Habitat",
      "Elv_stdev" = "Present Habitat Heterogeneity",
      "OC_mean" = "Present Habitat",
      "OC_stdev" = "Present Habitat Heterogeneity",
      "pH_mean" = "Present Habitat",
      "pH_stdev" = "Present Habitat Heterogeneity",
      "Pr_FCon_percentage_percentage" = "Present Habitat",
      "VWC_mean" = "Present Habitat",
      "VWC_stdev" = "Present Habitat Heterogeneity",
      "HerbCons_sum" = "Present Disturbances",
      "HerbRichn_sum" = "Present Disturbances",
      "Pr_FInt_2000_2023_mean" = "Present Disturbances",
      "Pr_FInt_2000_2023_sd" = "Present Disturbances",
      "Pr_FSurf_2000_2023_pixels" = "Present Disturbances",
      "Past_CCVelHolocene_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelLGM_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelShortTerm_mean.voccMag" = "Past Climate Stability",
      "Past_CCVelYoungerDryas_mean.voccMag" = "Past Climate Stability",
      "Past_MAT_sd" = "Past Climate Stability",
      "Past_TAP_sd" = "Past Climate Stability",
      "Present_AI_mean" = "Present Habitat",
      "Present_AI_stdev" = "Present Habitat Heterogeneity",
      "Present_MAT_mean" = "Present Habitat",
      "Present_MAT_stdev" = "Present Habitat Heterogeneity",
      "Present_TAP_mean" = "Present Habitat",
      "Present_TAP_stdev" = "Present Habitat Heterogeneity",
      "Past_Perc_croplands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_croplands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_dense_settlements_Weighted_Mean" = "Past Land Use",
      "Past_Perc_dense_settlements_Weighted_Sd" = "Past Land Use",
      "Past_Perc_rangelands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_rangelands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_seminatural_lands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_seminatural_lands_Weighted_Sd" = "Past Land Use",
      "Past_Perc_villages_Weighted_Mean" = "Past Land Use",
      "Past_Perc_villages_Weighted_Sd" = "Past Land Use",
      "Past_Perc_wild_lands_Weighted_Mean" = "Past Land Use",
      "Past_Perc_wild_lands_Weighted_Sd" = "Past Land Use",
      "Present_Perc_croplands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_croplands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_dense_settlements_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_dense_settlements_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_rangelands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_rangelands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_seminatural_lands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_seminatural_lands_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_villages_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_villages_Weighted_Sd" = "Present Direct Human Impact",
      "Present_Perc_wild_lands_Weighted_Mean" = "Present Direct Human Impact",
      "Present_Perc_wild_lands_Weighted_Sd" = "Present Direct Human Impact",
      "Pr_Pop_2020_mean" = "Present Direct Human Impact",
      "Pr_RatePop_2020_mean" = "Present Direct Human Impact"
    )

    # Add a new column to the taxa_df with drivers categories and select columns:
    taxa_subset_df <- taxa_df %>%
      tibble::rownames_to_column(var = "Driver_nm") %>%
      dplyr::mutate(Driver_cat = drivers_to_cat_vect[Driver_nm]) %>%
      dplyr::select(c("Driver_nm", "Driver_cat", "mean_imp", "sd_imp"))

    # For every category, compute mean and sd or mean_imp:
    summ_taxa_df <- taxa_subset_df %>%
      dplyr::group_by(Driver_cat) %>%
      dplyr::summarise(mean = mean(mean_imp),
                       sd = sd(mean_imp))

    # Add a column which refers to the taxa studied:
    summ_taxa_df <- summ_taxa_df %>%
      dplyr::mutate(Taxa_nm = names(list_originality[i]))


    # Store these values:
    if (i == 1){
      orig_all_taxa_df <- summ_taxa_df
    }
    if (i != 1){
      orig_all_taxa_df <- dplyr::bind_rows(orig_all_taxa_df, summ_taxa_df)
    }

  } # end loop on each taxa


  # Assemble the three dataframes into a big one:
  richn_all_taxa_df <- dplyr::mutate(richn_all_taxa_df,
                                     Metric_nm = "Richness")
  disp_all_taxa_df <- dplyr::mutate(disp_all_taxa_df,
                                     Metric_nm = "Dispersion")
  orig_all_taxa_df <- dplyr::mutate(orig_all_taxa_df,
                                     Metric_nm = "Originality")
  all_metric_plot_df <- dplyr::bind_rows(richn_all_taxa_df,
                                         disp_all_taxa_df,
                                         orig_all_taxa_df)

  # Order metrics:
  all_metric_plot_df$Metric_nm <- factor(all_metric_plot_df$Metric_nm,
                                         levels = c("Richness",
                                                    "Dispersion",
                                                    "Originality"))
  # Order driver categories:
  all_metric_plot_df$Driver_cat <- factor(all_metric_plot_df$Driver_cat,
                                         levels = c("Past Climate Stability",
                                                    "Present Habitat",
                                                    "Present Habitat Heterogeneity",
                                                    "Present Disturbances",
                                                    "Past Land Use",
                                                    "Present Direct Human Impact"))

  all_metric_plot_df$Taxa_nm <- factor(all_metric_plot_df$Taxa_nm,
                                          levels = c("Mammals",
                                                     "Birds",
                                                     "Reptiles",
                                                     "Butterflies",
                                                     "Trees"))

  # Create a mean heatmap:
  heatmap_mean_plot <- ggplot2::ggplot(data = all_metric_plot_df,
                                  ggplot2::aes(x = `Driver_cat`,
                                               y = `Taxa_nm`,
                                               fill = `mean`)) +
    ggplot2::geom_raster() +

    ggplot2::scale_fill_viridis_c(limits = c(min(all_metric_plot_df$mean),
                                             max(all_metric_plot_df$mean)),
                                  name = "Mean Standardised Importance") +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
                   panel.grid.major = ggplot2::element_line(colour = "grey83"),
                   legend.title = ggplot2::element_text(colour = "grey40",
                                                        size = 12),
                   legend.text = ggplot2::element_text(colour = "grey40",
                                                       size = 12),
                   axis.text.x = ggplot2::element_text(colour = "grey40",
                                                        size = 15,
                                                        angle = 45,
                                                        h = 1),
                   axis.text.y = ggplot2::element_text(colour = "grey40",
                                                       size = 15),
                   axis.ticks.x = ggplot2::element_line(colour = "grey40"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey40"),
                   strip.text = ggplot2::element_text(size = 15,
                                                      colour = "grey40"),
                   strip.background = ggplot2::element_rect(fill = NA,
                                                            colour = NA)) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(color = "Mean Standardised Importance") +

    ggplot2::facet_grid(cols = ggplot2::vars(`Metric_nm`))

  # Create a sd heatmap:
  heatmap_sd_plot <- ggplot2::ggplot(data = all_metric_plot_df,
                                  ggplot2::aes(x = `Driver_cat`,
                                               y = `Taxa_nm`,
                                               fill = `sd`)) +
    ggplot2::geom_raster() +

    ggplot2::scale_fill_viridis_c(limits = c(min(all_metric_plot_df$sd),
                                             max(all_metric_plot_df$sd)),
                                  name = "Mean Standardised Importance") +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey83"),
                   panel.grid.major = ggplot2::element_line(colour = "grey83"),
                   legend.title = ggplot2::element_text(colour = "grey40",
                                                        size = 12),
                   legend.text = ggplot2::element_text(colour = "grey40",
                                                       size = 12),
                   axis.text.x = ggplot2::element_text(colour = "grey40",
                                                       size = 15,
                                                       angle = 45,
                                                       h = 1),
                   axis.text.y = ggplot2::element_text(colour = "grey40",
                                                       size = 15),
                   axis.ticks.x = ggplot2::element_line(colour = "grey40"),
                   axis.ticks.y = ggplot2::element_line(colour = "grey40"),
                   strip.text = ggplot2::element_text(size = 15,
                                                      colour = "grey40"),
                   strip.background = ggplot2::element_rect(fill = NA,
                                                            colour = NA)) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(color = "Standard Deviation Standardised Importance") +

    ggplot2::facet_grid(cols = ggplot2::vars(`Metric_nm`))

  combines_plot <- heatmap_mean_plot + heatmap_sd_plot + patchwork::plot_layout(nrow = 2) +
    patchwork::plot_annotation(title = facet_nm,
                    theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5,
                                                            color = "grey40")))


  # Save it:
  ggplot2::ggsave(plot = combines_plot,
                  filename = here::here("outputs",
                                        paste0("heatmap_drivers_categories",
                                               sep = "_",
                                               facet_nm,
                                               ".jpeg")),
                  device = "jpeg",
                  scale = 1,
                  height = 7500,
                  width = 8000,
                  units = "px",
                  dpi = 600)

  # return the plot dataframe:
  return(all_metric_plot_df)

}
