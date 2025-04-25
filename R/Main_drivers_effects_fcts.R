################################################################################
##
## Script gathering functions which help to study the directionality of
## ... the effect of the main drivers for all taxa and diversity dimensions
##
## Camille Magneville
##
##  11/2024
##
## Main_drivers_effects_fcts.R
##
################################################################################


#' Compute logistic regression
#'
#' @param driver_ses_df
#' @param driver_nm
#'
#' @return
#' @export
#'

log.reg.compute <- function(drivers_ses_df,
                            driver_nm) {


  # Set the SES variable as binary:
  binary_rf_df <- drivers_ses_df
  binary_rf_df$ses_binary <- ifelse(binary_rf_df$ses > 0, "positive", "negative")
  binary_rf_df$ses_binary <- as.factor(binary_rf_df$ses_binary)

  # Compute the logistic regression:
  log_model <- stats::glm(ses_binary ~ get(driver_nm),
                          data = binary_rf_df,
                          family = binomial)

 # Get the model summary in a nice df:
  model_summ <- broom::tidy(log_model)

  # Compute odds ratios and 95% CI:
  model_summ <- model_summ %>%
    dplyr::mutate(Odds_Ratios = exp(estimate),
                  lower_CI = exp(estimate - 1.96 * std.error),
                  higher_CI = exp(estimate + 1.96 * std.error))

  print(model_summ)
  return(model_summ)

}


#' Plot forest plots for each taxa
#'
#' @param model_summ_list
#' @param drivers_nm_df
#'
#' @return
#' @export
#'


forest.plot <- function(model_summ_list,
                        drivers_nm_df) {


  # Create a dataframe that will contain all info for plotting:
  plot_df <- as.data.frame(matrix(ncol = 6,
                           nrow = 1,
                           NA))
  colnames(plot_df) <- c("Dim_nm", "Driver_nm",
                         "Odd_ratios", "Lower_CI", "Upper_CI",
                         "Driver_cat")

  # Loop on dimensions:
  for (i in c(1:length(model_summ_list))) {

    # Get dimension name:
    dim_nm <- names(model_summ_list)[i]

    # Get dimension list:
    dim_list <- model_summ_list[[i]]

    # Create a dataframe which will contain information for this dimension:
    dim_df <- as.data.frame(matrix(ncol = 5,
                                   nrow = 1,
                                   NA))
    colnames(dim_df) <- c("Dim_nm", "Driver_nm",
                           "Odd_ratios", "Lower_CI", "Upper_CI")

    # Loop on each main driver for this given dimension:
    for (j in c(1:length(dim_list))) {

      # Get the dataframe related to the studied driver:
      driver_df <- dim_list[[j]]

      # Get the name of the driver:
      driver_nm <- names(dim_list)[j]

      # Fill the dimension data frame for the studied driver:
      odds_ratios <- driver_df$Odds_Ratios[2]
      lower_ci <- driver_df$lower_CI[2]
      upper_ci <- driver_df$higher_CI[2]
      dim_df <- dim_df %>%
        dplyr::add_row("Dim_nm" = dim_nm,
                       "Driver_nm" = driver_nm,
                       "Odd_ratios" = odds_ratios,
                       "Lower_CI" = lower_ci,
                       "Upper_CI" = upper_ci)

    } # end loop on each driver for the given dimension

    # Remove the first row of dim_df (NA):
    dim_df <- dim_df[-1, ]

    # Add a driver_cat column filled with NA for now:
    dim_df$Driver_cat <- rep(NA, nrow(dim_df))

    # Add this df to the plot_df:
    plot_df <- plot_df %>%
      dplyr::bind_rows(dim_df)

  } # end loop on each dimension

  # Remove the first row of plot_df (NA):
  plot_df <- plot_df[-1, ]

  # Fill the Driver_cat column:
  for (m in c(1:nrow(plot_df))) {

    if (plot_df$Driver_nm[m] %in% c("Past MAT sd",
                                    "Past TAP sd",
                                    "Clim. Vel. LGM",
                                    "Clim. Vel. YD increase",
                                    "Clim. Vel. YD decrease",
                                    "Clim. Vel. Holocene")) {
      plot_df$Driver_cat[m] <- "Past Climate Stability"
    }
    if (plot_df$Driver_nm[m] %in% c("MAT mean",
                                    "Depth mean",
                                    "AI mean",
                                    "Elevation mean")) {
      plot_df$Driver_cat[m] <- "Present Habitat"
    }
    if (plot_df$Driver_nm[m] %in% c("Herbivores Consumption",
                                    "Herbivores Richness")) {
      plot_df$Driver_cat[m] <- "Disturbance"
    }
    if (plot_df$Driver_nm[m] %in% c("Pop. Growth Rate")) {
      plot_df$Driver_cat[m] <- "Present Human Direct Impact"
    }
  }





  # Plot the forest plot
  ggplot(all_data, aes(x = OR, y = Driver, color = Category)) +
    geom_pointrange(aes(xmin = Lower, xmax = Upper), position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
    labs(
      x = "Odds Ratio (95% CI)",
      y = "Driver",
      title = "Effect of Drivers on Diversity Dimensions"
    ) +
    scale_color_manual(values = category_colors) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold")  # Facet strip labels
    ) +
    facet_wrap(~Dimension, scales = "free_y", ncol = 1)


}


#' Contingency table, Chi2 and associated plots to get drivers effect
#'
#' @param driver_ses_df a dataframe linking Idgrid, drivers values and ses
#' values for each cell (rows)
#' @param driver_nm a character string referring to the name of the driver to
#' study
#' @param color_nms a vector containing the hexadecimal names of two colors used
#' to plot low and high values (negative/positive) of the studied driver
#' @param threshold_type a character string referring to whether the threshold
#' should divide the driver data in two (mean, 0 or mean of positive values), or
#' only study extreme values (superior to 75% quantile and inferior to 25% quantiles)
#' @param drivers_nm_df the dataframe containing shorten names of each driver
#' @param facet_nm the name of the facet to be studied, either FD or PD
#' @param dim_nm the name of the dimension of the facte to be studied, Richness,
#' Dispersion or Originality
#' @param taxa_nm the name of the taxa to be studied
#'
#' @return
#' @export
#'

contingency.analyses <- function(driver_ses_df,
                                 driver_nm,
                                 color_nms,
                                 threshold_type,
                                 drivers_nm_df,
                                 facet_nm,
                                 dim_nm,
                                 taxa_nm) {


  # Only keep interesting columns:
  simple_driver_ses_df <- driver_ses_df[, which(colnames(driver_ses_df) %in% c("Idgrid",
                                                                              "ses",
                                                                              driver_nm))]

  # If the threshold is one which divides the data in two using the mean:
  if (threshold_type == "normal") {


    # If the driver is not velocity or growth rate - compute the mean:
    if (! driver_nm %in% c("Past_CCVelHolocene_mean.voccMag",
                           "Past_CCVelLGM_mean.voccMag",
                           "Past_CCVelShortTerm_mean.voccMag",
                           "Past_CCVelYoungerDryas_mean.voccMag",
                           "Pr_RatePop_2020_mean")) {

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = mean(simple_driver_ses_df[, driver_nm]), col = "red4")
      abline(v = mean(simple_driver_ses_df[, driver_nm])
             + sd(simple_driver_ses_df[, driver_nm]), col = "blue4")
      abline(v = mean(simple_driver_ses_df[, driver_nm])
             - sd(simple_driver_ses_df[, driver_nm]), col = "blue4")

      # Define the mean as the threshold value:
      threshold_value <- mean(simple_driver_ses_df[, driver_nm])

      # Complete the table with high/low compared to the mean:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                  "high", "low"))

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity - use mean of positive values as a threshold:
    if (driver_nm %in% c("Past_CCVelLGM_mean.voccMag",
                         "Past_CCVelYoungerDryas_mean.voccMag")) {



      # Define the mean of positive value as the threshold:
      threshold_value <- mean(simple_driver_ses_df[which(simple_driver_ses_df[, driver_nm] > 0), driver_nm])

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value, col = "red4")

      # Complete the table with high/low compared to 0:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                  "high increase", "decrease or low increase"))

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity YD increase use mean as a threshold (but only - values):
    if (driver_nm == "Past_CCVelShortTerm_mean.voccMag") {


      # Define the mean of positive value as the threshold:
      threshold_value <- mean(simple_driver_ses_df[, driver_nm])

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value, col = "red4")

      # Complete the table with high/low compared to 0:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                  "low decrease", "high decrease"))

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity Holocene - use mean as a threshold (but only + values):
    if (driver_nm == "Past_CCVelHolocene_mean.voccMag") {


      # Define the mean of positive value as the threshold:
      threshold_value <- mean(simple_driver_ses_df[, driver_nm])

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value, col = "red4")

      # Complete the table with high/low compared to 0:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                  "high increase", "low increase"))

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }




    # If the driver is growth rate - use 0 as a threshold:
    if (driver_nm == "Pr_RatePop_2020_mean") {

      threshold_value <- 0

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = 0, col = "red4")

      # Complete the table with high/low compared to 0:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                  "increase", "decrease"))

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }


  } # end - if threshold_type == normal


  # if the threshold type only studies the most extreme values (25 and 75 quantiles):
  if (threshold_type == "extremes") {


    # - Only labels are changing here, not thresholds -

    # If the driver is not velocity or growth rate:
    if (! driver_nm %in% c("Past_CCVelHolocene_mean.voccMag",
                           "Past_CCVelLGM_mean.voccMag",
                           "Past_CCVelShortTerm_mean.voccMag",
                           "Past_CCVelYoungerDryas_mean.voccMag",
                           "Pr_RatePop_2020_mean")) {

      # Define the thwo quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely high",
          get(driver_nm) <= threshold_value_low ~ "extremely low",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity:
    if (driver_nm %in% c("Past_CCVelLGM_mean.voccMag",
                         "Past_CCVelYoungerDryas_mean.voccMag")) {



      # Define the thwo quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely high increase",
          get(driver_nm) <= threshold_value_low ~ "extremely low increase or decrease",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")
      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity YD Increase :
    if (driver_nm %in% c("Past_CCVelShortTerm_mean.voccMag")) {


      # Define the two quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely low decrease",
          get(driver_nm) <= threshold_value_low ~ "extremely high decrease",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")
      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity Holocene :
    if (driver_nm %in% c("Past_CCVelHolocene_mean.voccMag")) {


      # Define the two quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely high increase",
          get(driver_nm) <= threshold_value_low ~ "extremely low increase",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")
      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is growth rate - use 0 as a threshold:
    if (driver_nm == "Pr_RatePop_2020_mean") {

      # Define the thwo quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely high increase",
          get(driver_nm) <= threshold_value_low ~ "extremely low increase or decrease",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

  } # end - if threshold type == extremes


  # If the threshold is one which divides the data in three using quantiles:
  if (threshold_type == "extremes_medium") {

    # Define the two quantiles:
    threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
    threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


    # Print an histogram:
    hist(simple_driver_ses_df[, driver_nm])
    abline(v = threshold_value_low, col = "blue4")
    abline(v = threshold_value_high, col = "blue4")

    # Complete the table with high/low and remove rows in the middle:
    cat_drivers_ses_df <- simple_driver_ses_df %>%
      dplyr::mutate(driver_cat = dplyr::case_when(
        get(driver_nm) >= threshold_value_high ~ "high",
        get(driver_nm) <= threshold_value_low ~ "low",
        TRUE ~ "medium"))

    # But if driver = velocity Holocene, as it's always negative:
    if (driver_nm %in% c("Past_CCVelShortTerm_mean.voccMag")) {
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "low",
          get(driver_nm) <= threshold_value_low ~ "high",
          TRUE ~ "medium"))
    }

    # Format right order:
    cat_drivers_ses_df$driver_cat <- factor(cat_drivers_ses_df$driver_cat,
                                            levels = c("low", "medium", "high"))

    # Complete the table with +/- for ses:
    cat_drivers_ses_df <- cat_drivers_ses_df %>%
      dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                             "positive", "negative"))

    # Only keep the columns with categories and rename them:
    final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
    colnames(final_df)[2] <- "driver"
    colnames(final_df)[3] <- "ses"

  } # end - if threshold type == extremes_medium

  # Create the contingency table based on this data:
  # Table with total counts in each category:
  print("Count table")
  count_table <- xtabs(~ driver + ses, data = final_df)
  print(count_table)
  print("Frequency table")
  print((count_table/nrow(final_df))*100)
  print("Frequency table - % negative or positive")
  count_table_negpos <- as.data.frame(count_table)
  nb_pos <- nrow(final_df[which(final_df$ses == "positive"), ])
  nb_neg <- nrow(final_df[which(final_df$ses == "negative"), ])
  count_table_negpos$Freq[which(count_table_negpos$ses == "positive")] <- count_table_negpos$Freq[which(count_table_negpos$ses == "positive")]/nb_pos
  count_table_negpos$Freq[which(count_table_negpos$ses == "negative")] <- count_table_negpos$Freq[which(count_table_negpos$ses == "negative")]/nb_neg
  count_table_negpos <- dplyr::arrange(count_table_negpos, driver)
  print(count_table_negpos)

  # Do the Chi squared test of independence:
  # First check that ok to do the test: expected values > 5
  print("Expected values if no association - should all be higher than 5")
  expected <- chisq.test(count_table)$expected
  print(expected)
  # pvalue significant (inf 0.05) - signif association between color and driver:
  print("Chi2 test No Yate's corr as big sample size - pvalue < 0.05 significant association")
  test <- chisq.test(count_table, correct = FALSE)
  print(test)

  # Right factor:
  final_df$ses <- factor(final_df$ses, levels = c("negative", "positive"))


  # Mosaic plot:
  mosaic_plot <- ggplot2::ggplot(data = final_df) +
    ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(ses),
                                       fill = driver),
                          show.legend = FALSE) +
    ggmosaic::theme_mosaic() +
    ggplot2::scale_fill_manual(values = color_nms) +
    ggplot2::labs(y = drivers_nm_df$Drivers_short_nm[which(drivers_nm_df$Drivers_nm == driver_nm)],
                  x = "SES")
  print(mosaic_plot)

  # Save it:
  ggplot2::ggsave(plot = mosaic_plot,
                  filename = here::here("outputs",
                                        "directionality",
                                        paste0("mosaic_plot", sep = "_",
                                        driver_nm, sep = "_",
                                        threshold_type, sep = "_",
                                        facet_nm, sep = "_",
                                        dim_nm, sep = "_",
                                        taxa_nm, sep = "",
                                        ".jpeg")),
                  device = "jpeg",
                  scale = 1,
                  height = 3000,
                  width = 3000,
                  units = "px",
                  dpi = 600)


  # Continuous Plot:

  # Rename the driver column:
  simple_driver_plot_df <- simple_driver_ses_df
  colnames(simple_driver_plot_df)[2] <- "driver"

  continuous_plot <- ggplot2::ggplot(data = simple_driver_plot_df) +
    ggplot2::geom_jitter(ggplot2::aes(x = ses, y = 0, color = driver),
                         height = 0.2, size = 2) +
    ggplot2::scale_colour_gradient(low = color_nms[1], high = color_nms[3]) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(ylim = c(-0.4, 0.4),
                             xlim = c(-max(abs(simple_driver_plot_df$ses)),
                             max(abs(simple_driver_plot_df$ses)))) +
    ggplot2::labs(color = drivers_nm_df$Drivers_short_nm[which(drivers_nm_df$Drivers_nm == driver_nm)])

  print(continuous_plot)

  ggplot2::ggsave(plot = continuous_plot,
                  filename = here::here("outputs",
                                        "directionality",
                                        paste0("continuous_plot", sep = "_",
                                               driver_nm, sep = "_",
                                               threshold_type, sep = "_",
                                               facet_nm, sep = "_",
                                               dim_nm, sep = "_",
                                               taxa_nm, sep = "",
                                               ".jpeg")),
                  device = "jpeg",
                  scale = 1,
                  height = 3000,
                  width = 4000,
                  units = "px",
                  dpi = 600)


}
