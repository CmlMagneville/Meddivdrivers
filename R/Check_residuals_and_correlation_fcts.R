################################################################################
##
## Function to compute spatial correlation among residuals and
## ... correlation between PD and FD metrics and plot
## ... scatterplots with species richness as dots size
##
## Camille Magneville
##
## 11/2023-11/2024
##
## Check_residuals_and_correlation_fcts.R
##
################################################################################



#' Compute correlation between PD and FD metrics and plot
#'
#' @param indices_df a data frame gathering the indices to compare: they should
#' be fric, fdis, fspe, mpd,faith pd and species richness and also
#' colmns for \code{Idgrid}, \code{Taxon} and \code{Scale}. The df has been
#' created here under \code{5_..._Correlation_btw_..._metrics.R}. Should be in
#' the following order and names: \code{Idgrid, sp_richn, fric, fdis, fspe,
#' faith_pd, mpd, Grid, Taxon}.
#'
#' @save a TRUE/FALSE value indicating whether plots should be saved or not
#'
#' @return two ggplot plot showing correlation between all variables and species
#' richness as dot size (for the second plot)
#'
#' @export
#'
#' @examples
#'
#'


correlation.fd.pd <- function(indices_df,
                              save) {



  # Do the plot with correlation among all variables:
  all_correl_plot <- GGally::ggpairs(trees_indices_df,
                                 columns = 2:7,
                                 ggplot2::aes(alpha = 0.5))


  # Add a column with categories of species richness:
  # One class for less than 5 species and then quantiles values:
  quantiles <- quantile(trees_indices_df$sp_richn)
  quant_25 <- quantiles[2][[1]]
  quant_50 <- quantiles[3][[1]]
  quant_75 <- quantiles[4][[1]]
  quant_100 <- quantiles[5][[1]]

  trees_cat_ind_df <- trees_indices_df %>%
    dplyr::mutate(sp_richn_cat = cut(sp_richn,
                                     breaks = c(-Inf, 5, quant_25,
                                              quant_50, quant_75,
                                              quant_100),
                                     labels = c("1-5",
                                    paste0("6", sep = "-", quant_25),
                                    paste0(quant_25+1, sep = "-", quant_50),
                                    paste0(quant_50+1, sep = "-", quant_75),
                                    paste0(quant_75+1, sep = "-", quant_100))))


  # Do the plot with species richness being size of the plots:
  sp_richn_plot <- GGally::ggpairs(trees_cat_ind_df,
                                   columns = 3:7,
                                   ggplot2::aes(color = sp_richn_cat,
                                                alpha = 0.8))

  print(all_correl_plot)
  print(sp_richn_plot)

  # save the plots:
  if (save == TRUE) {

    ggplot2::ggsave(plot = all_correl_plot,
                    filename = here::here("outputs",
                                          paste0(unique(indices_df$Taxon),
                                                 sep = "_",
                                                 "TD_FD_PD_correlation",
                                                 sep = ".",
                                                 "pdf")),
                    device = "pdf",
                    scale = 1,
                    height = 4000,
                    width = 5000,
                    units = "px",
                    dpi = 600)

    ggplot2::ggsave(plot = sp_richn_plot,
                    filename = here::here("outputs",
                                          paste0(unique(indices_df$Taxon),
                                                 sep = "_",
                                                 "FD_PD_correlation_sprichncat",
                                                 sep = ".",
                                                 "pdf")),
                    device = "pdf",
                    scale = 1,
                    height = 4000,
                    width = 5000,
                    units = "px",
                    dpi = 600)

  }


  return(list(all_correl_plot,
              sp_richn_plot))


}
