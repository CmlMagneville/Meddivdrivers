################################################################################
##
## Fcts for diverse purposes
##
## Camille Magneville
##
## 15/05/2024
##
## Help_fcts.R
##
################################################################################




#' Localise cells in a given grid
#'
#' @param cell_vect a vector containing the name (Idgrid) of the cells to
#' localise.
#' @param grid  a \code{sf} object referring to the spatial grid used, either
#' the 10x10km grid (WOODIV one) or the 50x50km one (INTEGRADIV one).
#'
#' @return a ggplot object plotting in red where the given cells are located
#' @export
#'

locate.cells <- function(cell_vect,
                         grid) {


  # Add a column to the grid object for TRUE / FALSE if the cell is to be located:
  grid_plot_df <- grid %>%
    dplyr::mutate("plot" = rep(FALSE, nrow(grid)))

  # Fill it with TRUE for the cells to plot:
  grid_plot_df$plot[which(grid_plot_df$Idgrid %in% cell_vect)] <- TRUE

  # Plot it:
  cells_plot <- ggplot2::ggplot(data = grid_plot_df) +

    ggplot2::geom_sf(ggplot2::aes(fill = plot)) +

    ggplot2::scale_fill_manual(values = c("gray96",
                                          "turquoise")) +

    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 12),
                   panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
                   panel.background = ggplot2::element_rect(fill = NA,
                                                            colour = "black"))  +

    ggplot2::labs(x = "Longitude (EPSG 3035)", y = "Latitude (EPSG 3035)",
                  fill= "To be localised")

  print(cells_plot)


}



