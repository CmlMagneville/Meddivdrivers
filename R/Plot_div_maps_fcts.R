################################################################################
##
## Fcts to plot taxonomic, functional and phylogenetic diversities maps
##
## Camille Magneville
##
## 03/10/2023
##
## Plot_div_maps_fcts.R
##
################################################################################


#' Plot A Given Diversity Metric At A Given Scale
#'
#' @param div_per_cell_df a data frame with the spatial cells in rows and the
#' following columns: \strong{Idgrid} with the identity of each cell,
#' \strong{metric} name of the metric for column name (ex: \code{sp_richness}),
#' \strong{Grid} referring to the scale studied (only one value here),
#' \strong{Taxon} with Taxon's name (should be the same for all df as
#' one taxon is studied here). Columns should be \bold{in this order}.
#'
#' @param div_facet_nm a character string referring to the name of the diversity
#' facets studied: Can be either \code{TD}, \code{FD} or \code{PD}.
#'
#' @param metric_nm a character string referring to the name of the diversity
#' metric studied.
#'
#' @param grid a \code{sf} object referring to the spatial grid used, either
#' the 10x10km grid (WOODIV one) or the 50x50km one (INTEGRADIV one).
#'
#' @param plot_title a TRUE/FALSE value referring to plotting or not the graph
#' title (on the diversity facet studied)
#'
#' @param land_mask a shapefile of europe land mask
#'
#' @param save a TRUE/FALSE value referring to saving or not the graph
#'
#' @return a data frame with the values of taxonomic diversity (here species
#' richness) per cell of the chosen grid.
#'
#' @export
#'
#' @examples
#'


div.maps.plot <- function(div_per_cell_df,
                          div_facet_nm,
                          metric_nm,
                          grid,
                          col_pal,
                          plot_title,
                          land_mask,
                          save) {


  # Rename the column referring to the diversity metric:
  colnames(div_per_cell_df)[2] <- "Div_metric"


  # Make sure that same format for the Idgrid column in div and grid objects:
  div_per_cell_df <- dplyr::mutate(div_per_cell_df,
                                   Idgrid = as.character(Idgrid))
  grid <- dplyr::mutate(grid, Idgrid = as.character(Idgrid))

  # Link the div_per_cell_df and the grid so can have spatial info:
  spatial_div_per_cell_df <- dplyr::left_join(grid, div_per_cell_df,
                                              by = "Idgrid")

  # Some rows have Div_metric = NA, remove them:
  spatial_div_per_cell_df <- dplyr::filter(spatial_div_per_cell_df,
                                           ! is.na(Div_metric))

    # Create a box around the study area:
    box_m <- sf::st_bbox(grid)

    # Get limit so scale it's center on 0:
    limit <- max(abs(spatial_div_per_cell_df$Div_metric)) * c(-1, 1)

    # Plot the map:

    if (plot_title == FALSE) {

      plot_map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = land_mask, fill = "lightgrey", color = "#636363") +
        ggplot2::geom_sf(data= spatial_div_per_cell_df, ggplot2::aes(fill = Div_metric),
                         color = "lightgrey") +
        ggplot2::scale_fill_distiller(type = "div", limit = limit) +
        ggplot2::theme(legend.position = "right")  +
        ggplot2::theme(legend.title = ggplot2::element_text(size = 12,
                                                            face = "bold")) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                       axis.title = ggplot2::element_text(size = 12,
                                                          face = "bold")) +
        ggplot2::labs(x = "Longitude", y = "Latitude",
                      fill = metric_nm) +
        ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "lightgrey",
                                                                linewidth = 0.1)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                                colour = "darkgrey"))+
        ggplot2::coord_sf(xlim = c(box_m["xmin"], box_m["xmax"]),
                          ylim = c(box_m["ymin"], box_m["ymax"]))

    }


    # if plot title is TRUE:
    if (plot_title == TRUE) {

      plot_map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = land_mask, fill = "lightgrey", color = "#636363") +
        ggplot2::geom_sf(data= spatial_div_per_cell_df, ggplot2::aes(fill = Div_metric),
                         color = "lightgrey") +
        ggplot2::scale_fill_distiller(type = "div", limit = limit) +
        ggplot2::theme(legend.position = "right")  +
        ggplot2::theme(legend.title = ggplot2::element_text(size = 12,
                                                            face = "bold")) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                       axis.title = ggplot2::element_text(size = 12,
                                                          face = "bold")) +
        ggplot2::labs(x = "Longitude", y = "Latitude",
                      fill = metric_nm) +
        ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "lightgrey",
                                                                linewidth = 0.1)) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                                colour = "darkgrey"))+
        ggplot2::coord_sf(xlim = c(box_m["xmin"], box_m["xmax"]),
                          ylim = c(box_m["ymin"], box_m["ymax"])) +

            ggplot2::ggtitle(paste0(div_facet_nm, sep = " - ",
                                    unique(div_per_cell_df$Taxon)))

    }

  # Print the plot:
  print(plot_map)

  # Save if needed:
  if (save == TRUE) {

    ggplot2::ggsave(plot = plot_map,
                    filename = here::here("outputs",
                                          paste0(unique(div_per_cell_df$Taxon),
                                                 sep = "_",
                                                 metric_nm,
                                                 sep = "_",
                                                 unique(div_per_cell_df$Grid),
                                                 sep = ".",
                                                 "pdf")),
                    device = "pdf",
                    scale = 1,
                    height = 3000,
                    width = 5000,
                    units = "px",
                    dpi = 600)
    ggplot2::ggsave(plot = plot_map,
                    filename = here::here("outputs",
                                          paste0(unique(div_per_cell_df$Taxon),
                                                 sep = "_",
                                                 metric_nm,
                                                 sep = "_",
                                                 unique(div_per_cell_df$Grid),
                                                 sep = ".",
                                                 "jpg")),
                    device = "jpg",
                    scale = 1,
                    height = 3000,
                    width = 5000,
                    units = "px",
                    dpi = 600)

  }

}



#' Maps drivers or residuals
#'
#' @param drivers_df the data frame with drivers values
#' @param driver_nm a character string referring to the driver to study
#' (named as in the associated column in \code{drivers_df})
#' @param grid a \code{sf} object referring to the spatial grid used, either
#' the 10x10km grid (WOODIV one) or the 50x50km one (INTEGRADIV one).
#' @param col_pal a vector gathering two colors for the gradient from low to high
#' @param land_mask a shapefile of europe land mask
#' @param type a character string being "residuals" is residuals are being
#' plotted - if not residuals, the function will automatically save the file as
#' a driver map
#' @param div_dim a character string to fill out if \code{type = "residuals},
#' to get the dimensions of diversity the map is referring to
#' (ex "PD_richness_TREES")
#' @param save TRUE or FALSE whether the plot is to be saved or not
#'
#' @return
#' @export
#'

drivers.maps.plot <- function(drivers_df,
                              driver_nm,
                              grid,
                              col_pal,
                              land_mask,
                              type,
                              div_dim,
                              save) {



  # Make sure that same format for the Idgrid column in div and grid objects:
  drivers_df <- dplyr::mutate(drivers_df,
                              Idgrid = as.character(Idgrid))
  grid <- dplyr::mutate(grid, Idgrid = as.character(Idgrid))

  # Link the div_per_cell_df and the grid so can have spatial info:
  spatial_driv_df <- dplyr::left_join(grid, drivers_df,
                                              by = "Idgrid")

  # Some rows have Div_metric = NA, remove them:
  spatial_driv_df <- dplyr::filter(spatial_driv_df,
                                  ! is.na(get(driver_nm)))

  # Create a box around the study area:
  box_m <- sf::st_bbox(grid)

  # Rename the driver so can take the given column:
  spatial_driv_df <- spatial_driv_df %>%
    dplyr::rename(driver_plot = driver_nm)

  # Plot the map:
  plot_map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = land_mask, fill = "lightgrey", color = "#636363") +
      ggplot2::geom_sf(data = spatial_driv_df, ggplot2::aes(fill = driver_plot),
                       color = "lightgrey") +
      ggplot2::scale_fill_gradient(low = col_pal[1], high = col_pal[2]) +
      ggplot2::theme(legend.position = "right")  +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 12,
                                                          face = "bold")) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                     axis.title = ggplot2::element_text(size = 12,
                                                        face = "bold")) +
      ggplot2::labs(x = "Longitude", y = "Latitude",
                    fill = "") +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "lightgrey",
                                                              linewidth = 0.1)) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "darkgrey"))+
      ggplot2::coord_sf(xlim = c(box_m["xmin"], box_m["xmax"]),
                        ylim = c(box_m["ymin"], box_m["ymax"])) +
      ggplot2::ggtitle(paste0("Distribution of", sep = " ", driver_nm))


  # Print the plot:
  print(plot_map)

  # Save if needed:
  if (save == TRUE) {

    # If it's a residual map:
    if (type == "residuals") {

      ggplot2::ggsave(plot = plot_map,
                      filename = here::here("outputs",
                                            "residuals_maps",
                                            paste0(driver_nm,
                                                   sep = "_",
                                                   div_dim,
                                                   sep = ".",
                                                   "pdf")),
                      device = "pdf",
                      scale = 1,
                      height = 3000,
                      width = 5000,
                      units = "px",
                      dpi = 600)
      ggplot2::ggsave(plot = plot_map,
                      filename = here::here("outputs",
                                            "residuals_maps",
                                            paste0(driver_nm,
                                                   sep = "_",
                                                   div_dim,
                                                   sep = ".",
                                                   "jpg")),
                      device = "jpg",
                      scale = 1,
                      height = 3000,
                      width = 5000,
                      units = "px",
                      dpi = 600)

    }
    # If it's a driver map:
    else {
      ggplot2::ggsave(plot = plot_map,
                      filename = here::here("outputs",
                                            "drivers_maps",
                                            paste0(driver_nm,
                                                   sep = ".",
                                                   "pdf")),
                      device = "pdf",
                      scale = 1,
                      height = 3000,
                      width = 5000,
                      units = "px",
                      dpi = 600)
      ggplot2::ggsave(plot = plot_map,
                      filename = here::here("outputs",
                                            "drivers_maps",
                                            paste0(driver_nm,
                                                   sep = ".",
                                                   "jpg")),
                      device = "jpg",
                      scale = 1,
                      height = 3000,
                      width = 5000,
                      units = "px",
                      dpi = 600)
    }

  }

}
