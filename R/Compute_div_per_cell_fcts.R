################################################################################
##
## Fcts to compute taxonomic, functional and phylogenetic diversities ...
## ... per grid cell
##
## Camille Magneville
##
## 03/10/2023
##
## Compute_div_per_cell_fcts.R
##
################################################################################





#' Compute Taxonomic Diversity At A Given Scale
#'
#' @param occ_df a data frame with the spatial cells in rows and the following
#' columns: \strong{Taxon} with Taxon name (should be the same for all df as
#' one taxon is studied here), \strong{Species} with species name
#' (Genus_species_subspecies), \strong{Region} with Region's name (should be the
#' same for all df as one area studied here), \strong{Grid} with either
#' \code{50x50} or \code{10x10} values as they are the two different scales
#' studied and \code{Idgrid} with the identity of each cell (Note: 50x50km and
#' 10x10km cells have different format of ID).
#'
#' @param scale a character string referring to the scale at which the analysis
#' should be realised. Could be either \code{10x10} or \code{50x50}.
#'
#' @return a data frame with the values of taxonomic diversity (here species
#' richness) per cell of the chosen grid (Chosen grid reminded as a column),
#' with a column referring to the Taxon studied (should be the same for all df as
#' one taxon is studied here)
#'
#' @export
#'
#' @examples
#'


compute.TD.per.cell <- function(occ_df,
                                scale) {


  # Define the pipe symbol so I can use it:
  `%>%` <- magrittr::`%>%`

  # Only keep the cells referring to the studied scale:
  cell_occ_df <- dplyr::filter(occ_df, Grid == scale)

  # Get species richness per cell:
  sp_richn_df <- cell_occ_df %>%
                 dplyr::group_by(Idgrid) %>%
                 dplyr::summarize(sp_richness = dplyr::n())

  # Add a column with the scale information:
  sp_richn_df <- dplyr::mutate(sp_richn_df,
                               Grid = scale)

  # Add a column with the taxon information:
  sp_richn_df <- dplyr::mutate(sp_richn_df,
                               Taxon = unique(occ_df$Taxon))

  # Return the species richness df:
  return(sp_richn_df)

}







#' Compute phylogenetic indices
#'
#' @param sp_asb_df a dataframe linking species (columns) and cells (rows) for
#' a given scale
#' @param phylo a phylogeny as a phylo object with only species present in the
#' \code{sp_asb_df}
#' @param metric_nm a character string refering to the name of the metric to be
#' computed, could be either: "Faith_index", "MPD", "meanED".
#' Faith index is computed through the \code{picante::pd()} function. MPD
#' metric is computed throught the \code{picante::mpd()} , mean(ED) is computed
#' through the \code{picante::evol.distinct} as the mean of ED for the studied
#' subset of species (ED computed on the global tree).
#' @param grid a character string referring to the scale used. Can either be
#' "50x50" or "10x10".
#' @param taxon a character string referring to the taxa studied. Can either be
#' "Trees", "Birds" or "Butterflies".
#'
#' @return a data frae with cells as rows and the following columns:
#' \code{Idgrid}, \code{metric}, \code{Grid}, \code{Taxon}. Can be used to plot
#' the metric thorugh the \code{div.maps.plot} function


compute.PD.per.cell <- function(sp_asb_df,
                                phylo,
                                metric_nm,
                                grid,
                                taxon) {

  # Remove "_" between genus and species and add " " (same tr and occ data):
  phylo$tip.label <- gsub("_", " ", phylo$tip.label)

  # Compute Faith's PD if asked:
  if (metric_nm == "Faith_index") {

    # Compute general PD for all branches:
    # Add a new row (asb) with all species occurring in it:
    sp_asb_all_df <- as.data.frame(sp_asb_df)
    sp_asb_all_df[nrow(sp_asb_all_df) + 1, ] <- 1
    rownames(sp_asb_all_df)[nrow(sp_asb_all_df)] <- "all_sp"

    PD_Faith_values_all <- picante::pd(sp_asb_all_df,
                                       phylo)

    # Compute relative values according to total PD:
    tot_PD_value <- PD_Faith_values_all[nrow(PD_Faith_values_all), "PD"]
    PD_relat_Faith_df <- PD_Faith_values_all[-nrow(PD_Faith_values_all), ]
    PD_relat_Faith_df$PD <- PD_relat_Faith_df$PD/tot_PD_value

    # Compute the final df to return:
    final_df <- PD_relat_Faith_df %>%
      # Add cells as rownames:
      tibble::rownames_to_column(var = "Idgrid") %>%
      # Rename column with Faith values from PD -> metric
      dplyr::rename(metric = PD) %>%
      # Remove the SR column:
      dplyr::select(c(Idgrid, metric)) %>%
      # Add column for scale:
      dplyr::mutate(Grid = grid) %>%
      # Add column for taxon:
      dplyr::mutate(Taxon = taxon)
  }


  if (metric_nm == "MPD") {

    # Compute a matrix of cophenetic distances based on the phylogeny:
    dist_mat <- stats::cophenetic(phylo)
    # Compute Mean Pairwise Distance (MPD) per asb:
    MPD_values <- picante::mpd(samp = sp_asb_df,
                               dis = dist_mat,
                               abundance.weighted = FALSE)

    # Get the mean distance of each species to all its neighb from glob pool:
    mean_dist_sp <- apply(dist_mat, 1, mean, na.rm = TRUE)

    # Compute the final df to return:
    final_df <- sp_asb_df %>%
      as.data.frame() %>%
      # Add cells as rownames:
      tibble::rownames_to_column(var = "Idgrid") %>%
      # Add column for metric:
      dplyr::mutate(metric = MPD_values / max(mean_dist_sp)) %>%
      # Remove the SR column:
      dplyr::select(c(Idgrid, metric)) %>%
      # Add column for scale:
      dplyr::mutate(Grid = grid) %>%
      # Add column for taxon:
      dplyr::mutate(Taxon = taxon)

  }

  if (metric_nm == "MNTD") {

    # Compute a matrix of cophenetic distances based on the phylogeny:
    dist_mat <- stats::cophenetic(phylo)
    # Compute Mean Pairwise Distance (MPD) per asb:
    MNTD_values <- picante::mntd(samp = sp_asb_df,
                               dis = dist_mat,
                               abundance.weighted = FALSE)


    # Get the highest pairwise distance from all sp of the global pool:
    highest_pairw <- max(dist_mat)


    # Compute the final df to return:
    final_df <- sp_asb_df %>%
      as.data.frame() %>%
      # Add cells as rownames:
      tibble::rownames_to_column(var = "Idgrid") %>%
      # Add column for metric:
      dplyr::mutate(metric = MNTD_values/highest_pairw) %>%
      # Remove the SR column:
      dplyr::select(c(Idgrid, metric)) %>%
      # Add column for scale:
      dplyr::mutate(Grid = grid) %>%
      # Add column for taxon:
      dplyr::mutate(Taxon = taxon)

  }

  return(final_df)

}



