################################################################################
##
## Functions compute null models for FD and PD: 1000 new asb per grid cell ...
## ... and compute and plot SES values
##
## Camille Magneville
##
## 31/10/2023 - 10/2024
##
## Null_models_fcts.R
##
################################################################################





#' Compute null models for FD metrics
#'
#' @param sp_faxes_coord the array with species coordinates of functional axes -
#' with only the PC wanted. Saved in the \code{2_a_Compute_..._TD_FD_PD.R}.
#'
#' @param faxes_nm_vect vector containing the names of the functional axes to
#' consider
#'
#' @param sp_asb_df a data frame with species in columns and asb (grid cells)
#' in rows.
#'
#' @param nb_asb_rep a number with the number of null assemblages to compute for
#' each grid cell.
#'
#' @return a list of data frames - as many as FD metrics - with the grid cells
#' in rows and the values of the metric for each null asb in columns.
#'
#' @export
#'
#' @examples
#'

compute.null.model.FD.paral <- function(sp_faxes_coord,
                                  faxes_nm_vect,
                                  sp_asb_df,
                                  nb_asb_rep) {

  # Set Parallelisation
  # detect cores:
  core_nb <- parallel::detectCores()
  # setup cluster:
  cluster <- parallel::makeCluster(core_nb - 1)
  # register cluster:
  doParallel::registerDoParallel(cluster)
  # create a list to save the results:
  results_paral <- list()

  # Define:
  `%>%` <- magrittr::`%>%`

  # Create random assemblages: same sp richn as real grid cells but nb_asb_rep
  # ... different species composition:
  random_asb_array <- replicate(nb_asb_rep,
                                picante::randomizeMatrix(sp_asb_df,
                                                         null.model = "richness"))



  # launch paralellisation:
  results_paral <- foreach::foreach(i = 1:nb_asb_rep) %dopar% {

    # Retrieve the sp*null_asb dataframe on which to work:
    sp_null_asb_df <- random_asb_array[,,i]

    # Remove asb that do not have enough species fpr FRIc computation:
    sp_null_asb_subset_df <- sp_null_asb_df %>%
      as.data.frame() %>%
      dplyr::filter(rowSums(sp_null_asb_df) > ncol(sp_faxes_coord)) %>%
      as.matrix()

    # Compute indices for fmpd and FOri:
    alpha_fd_indices_1 <- mFD::alpha.fd.multidim(
      sp_faxes_coord   = sp_faxes_coord[, faxes_nm_vect],
      asb_sp_w         = sp_null_asb_df,
      ind_vect         = c("fmpd", "fori"),
      scaling          = TRUE,
      check_input      = TRUE,
      details_returned = TRUE,
      verbose          = TRUE)
    alpha_fd_indices_df <- alpha_fd_indices_1$functional_diversity_indices

    # Compute indices for FRic:
    alpha_fd_indices_2 <- mFD::alpha.fd.multidim(
      sp_faxes_coord   = sp_faxes_coord[, faxes_nm_vect],
      asb_sp_w         = sp_null_asb_subset_df,
      ind_vect         = c("fric"),
      scaling          = TRUE,
      check_input      = TRUE,
      details_returned = TRUE,
      verbose          = TRUE)
    alpha_fric_indices_df <- alpha_fd_indices_2$functional_diversity_indices

    results_paral[i] <- list(alpha_fd_indices_df,
                             alpha_fric_indices_df)

  }

  # Stop cluster:
  parallel::stopCluster(cl =  cluster)


  # Initialise three dataframes to return - one for each FD metric:

  # For FMPD:
  fmpd_df <- results_paral[[1]][[1]]
  fmpd_df <- tibble::rownames_to_column(fmpd_df,
                                        var = "Idgrid")
  # For FOri:
  fori_df <- fmpd_df[, c(1, 4)]
  fmpd_df <- fmpd_df[, c(1, 3)]
  # For FRic:
  fric_df <- results_paral[[1]][[2]]
  fric_df <- tibble::rownames_to_column(fric_df,
                                        var = "Idgrid")
  fric_df <- fric_df[, c(1, 3)]


  # Fill these dfs:
  for (i in c(2:nb_asb_rep)) {

    fmpd_tmp <- results_paral[[i]][[1]]
    fric_tmp <- results_paral[[i]][[2]]

    fmpd_tmp <- tibble::rownames_to_column(fmpd_tmp,
                                           var = "Idgrid")
    fric_tmp <- tibble::rownames_to_column(fric_tmp,
                                           var = "Idgrid")

    fori_tmp <- fmpd_tmp[, c(1, 4)]
    fmpd_tmp <- fmpd_tmp[, c(1, 3)]
    fric_tmp <- fric_tmp[, c(1, 3)]

    # Bind columns:
    fmpd_df <- dplyr::full_join(fmpd_df,
                                fmpd_tmp,
                                by = "Idgrid")
    fori_df <- dplyr::full_join(fori_df,
                                fori_tmp,
                                by = "Idgrid")
    fric_df <- dplyr::full_join(fric_df,
                                fric_tmp,
                                by = "Idgrid")

  }

  # Renames the columns of the three dfs:
  colnames(fmpd_df) <- c("Idgrid", paste0("null_asb",
                                          sep = "_",
                                          rep(1:nb_asb_rep)))
  colnames(fori_df) <- c("Idgrid", paste0("null_asb",
                                          sep = "_",
                                          rep(1:nb_asb_rep)))
  colnames(fric_df) <- c("Idgrid", paste0("null_asb",
                                          sep = "_",
                                          rep(1:nb_asb_rep)))


  return(list("fmpd" = fmpd_df,
              "fori" = fori_df,
              "fric" = fric_df))

}




#' Function to compute SES and Realised/Potential ratios based on null models
#'
#' @param null_model_df the data frame gathering for each  grid cell the
#' simulated values of a given metric. Grid cells are in row and null asb in
#' columns. It's the output of the \code{compute.null.models.FD} or
#' \code{compute.null.models.PD} functions.
#'
#' @param null_metric_to_compute a vector gathering character strings referring to
#' the metric to compute. Can be either \code{c("ses")}, \code{c("R/P")} or
#' \code{c("ses", "RP_ratio")}.
#'
#' @param ind_values_df a data frame gathering the real values of the PD or
#' FD metric computed.
#'
#' @return a list with one or two data frames (according to the number of
#' metrics to compute) gathering for each grid cell (rows), the metric value
#' (column).
#'
#' @export
#'
#' @examples
#'

compute.null.model.metrics <- function(null_model_df,
                                       null_metric_to_compute,
                                       ind_values_df) {


  # Add 3 new columns, compute mean and sd for each grid cell:
  tmp_null_model_df <- null_model_df %>%
    dplyr::mutate(mean = rowMeans(null_model_df)) %>%
    dplyr::mutate(sd = matrixStats::rowSds(as.matrix(null_model_df),
                                           na.rm = TRUE)) %>%
    dplyr::mutate(maxnull = matrixStats::rowMaxs(as.matrix(null_model_df),
                                                 na.rm = TRUE)) %>%
    as.data.frame()

  # Add column with real values of FD or PD metrics for each grid cell:
  tmp_null_model_df <- tmp_null_model_df %>%
    tibble::rownames_to_column(var = "Idgrid") %>%
    dplyr::left_join(ind_values_df,
                     by = "Idgrid")


  # if the SES is to be computed:
  if ("ses" %in% null_metric_to_compute) {
    final_null_model_df <- tmp_null_model_df %>%
      dplyr::mutate(ses = (metric - mean)/sd) %>%
      dplyr::select(c("Idgrid", "metric", "mean", "sd", "maxnull", "ses"))

    # Compute the rank of the observed value on the null values distrib:
    tmp_null_model_df <- tmp_null_model_df %>%
      dplyr::mutate(rank_obs = NA)

    for (i in (1:nrow(tmp_null_model_df))) {

      tmp_null_model_df$rank_obs[i] <- rank(c(tmp_null_model_df$metric[i],
                                              as.numeric(tmp_null_model_df[i,
                                                                           c(2:(ncol(null_model_df) + 1))])))[1]

    } # end fill information on the raking of the obs value on null distrib

    # Compute the pvalue of the SES based on the rank:
    tmp_null_model_df <- tmp_null_model_df %>%
      dplyr::mutate(pvalue = rank_obs/(ncol(null_model_df) + 1))

    # Add it to the final df:
    final_null_model_df <- final_null_model_df %>%
      dplyr::mutate(pvalues_ses = tmp_null_model_df$pvalue) %>%
      dplyr::mutate(rank_obs = tmp_null_model_df$rank_obs)

  }

  # if the R/P ratio is to be compute:
  if ("RP_ratio" %in% null_metric_to_compute) {
    final_null_model_df <- final_null_model_df %>%
      dplyr::mutate(RP_ratio = metric/maxnull)
  }

  return(final_null_model_df)

}





#' Compute null models for PD metrics
#'
#' @param phylo_tree a \code{tree} object linking all the species from the
#' studied pool of species and ONLY species from the studied global pool.
#'
#' @param sp_asb_df a data frame with species in columns and asb (grid cells)
#' in rows.
#'
#' @param nb_asb_rep a number with the number of null assemblages to compute for
#' each grid cell.
#'
#' @return a list of data frames - as many as PD metrics - with the grid cells
#' in rows and the values of the metric for each null asb in columns.
#'
#' @export
#'
#'

compute.null.model.PD <- function(phylo_tree,
                                  sp_asb_df,
                                  nb_asb_rep) {



  # Create random assemblages: same sp richn as real grid cells but nb_asb_rep
  # ... different species composition:
  set.seed(1)
  random_asb_array <- replicate(nb_asb_rep,
                                picante::randomizeMatrix(sp_asb_df,
                                                         null.model = "richness"))


  # Build the data frames which will be outputs:
  # For Faith's PD:
  faith_df <- as.data.frame(matrix(ncol = nb_asb_rep,
                                   nrow = nrow(sp_asb_df),
                                   NA))
  colnames(faith_df) <- paste0(rep("null_asb", nb_asb_rep), sep = "_",
                               c(1:nb_asb_rep))
  rownames(faith_df) <- rownames(sp_asb_df)

  # For MPD:
  mpd_df <- faith_df

  # For MNTD:
  mntd_df <- faith_df


  # Change labels because when save with write.tree, add "_":
  # Remove "_" between genus and species and add " " (same tr and occ data):
  phylo_tree$tip.label <- gsub("_", " ", phylo_tree$tip.label)


  # Compute general Faith PD for all branches:
  # Add a new row (asb) with all species occurring in it:
  sp_asb_all_df <- as.data.frame(sp_asb_df)
  sp_asb_all_df[nrow(sp_asb_all_df) + 1, ] <- 1
  rownames(sp_asb_all_df)[nrow(sp_asb_all_df)] <- "all_sp"
  PD_Faith_tot <- picante::pd(sp_asb_all_df[nrow(sp_asb_all_df), ],
                              phylo_tree)


  # Loop on each null assemblage:
  for (i in (1:nb_asb_rep)) {


    # Print a message:
    print(paste0("Start the", sep = " ", i,
                 sep = " ", "repetition over", sep = " ", nb_asb_rep))


    # Retrieve the sp*null_asb dataframe on which to work:
    sp_null_asb_df <- random_asb_array[,,i]


    ## Compute Faith's PD:
    faith_pd <- picante::pd(sp_null_asb_df,
                            phylo_tree)
    # Standardize by total branch length:
    faith_pd_relat <-faith_pd
    faith_pd_relat$PD <- faith_pd_relat$PD/PD_Faith_tot$PD


    ## Compute MPD:
    # Compute a matrix of cophenetic distances based on the phylogeny:
    dist_mat <- stats::cophenetic(phylo_tree)
    # Compute Mean Pairwise Distance (MPD) per asb:
    MPD_pd <- picante::mpd(samp = sp_null_asb_df,
                           dis = dist_mat,
                           abundance.weighted = FALSE)
    # Get the mean distance of each species to all its neighb from glob pool:
    mean_dist_sp <- apply(dist_mat, 1, mean, na.rm = TRUE)
    # Compute relative values:
    MPD_pd_relat <- MPD_pd
    MPD_pd_relat <- MPD_pd_relat/max(mean_dist_sp)


    ## Compute MNTD:
    # Compute Mean Pairwise Distance (MPD) per asb:
    MNTD_pd <- picante::mntd(samp = sp_null_asb_df,
                             dis = dist_mat,
                             abundance.weighted = FALSE)


    # Get the highest pairwise distance from all sp of the global pool:
    highest_pairw <- max(dist_mat)

    # Compute relative values:
    MNTD_pd_relat <- MNTD_pd
    MNTD_pd_relat <- MNTD_pd_relat/highest_pairw


    # Fill the Faith df:
    faith_tmp <- faith_pd_relat %>%
      tibble::rownames_to_column(var = "Idgrid") %>%
      dplyr::rename(metric = "PD") %>%
      dplyr::select(c("Idgrid", "metric"))
    faith_df[, i] <- faith_tmp$metric

    # Fill the MPD df:
    mpd_df[, i] <- MPD_pd_relat

    # Fill the mean(ED) df:
    mntd_df[, i] <- MNTD_pd_relat


  } # end loop on null assemblage rep


  return(list("faith" = faith_df,
              "mpd" = mpd_df,
              "mntd" = mntd_df))

}



#' Compute FD null models
#'
#' @param sp_faxes_coord a matrix gathering species coordinates in the
#' functional space - from the mFD package. Must have at least the same number
#' of dimensions than \code{faxes_nm_vect}.
#' @param faxes_nm_vect a vector gathering the names of functional axes to study.
#' They must be contained in the \code{sp_faxes_coord} matrix
#' @param sp_asb_df a data frame with species in columns and asb (grid cells)
#' in rows.
#' @param nb_asb_rep a number with the number of null assemblages to compute for
#' each grid cell.
#'
#' @return a list of data frames - as many as FD metrics - with the grid cells
#' in rows and the values of the metric for each null asb in columns. For FRic,
#' some values are NA because for some assemblages, to few species.
#'
#' @export
#'

compute.null.model.FD <- function(sp_faxes_coord,
                                  faxes_nm_vect,
                                  sp_asb_df,
                                  nb_asb_rep) {


  # Create random assemblages: same sp richn as real grid cells but nb_asb_rep
  # ... different species composition:
  set.seed(1)
  random_asb_array <- replicate(nb_asb_rep,
                                picante::randomizeMatrix(sp_asb_df,
                                                         null.model = "richness"))


  # Build the data frames which will be outputs:
  # For FMPD:
  fmpd_df <- as.data.frame(matrix(ncol = nb_asb_rep,
                                  nrow = nrow(sp_asb_df),
                                  NA))
  colnames(fmpd_df) <- paste0(rep("null_asb", nb_asb_rep), sep = "_",
                              c(1:nb_asb_rep))
  rownames(fmpd_df) <- rownames(sp_asb_df)

  # For FOri:
  fori_df <- fmpd_df

  # For FRic (in the end, it's reduced because some asb do not have enough sp):
  fric_df <- fmpd_df
  # Remove asb that do not have enough species for FRic computation:
  sp_asb_subset_df <- sp_asb_df %>%
    as.data.frame() %>%
    dplyr::filter(rowSums(sp_asb_df) > length(faxes_nm_vect)) %>%
    as.matrix()
  fric_df <- fric_df %>%
    dplyr::filter(rownames(fric_df) %in% rownames(sp_asb_subset_df))


  # Loop on each null assemblage:
  for (i in (1:nb_asb_rep)) {


    # Print a message:
    print(paste0("Start the", sep = " ", i,
                 sep = " ", "repetition over", sep = " ", nb_asb_rep))


    # Retrieve the sp*null_asb dataframe on which to work:
    sp_null_asb_df <- random_asb_array[,,i]

    # Remove asb that do not have enough species fpr FRic computation:
    sp_null_asb_subset_df <- sp_null_asb_df %>%
      as.data.frame() %>%
      dplyr::filter(rowSums(sp_null_asb_df) > length(faxes_nm_vect)) %>%
      as.matrix()

    # Compute indices for fmpd and FOri:
    alpha_fd_indices_1 <- mFD::alpha.fd.multidim(
      sp_faxes_coord   = sp_faxes_coord[, faxes_nm_vect],
      asb_sp_w         = sp_null_asb_df,
      ind_vect         = c("fmpd", "fori"),
      scaling          = TRUE,
      check_input      = TRUE,
      details_returned = TRUE,
      verbose          = TRUE)
    alpha_fd_indices_df <- alpha_fd_indices_1$functional_diversity_indices

    # Compute indices for FRic:
    alpha_fd_indices_2 <- mFD::alpha.fd.multidim(
      sp_faxes_coord   = sp_faxes_coord[, faxes_nm_vect],
      asb_sp_w         = sp_null_asb_subset_df,
      ind_vect         = c("fric"),
      scaling          = TRUE,
      check_input      = TRUE,
      details_returned = TRUE,
      verbose          = TRUE)
    alpha_fric_indices_df <- alpha_fd_indices_2$functional_diversity_indices


    # Fill the FMPD df:
    fmpd_tmp <- alpha_fd_indices_df %>%
      tibble::rownames_to_column(var = "Idgrid") %>%
      dplyr::rename(metric = "fmpd") %>%
      dplyr::select(c("Idgrid", "metric"))
    fmpd_df[, i] <- fmpd_tmp$metric

    # Fill the FOri df:
    fori_tmp <- alpha_fd_indices_df %>%
      tibble::rownames_to_column(var = "Idgrid") %>%
      dplyr::rename(metric = "fori") %>%
      dplyr::select(c("Idgrid", "metric"))
    fori_df[, i] <- fori_tmp$metric

    # Fill the FRic df:
    fric_tmp <- alpha_fric_indices_df %>%
      tibble::rownames_to_column(var = "Idgrid") %>%
      dplyr::rename(metric = "fric") %>%
      dplyr::select(c("Idgrid", "metric"))
    fric_df[, i] <- fric_tmp$metric


  } # end loop on null assemblage rep


  return(list("fmpd" = fmpd_df,
              "fori" = fori_df,
              "fric" = fric_df))

}

