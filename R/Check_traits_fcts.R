################################################################################
##
## Function to compute correlations between traits
##
## Camille Magneville
##
## 13/12/2023 - 10/2024
##
## Check_traits_fcts.R
##
################################################################################


#' Get names of species which have extreme traits values
#'
#' @param traits_df species*traits data frame with species as a column
#' @param quant_traits a vector containing the name of quantitative traits
#'
#' @return
#' @export
#'

check.quantiles <- function(traits_df,
                           quant_traits_vect) {


  # Build a dataframe which will contain speciesnm for each tr:
  quantiles_df <- as.data.frame(matrix(ncol = 4, nrow = 1, NA))
  colnames(quantiles_df) <- c("trait_nm", "trait_value", "species_nm", "quantile")


  # For each trait:
  for (i in c(2:ncol(traits_df))) {

    # Get the trait name and check quantitative:
    tr_nm <- colnames(traits_df)[i]

    if (tr_nm %in% quant_traits_vect) {

      # Check that the trait is coded as numerical:
      if (! is.numeric(traits_df[, i][[1]])) {

        traits_df[, i][[1]] <- as.numeric(traits_df[, i][[1]])

      }

      # Get the quantiles values:
      quant99 <- quantile(traits_df[, i][[1]],
                          probs = 99/100,
                          na.rm = TRUE)
      quant1 <- quantile(traits_df[, i][[1]],
                          probs = 1/100,
                         na.rm = TRUE)

      # Get the names of species over 99 quantile for this trait:
      quant99_sp <- traits_df$Species[which(traits_df[, i][[1]] > quant99)]
      quant1_sp <- traits_df$Species[which(traits_df[, i][[1]] < quant1)]

      # get the values of each trait for each species:
      tr_val <- c(traits_df[which(traits_df$Species %in% quant99_sp),i][[1]],
                  traits_df[which(traits_df$Species %in% quant1_sp),i][[1]])

      # Complete the dataframe:
      to_add <- as.data.frame(matrix(ncol = 4, nrow = length(quant99_sp)+
                                                      length(quant1_sp),
                              NA))
      colnames(to_add) <- colnames(quantiles_df)
      to_add$trait_nm <- rep(tr_nm, nrow(to_add))

      to_add$trait_value <- tr_val
      to_add$species_nm <- c(quant99_sp, quant1_sp)
      to_add$quantile <- c(rep("quant99", length(quant99_sp)),
                           rep("quant1", length(quant1_sp)))


      quantiles_df <- dplyr::bind_rows(quantiles_df,
                                       to_add)

    } # if trait is a quantitative

  } # end loop on traits


  return(quantiles_df[-1, ])

}

#' Compute Cramer's V on a set of traits
#'
#' @param sp_tr_df dataframe gathering species in rows and traits in columns
#' @param var_nm a vector gathering the name of the traits to study (so only
#' categorical ones)
#'
#' @return a dataframe with Cramer's V values for eauch pair of trait
#' @export
#'
#' @examples
#'

cramerv.compute <- function(sp_tr_df, var_nm) {

  # Build an empty dataframe with the traits names in rows and columns:
  cramerv_df <- as.data.frame(matrix(ncol = length(var_nm),
                                     nrow = length(var_nm),
                                     NA))
  colnames(cramerv_df) <- var_nm
  rownames(cramerv_df) <- var_nm

  # Fill the df by computing cramer's v for each pair of traits:
  for (i in (1:ncol(cramerv_df))) {

    for (j in (1:nrow(cramerv_df))) {

      # if we are not comparing the same variables:
      if (i != j) {

        var1 <- dplyr::select(sp_tr_df,
                              colnames(cramerv_df)[i])
        var2 <- dplyr::select(sp_tr_df,
                              colnames(cramerv_df)[j])
        value <- rcompanion::cramerV(var1[[1]], var2[[1]])

        cramerv_df[i, j] <- value
      }

    }

  }

  return(cramerv_df)

}
