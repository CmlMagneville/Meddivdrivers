################################################################################
##
## Script to gather functions to complete missing traits based on Genus data
##
## Camille Magneville
##
## 09/2024
##
## Impute_missing_traits_fcts.R
##
################################################################################


#' Function to complete the taxonomic data frame if data already
#' gathered on missing Genus
#'
#' @param taxo_df the data frame output of the \code{taxize::tax_name()} function
#' @param missing_sp_vect a vector containing the name of species with missing
#' taxonomic data
#'
#' @return the taxonoic dataframe completed where it can be.
#'
#' @export
#'

check.genus <- function(taxo_df,
                        missing_sp_vect) {


  # Loop on species with missing information:
  for (sp in missing_sp_vect) {

    # Retrieve Genus:
    genus_nm <- sub("_.*", "", sp)

    # Check if the Genus is already present in the taxo_df:
    if (! genus_nm %in% taxo_df$genus) {
      print(paste0("No information on genus", sep = " ",
                   genus_nm))
    }
    if(genus_nm %in% taxo_df$genus) {
      print(paste0("Information completed for", sep = " ",
                   sp))
      family_nm <- unique(taxo_df$family[which(taxo_df$genus == genus_nm)])
      order_nm <- unique(taxo_df$order[which(taxo_df$genus == genus_nm)])
      taxo_df[which(taxo_df$query == sp), c("genus", "family", "order")] <- c(genus_nm,
                                                                              family_nm,
                                                                              order_nm)
    }

  } # end loop on species with missing information

  return(taxo_df)

}


#' Compute mean/mode at the closest taxonomic level with information to impute
#' missing values
#'
#' @param sp_tr_NA_df
#' @param taxo_df
#' @param traits_NA_nms
#'
#' @return
#' @export
#'

impute.missing.traits <- function(sp_tr_NA_df,
                                  taxo_df,
                                  traits_NA_nms) {


  # Add taxo info in the trait data:
  sp_tr_NA_taxo_df <- sp_tr_NA_df %>%
    tibble::rownames_to_column(var = "query") %>%
    dplyr::left_join(taxo_df, by = "query") %>%
    # Remove db column:
    dplyr::select(-c("db")) %>%
    # Rename column Species
    dplyr::rename("Species" = query) %>%
    dplyr::distinct()

  # Loop on traits to impute:
  for (trait in traits_NA_nms) {

    # Build a df with only this trait:
    trait_df <- sp_tr_NA_taxo_df[, c("Species", "genus", "family", "order", trait)]
    # Longer format for this given trait:
    trait_long_df <- trait_df %>%
      tidyr::pivot_longer(! c(Species, genus, family, order),
                          names_to = "trait", values_to = "value")
    # Get the names of the species with missing value on this given trait:
    missing_sp <- trait_long_df$Species[which(is.na(trait_long_df$value))]

    # Compute the mean or mode for each Genus:
    if (is.numeric(trait_long_df$value)) {

      value_genus <- trait_long_df %>%
        dplyr::group_by(genus, trait) %>%
        dplyr::mutate_at("value", as.numeric) %>%
        dplyr::summarise(mean = mean(value, na.rm = T),
                         sd = sd(value, na.rm = T),
                         n = dplyr::n()) %>%
        reshape::cast(genus ~ trait, value = 'mean') %>%
        as.data.frame()

    }
    if (! is.numeric(trait_long_df$value)) {

      # Define a fct to compute the mode:
      mode.fct <- function(){
        count_df <- dplyr::add_count(trait_long_df, value)
        max_n <- max(count_df$n)
        unique(count_df$value[which(count_df$n == max_n)])
      }

      value_genus <- trait_long_df %>%
        dplyr::group_by(genus, trait) %>%
        dplyr::mutate_at("value", as.numeric) %>%
        dplyr::summarise(mode = mode.fct()) %>%
        reshape::cast(genus ~ trait, value = 'mode') %>%
        as.data.frame()

    }


    # Compute the mean or mode for each Family:
    if (is.numeric(trait_long_df$value)) {

      value_family <- trait_long_df %>%
        dplyr::group_by(family, trait) %>%
        dplyr::mutate_at("value", as.numeric) %>%
        dplyr::summarise(mean = mean(value, na.rm = T),
                         sd = sd(value, na.rm = T),
                         n = dplyr::n()) %>%
        reshape::cast(family ~ trait, value = 'mean') %>%
        as.data.frame()

    }
    if (! is.numeric(trait_long_df$value)) {

      # Define a fct to compute the mode:
      mode.fct <- function(){
        count_df <- dplyr::add_count(trait_long_df, value)
        max_n <- max(count_df$n)
        unique(count_df$value[which(count_df$n == max_n)])
      }

      value_family <- trait_long_df %>%
        dplyr::group_by(family, trait) %>%
        dplyr::mutate_at("value", as.numeric) %>%
        dplyr::summarise(mode = mode.fct()) %>%
        reshape::cast(family ~ trait, value = 'mode') %>%
        as.data.frame()

    }


    # Compute the mean or mode for each Order:
    if (is.numeric(trait_long_df$value)) {

      value_order <- trait_long_df %>%
        dplyr::group_by(order, trait) %>%
        dplyr::mutate_at("value", as.numeric) %>%
        dplyr::summarise(mean = mean(value, na.rm = T),
                         sd = sd(value, na.rm = T),
                         n = dplyr::n()) %>%
        reshape::cast(order ~ trait, value = 'mean') %>%
        as.data.frame()

    }
    if (! is.numeric(trait_long_df$value)) {

      # Define a fct to compute the mode:
      mode.fct <- function(){
        count_df <- dplyr::add_count(trait_long_df, value)
        max_n <- max(count_df$n)
        unique(count_df$value[which(count_df$n == max_n)])
      }

      value_order <- trait_long_df %>%
        dplyr::group_by(order, trait) %>%
        dplyr::mutate_at("value", as.numeric) %>%
        dplyr::summarise(mode = mode.fct()) %>%
        reshape::cast(order ~ trait, value = 'mode') %>%
        as.data.frame()

    }

    # Loop on missing species:
    for (sp in missing_sp) {

      # Get Genus nm:
      genus_nm <- sp_tr_NA_taxo_df$genus[which(sp_tr_NA_taxo_df$Species == sp)]
      # Get Family nm:
      family_nm <- sp_tr_NA_taxo_df$family[which(sp_tr_NA_taxo_df$Species == sp)]
      # Get Order nm:
      order_nm <- sp_tr_NA_taxo_df$order[which(sp_tr_NA_taxo_df$Species == sp)]


      # Check if there is information on this trait at the Genus level:
      if (! is.na(value_genus[which(value_genus$genus == genus_nm), trait])) {

        sp_tr_NA_df[which(rownames(sp_tr_NA_df) == sp), trait] <- value_genus[which(value_genus$genus == genus_nm), trait]
        print(paste0("Information added at Genus level for",
                     sep = " ",
                     sp,
                     sep = " ",
                     "and",
                     sep = " ",
                     trait))
      } # end - if information available at Genus level


      # If there is no information on this trait at Genus level:
      if (is.na(value_genus[which(value_genus$genus == genus_nm), trait])) {


        # Check if info available at Family level:
        if (! is.na(value_family[which(value_family$family == family_nm), trait])) {

          sp_tr_NA_df[which(rownames(sp_tr_NA_df) == sp), trait] <- value_family[which(value_family$family == family_nm), trait]
          print(paste0("Information added at Family level for",
                       sep = " ",
                       sp,
                       sep = " ",
                       "and",
                       sep = " ",
                       trait))
        } # end - if information available at Family level


        # If there is no info available at Family level:
        if (is.na(value_family[which(value_family$family == family_nm), trait])) {


          # Check if info at Order level:
          if (! is.na(value_order[which(value_order$order == order_nm), trait])) {

            sp_tr_NA_df[which(rownames(sp_tr_NA_df) == sp), trait] <- value_order[which(value_order$order == order_nm), trait]
            print(paste0("Information added at Order level for",
                         sep = " ",
                         sp,
                         sep = " ",
                         "and",
                         sep = " ",
                         trait))
          } # end - if information available at Order level

          # If no info available at Order level:
          if (is.na(value_order[which(value_order$order == order_nm), trait])) {

            print(paste0("NO INFORMATION AT GENUS, FAMILY AND ORDER LEVELS FOR",
                         sep = " ",
                         sp,
                         sep = " ",
                         "and",
                         sep = " ",
                         trait))

          } # end  if no information at Order level

        } # end - if no information available at Family level

      } # end - if no information available at Genus level

    } # Loop on Species

  } # Loop on traits

  return(sp_tr_completed_df = sp_tr_NA_df)

}
