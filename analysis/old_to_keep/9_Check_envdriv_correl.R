################################################################################
##
## Script to check correlation among env drivers and chose a subset of
## ... uncorrelated variables
##
## Camille Magneville
##
## 01/05/2024
##
## 8_Check_envdriv_correl.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load the environmental drivers ===========================================


envdriv_full_db <- readRDS(here::here("transformed_data",
                                      "env_db",
                                      "env_drivers_final_noNA_db.rds"))

# 2 - Compute pairwise correlation============================================


# Make sure each column is numeric:
envdriv_full_db[, -1] <- apply(envdriv_full_db[, -1], 2, as.numeric)


# Compute correlation matrix between all env drivers:
cor_matrix <- Hmisc::rcorr(as.matrix(envdriv_full_db[, -1]),
                           type = "spearman")
correl_df <- as.data.frame(cor_matrix$r)
pvalues_correl_df <- as.data.frame(cor_matrix$P)

# Put variables names back (checked the code, it respects var order):
colnames(correl_df) <- colnames(envdriv_full_db)[-1]
rownames(correl_df) <- colnames(envdriv_full_db)[-1]
colnames(pvalues_correl_df) <- colnames(envdriv_full_db)[-1]
rownames(pvalues_correl_df) <- colnames(envdriv_full_db)[-1]

# Format so can link correl and pvalues:
full_correl_df <- reshape2::melt(as.matrix(correl_df)) %>%
  dplyr::rename(Correl = value) %>%
  dplyr::mutate("Comb" = paste0(Var1, sep = "_", Var2))
full_pvalue_df <- reshape2::melt(as.matrix(pvalues_correl_df)) %>%
  dplyr::rename(Pvalue = value) %>%
  dplyr::mutate("Comb" = paste0(Var1, sep = "_", Var2))

# Create a final df with pvalues and spearman correl:
full_correl_pvalue_df <- dplyr::left_join(full_correl_df,
                                          full_pvalue_df[, -c(1, 2)],
                                          by = "Comb") %>%
  dplyr::select(c("Var1", "Var2", "Correl", "Pvalue"))


# Get the most correlated variables: BE CAREFUL 1 pair + 2 rows
correl_70 <- subset(full_correl_pvalue_df, Correl > .70 & Correl != 1)


# 3 - Visualize correlation between envdrivers =================================


# Note: Between variables of a specific type of drivers and then by
# ... pairs of environmental drivers

# Soil, Topo and Present climate : present habitat characteritics:
GGally::ggpairs(envdriv_full_db[, c(2:11, 20:25)],
                upper = list(continuous = GGally::wrap("cor",
                                                       method = "spearman")))



# including correlated predictors in the SEM? In principle, they are not
# ... super-sensitive to that, but I would keep these correlated predictors
# ... to the very minimum you can afford to tests your hypotheses. It is ok to
# ... include average and SDev, even if they are correlated, just not too many
# ... of those, otherwise the results become quite volatile and not very
# ... trustworthy









