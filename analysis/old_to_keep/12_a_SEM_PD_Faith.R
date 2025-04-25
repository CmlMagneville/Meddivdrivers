################################################################################
##
## Script to study the relationships between drivers and PD Faith for all taxa
## and do SEM for birds and then for all taxa
##
## Camille Magneville
##
## 16/05/2024 - 07/2024
##
## 11_a_SEM_PD_Faith.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load environmental data and PD Faith =====================================


# Load environmental drivers - PCA synthetic var (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
driv_db <- readRDS(here::here("transformed_data", "env_db",
                                      "SEM_env_db.rds"))

# Load SES PD - Faith:
faith_ses_birds_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_BIRDS.rds"))
faith_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_TREES.rds"))
faith_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                            "PD_Faith_null_models_metrics_50km_REPTILES.rds"))


# 2 - Build a df to study relationships ========================================


# Link environmental db and ses for all taxa:
relationsh_ses_faith_df <- driv_db %>%
  dplyr::left_join(faith_ses_birds_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_birds" = ses) %>%
  dplyr::left_join(faith_ses_reptiles_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_reptiles" = ses) %>%
  dplyr::left_join(faith_ses_trees_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_trees" = ses)


# 3 - Correlations between all drivers variables ===============================

cor_matrix <- Hmisc::rcorr(as.matrix(driv_db[, -1]),
                           type = "spearman")
correl_df <- as.data.frame(cor_matrix$r)
pvalues_correl_df <- as.data.frame(cor_matrix$P)

# Put variables names back (checked the code, it respects var order):
colnames(correl_df) <- colnames(driv_db)[-1]
rownames(correl_df) <- colnames(driv_db)[-1]
colnames(pvalues_correl_df) <- colnames(driv_db)[-1]
rownames(pvalues_correl_df) <- colnames(driv_db)[-1]

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
correl_neg70 <- subset(full_correl_pvalue_df, Correl < -.70 & Correl != -1)



# 4 - Relationship between synthetic variables for past stab and Faith PD ======


# Note shapiro test: as sample sizes grow, increasingly trivial
# ... departures from normality (which are almost always present in real data)
# ... will result in small p-values.
# ... For this reason, visual tests are more useful.

# Note GLM: Assumptions are about the RESIDUALS, not the actual varaibles


# Histograms for drivers var:
hist(driv_db$PastClimStab_dim1)
hist(driv_db$PastClimStab_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)

# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastClimStab_dim1)
ggplot2::ggplot(data = relationsh_ses_faith_df,
                ggplot2::aes(x=ses_reptiles,
                             y = PastClimStab_dim1)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth()

plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastClimStab_dim2)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastClimStab_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastClimStab_dim2)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastClimStab_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastClimStab_dim2)


# BIRDS - GLM:

# Test Linear Regression - dim1:
birds_pastclim1_lm <- lm(ses_birds ~ PastClimStab_dim1,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_pastclim1_lm)
# Test normality residuals: OK
shapiro.test(rstandard(birds_pastclim1_lm)) # H0: normal distrib
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_pastclim1_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_pastclim1_lm)
car::Anova(birds_pastclim1_lm)

# Test Linear Regression - dim2:
birds_pastclim2_lm <- lm(ses_birds ~ PastClimStab_dim2,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_pastclim2_lm)
# Test normality residuals:
shapiro.test(rstandard(birds_pastclim2_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_pastclim2_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_pastclim2_lm)
car::Anova(birds_pastclim2_lm)

# Test Linear Regression - dim 1 + dim 2:
birds_pastclim_lm <- lm(ses_birds ~ PastClimStab_dim1+
                                    PastClimStab_dim2,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_pastclim_lm)
# Test normality residuals: OK
shapiro.test(rstandard(birds_pastclim_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_pastclim_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_pastclim_lm)
car::Anova(birds_pastclim_lm)


# REPTILES - GLM:

# Test Linear Regression - dim1:
reptiles_pastclim1_lm <- lm(ses_reptiles ~ PastClimStab_dim1,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_pastclim1_lm)
# Test normality residuals: OK
shapiro.test(rstandard(reptiles_pastclim1_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_pastclim1_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_pastclim1_lm)
car::Anova(reptiles_pastclim1_lm)

# Test Linear Regression - dim2:
reptiles_pastclim2_lm <- lm(ses_reptiles ~ PastClimStab_dim2,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_pastclim2_lm)
# Test normality residuals:
shapiro.test(rstandard(reptiles_pastclim2_lm)) # Not normal but check visual cues
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_pastclim2_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_pastclim2_lm)
car::Anova(reptiles_pastclim2_lm)

# Test Linear Regression - dim 1 + dim 2:
reptiles_pastclim_lm <- lm(ses_reptiles ~ PastClimStab_dim1+
                          PastClimStab_dim2,
                        data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_pastclim_lm)
# Test normality residuals: OK
shapiro.test(rstandard(reptiles_pastclim_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_pastclim_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_pastclim_lm)
car::Anova(reptiles_pastclim_lm)


# TREES - GLM:

# Test Linear Regression - dim1:
trees_pastclim1_lm <- lm(ses_trees ~ PastClimStab_dim1,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_pastclim1_lm)
# Test normality residuals: OK
shapiro.test(rstandard(trees_pastclim1_lm)) # NOT NORMAL - check visual cues
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_pastclim1_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed() # STILL NO
# Model summary:
summary(trees_pastclim1_lm)
car::Anova(trees_pastclim1_lm)

# Test Linear Regression - dim2:
trees_pastclim2_lm <- lm(ses_trees ~ PastClimStab_dim2,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_pastclim2_lm)
# Test normality residuals:
shapiro.test(rstandard(trees_pastclim2_lm)) # NOT NORMAL - check visual cues
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_pastclim2_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed() # STILL NO
# Model summary:
summary(trees_pastclim2_lm)
car::Anova(trees_pastclim2_lm)

# Test Linear Regression - dim 1 + dim 2:
trees_pastclim_lm <- lm(ses_trees ~ PastClimStab_dim1+
                             PastClimStab_dim2,
                           data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_pastclim_lm)
# Test normality residuals: OK
shapiro.test(rstandard(trees_pastclim_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_pastclim_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_pastclim_lm)
car::Anova(trees_pastclim_lm)


# 5 - Relationship between synthetic variables for present clim mean and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentClimMean_dim1)
hist(driv_db$PresentClimMean_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentClimMean_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentClimMean_dim2)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentClimMean_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentClimMean_dim2)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentClimMean_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentClimMean_dim2)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_presclimmean_lm <- lm(ses_birds ~ PresentClimMean_dim1 +
                             PresentClimMean_dim2,
                        data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_presclimmean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(birds_presclimmean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_presclimmean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_presclimmean_lm)
car::Anova(birds_presclimmean_lm)


# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2:
reptiles_presclimmean_lm <- lm(ses_reptiles ~ PresentClimMean_dim1 +
                              PresentClimMean_dim2,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_presclimmean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(reptiles_presclimmean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_presclimmean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_presclimmean_lm)
car::Anova(reptiles_presclimmean_lm)


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2:
trees_presclimmean_lm <- lm(ses_trees ~ PresentClimMean_dim1 +
                                 PresentClimMean_dim2,
                               data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_presclimmean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_presclimmean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_presclimmean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_presclimmean_lm)
car::Anova(trees_presclimmean_lm)


# 6 - Relationship between synthetic variables for present clim sd and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentClimSd_dim1)
hist(driv_db$PresentClimSd_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentClimSd_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentClimSd_dim2)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentClimSd_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentClimSd_dim2)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentClimSd_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentClimSd_dim2)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_presclimsd_lm <- lm(ses_birds ~ PresentClimSd_dim1 +
                              PresentClimSd_dim2,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_presclimsd_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_presclimsd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_presclimsd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_presclimsd_lm)
car::Anova(birds_presclimsd_lm)


# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2:
reptiles_presclimsd_lm <- lm(ses_reptiles ~ PresentClimSd_dim1 +
                                 PresentClimSd_dim2,
                               data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_presclimsd_lm)
# Test normality residuals: NO
shapiro.test(rstandard(reptiles_presclimsd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_presclimsd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_presclimsd_lm)
car::Anova(reptiles_presclimsd_lm)


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2:
trees_presclimsd_lm <- lm(ses_trees ~ PresentClimSd_dim1 +
                              PresentClimSd_dim2,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_presclimsd_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_presclimsd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_presclimsd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_presclimsd_lm)
car::Anova(trees_presclimsd_lm)


# 7 - Relationship between synthetic variables for hab mean and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentHabMean_dim1)
hist(driv_db$PresentHabMean_dim2)
hist(driv_db$PresentHabMean_dim3)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabMean_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabMean_dim2)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabMean_dim3)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabMean_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabMean_dim2)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabMean_dim3)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabMean_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabMean_dim2)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabMean_dim3)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
birds_PresentHabMean_lm <- lm(ses_birds ~ PresentHabMean_dim1 +
                                PresentHabMean_dim2 +
                                PresentHabMean_dim3,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_PresentHabMean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(birds_PresentHabMean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_PresentHabMean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_PresentHabMean_lm)
car::Anova(birds_PresentHabMean_lm)


# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
reptiles_PresentHabMean_lm <- lm(ses_reptiles ~ PresentHabMean_dim1 +
                                  PresentHabMean_dim2 +
                                  PresentHabMean_dim3,
                               data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_PresentHabMean_lm)
# Test normality residuals: YES
shapiro.test(rstandard(reptiles_PresentHabMean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_PresentHabMean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_PresentHabMean_lm)
car::Anova(reptiles_PresentHabMean_lm)


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
trees_PresentHabMean_lm <- lm(ses_trees ~ PresentHabMean_dim1 +
                              PresentHabMean_dim2 +
                              PresentHabMean_dim3,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_PresentHabMean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_PresentHabMean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_PresentHabMean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_PresentHabMean_lm)
car::Anova(trees_PresentHabMean_lm)


# 8 - Relationship between synthetic variables for hab sd and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentHabSd_dim1)
hist(driv_db$PresentHabSd_dim2)
hist(driv_db$PresentHabSd_dim3)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabSd_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabSd_dim2)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabSd_dim3)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabSd_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabSd_dim2)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabSd_dim3)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabSd_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabSd_dim2)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabSd_dim3)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
birds_PresentHabSd_lm <- lm(ses_birds ~ PresentHabSd_dim1 +
                                PresentHabSd_dim2 +
                                PresentHabSd_dim3,
                              data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_PresentHabSd_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_PresentHabSd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_PresentHabSd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_PresentHabSd_lm)
car::Anova(birds_PresentHabSd_lm)


# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
reptiles_PresentHabSd_lm <- lm(ses_reptiles ~ PresentHabSd_dim1 +
                                   PresentHabSd_dim2 +
                                   PresentHabSd_dim3,
                                 data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_PresentHabSd_lm)
# Test normality residuals: YES
shapiro.test(rstandard(reptiles_PresentHabSd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_PresentHabSd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_PresentHabSd_lm)
car::Anova(reptiles_PresentHabSd_lm)


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
trees_PresentHabSd_lm <- lm(ses_trees ~ PresentHabSd_dim1 +
                                PresentHabSd_dim2 +
                                PresentHabSd_dim3,
                              data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_PresentHabSd_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_PresentHabSd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_PresentHabSd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_PresentHabSd_lm)
car::Anova(trees_PresentHabSd_lm)


# 9 - Relationship between synthetic variables for fire and Faith PD ======


# Histograms for drivers var:
hist(driv_db$Fire_dim1)
hist(driv_db$Fire_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$Fire_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$Fire_dim2)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$Fire_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$Fire_dim2)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$Fire_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$Fire_dim2)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_Fire_lm <- lm(ses_birds ~ Fire_dim1 +
                            Fire_dim2,
                          data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_Fire_lm)
# Test normality residuals: NO
shapiro.test(rstandard(birds_Fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_Fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_Fire_lm)
car::Anova(birds_Fire_lm)

# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2:
reptiles_Fire_lm <- lm(ses_reptiles ~ Fire_dim1 +
                               Fire_dim2,
                             data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_Fire_lm)
# Test normality residuals: NO
shapiro.test(rstandard(reptiles_Fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_Fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_Fire_lm)
car::Anova(reptiles_Fire_lm)


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2:
trees_Fire_lm <- lm(ses_trees ~ Fire_dim1 +
                            Fire_dim2,
                          data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_Fire_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_Fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_Fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_Fire_lm)
car::Anova(trees_Fire_lm)


# 10 - Relationship between synthetic variables for present lu and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentLandUse_dim1)
hist(driv_db$PresentLandUse_dim2)
hist(driv_db$PresentLandUse_dim3)
hist(driv_db$PresentLandUse_dim4)


# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentLandUse_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentLandUse_dim2)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentLandUse_dim3)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentLandUse_dim4)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentLandUse_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentLandUse_dim2)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentLandUse_dim3)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentLandUse_dim4)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentLandUse_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentLandUse_dim2)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentLandUse_dim3)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentLandUse_dim4)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3 + dim 4:
birds_PresentLandUse_lm <- lm(ses_birds ~ PresentLandUse_dim1 +
                              PresentLandUse_dim2 +
                              PresentLandUse_dim3 +
                              PresentLandUse_dim4,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_PresentLandUse_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_PresentLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_PresentLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_PresentLandUse_lm)
car::Anova(birds_PresentLandUse_lm)


# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
reptiles_PresentLandUse_lm <- lm(ses_reptiles ~ PresentLandUse_dim1 +
                                 PresentLandUse_dim2 +
                                 PresentLandUse_dim3 +
                                 PresentLandUse_dim4,
                               data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_PresentLandUse_lm)
# Test normality residuals: YES
shapiro.test(rstandard(reptiles_PresentLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_PresentLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_PresentLandUse_lm)
car::Anova(reptiles_PresentLandUse_lm)


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3 + dim 4:
trees_PresentLandUse_lm <- lm(ses_trees ~ PresentLandUse_dim1 +
                              PresentLandUse_dim2 +
                              PresentLandUse_dim3 +
                              PresentLandUse_dim4,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_PresentLandUse_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_PresentLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_PresentLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_PresentLandUse_lm)
car::Anova(trees_PresentLandUse_lm)


# 11 - Relationship between synthetic variables for past lu and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PastLandUse_dim1)
hist(driv_db$PastLandUse_dim2)
hist(driv_db$PastLandUse_dim3)
hist(driv_db$PastLandUse_dim4)


# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastLandUse_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastLandUse_dim2)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastLandUse_dim3)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastLandUse_dim4)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastLandUse_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastLandUse_dim2)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastLandUse_dim3)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastLandUse_dim4)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastLandUse_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastLandUse_dim2)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastLandUse_dim3)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastLandUse_dim4)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3 + dim 4:
birds_PastLandUse_lm <- lm(ses_birds ~ PastLandUse_dim1 +
                                PastLandUse_dim2 +
                                PastLandUse_dim3 +
                                PastLandUse_dim4,
                              data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_PastLandUse_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_PastLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_PastLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_PastLandUse_lm)
car::Anova(birds_PastLandUse_lm)


# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
reptiles_PastLandUse_lm <- lm(ses_reptiles ~ PastLandUse_dim1 +
                                   PastLandUse_dim2 +
                                   PastLandUse_dim3 +
                                   PastLandUse_dim4,
                                 data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_PastLandUse_lm)
# Test normality residuals: YES
shapiro.test(rstandard(reptiles_PastLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_PastLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_PastLandUse_lm)
car::Anova(reptiles_PastLandUse_lm)


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3 + dim 4:
trees_PastLandUse_lm <- lm(ses_trees ~ PastLandUse_dim1 +
                                PastLandUse_dim2 +
                                PastLandUse_dim3 +
                                PastLandUse_dim4,
                              data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_PastLandUse_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_PastLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_PastLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_PastLandUse_lm)
car::Anova(trees_PastLandUse_lm)

# 12 - Relationship between synthetic variables for pop and Faith PD ===========


# Histograms for drivers var:
hist(driv_db$Pr_Pop_2020_mean)
hist(driv_db$Pr_RatePop_2020_mean)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$Pr_Pop_2020_mean)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$Pr_RatePop_2020_mean)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$Pr_Pop_2020_mean)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$Pr_RatePop_2020_mean)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$Pr_Pop_2020_mean)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$Pr_RatePop_2020_mean)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_pop_lm <- lm(ses_birds ~ Pr_Pop_2020_mean +
                     Pr_RatePop_2020_mean,
                   data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_pop_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_pop_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_pop_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_pop_lm)
car::Anova(birds_pop_lm)


# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2:
reptiles_pop_lm <- lm(ses_reptiles ~ Pr_Pop_2020_mean +
                        Pr_RatePop_2020_mean,
                      data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_pop_lm)
# Test normality residuals: NO
shapiro.test(rstandard(reptiles_pop_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_pop_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_pop_lm)
car::Anova(reptiles_pop_lm)


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2:
trees_pop_lm <- lm(ses_trees ~ Pr_Pop_2020_mean +
                     Pr_RatePop_2020_mean,
                   data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_pop_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_pop_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_pop_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_pop_lm)
car::Anova(trees_pop_lm)


# 13 - Relationship between synthetic variables for herb and Faith PD ==========


# Histograms for drivers var:
hist(driv_db$HerbCons_sum)
hist(driv_db$HerbRichn_sum)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$HerbCons_sum)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$HerbRichn_sum)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$HerbCons_sum)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$HerbRichn_sum)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$HerbCons_sum)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$HerbRichn_sum)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_pop_lm <- lm(ses_birds ~ HerbCons_sum +
                     HerbRichn_sum,
                   data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_pop_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_pop_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_pop_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_pop_lm)
car::Anova(birds_pop_lm)


# 14 - SEM for Birds - Faith's PD ==============================================




