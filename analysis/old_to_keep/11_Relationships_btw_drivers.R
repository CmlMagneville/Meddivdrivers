################################################################################
##
## Script to study the relationships between drivers
##
## Camille Magneville
##
## 04/07/2024
##
## 11_Relationships_btw_drivers.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load environmental data ==================================================


# Load environmental drivers - PCA synthetic var (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
driv_db <- readRDS(here::here("transformed_data", "env_db",
                              "SEM_env_db.rds"))


# 2 - Relationship present climate sd and habitat sd - inside categories =======


# Histograms:
hist(driv_db$PresentClimSd_dim1)
hist(driv_db$PresentClimSd_dim2)
hist(driv_db$PresentHabSd_dim1)
hist(driv_db$PresentHabSd_dim2)
hist(driv_db$PresentHabSd_dim3)

# Plot individual relationships:
# Dim1:
ggplot2::ggplot(data = driv_db,
                ggplot2::aes(x = PresentHabSd_dim1,
                             y = PresentClimSd_dim1)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  ggplot2::theme_minimal()
ggplot2::ggplot(data = driv_db,
                ggplot2::aes(x = PresentHabSd_dim2,
                             y = PresentClimSd_dim1)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  ggplot2::theme_minimal()
ggplot2::ggplot(data = driv_db,
                ggplot2::aes(x = PresentHabSd_dim3,
                             y = PresentClimSd_dim1)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  ggplot2::theme_minimal()

# Dim 2:
ggplot2::ggplot(data = driv_db,
                ggplot2::aes(x = PresentHabSd_dim1,
                             y = PresentClimSd_dim2)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  ggplot2::theme_minimal()
ggplot2::ggplot(data = driv_db,
                ggplot2::aes(x = PresentHabSd_dim2,
                             y = PresentClimSd_dim2)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  ggplot2::theme_minimal()
ggplot2::ggplot(data = driv_db,
                ggplot2::aes(x = PresentHabSd_dim3,
                             y = PresentClimSd_dim2)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  ggplot2::theme_minimal()


# Test Linear Regression - dim 1:
lm_dim1 <- lm(PresentClimSd_dim1 ~ PresentHabSd_dim1 +
           PresentHabSd_dim2 + PresentHabSd_dim3,
         data = driv_db)
# General tests model:
performance::check_model(lm_dim1)
# Test normality residuals:
shapiro.test(rstandard(lm_dim1))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(lm_dim1))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(lm_dim1)
car::Anova(lm_dim1)


# 3 - Relationship present climate mean and habitat mean - inside categories ===

