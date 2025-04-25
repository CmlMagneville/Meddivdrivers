################################################################################
##
## Script to maps main drivers when needed
##
## Camille Magneville
##
## 11/2024
##
## 14_Map_drivers.R
##
################################################################################

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# Load data ====================================================================

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# Load the Europe land mask:
land_mask <- sf::st_read(here::here("integradiv_db",
                                    "land_mask",
                                    "land_EPSG3035.shp"))

# Load environmental drivers (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))


# 2 - Map drivers ==============================================================


# Herbivory:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "HerbCons_sum",
                  grid = grid_50km,
                  col_pal = c("white", "#dab811"),
                  land_mask = land_mask,
                  type = "drivers",
                  save = TRUE)

# Past MAT sd:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Past_MAT_sd",
                  grid = grid_50km,
                  col_pal = c("white", "#0881bd"),
                  land_mask = land_mask,
                  save = TRUE)

# Velocity LGM:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Past_CCVelLGM_mean.voccMag",
                  grid = grid_50km,
                  col_pal = c("white", "#0881bd"),
                  land_mask = land_mask,
                  save = TRUE)

# Velocity Holocene:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Past_CCVelShortTerm_mean.voccMag",
                  grid = grid_50km,
                  col_pal = c("white", "#0881bd"),
                  land_mask = land_mask,
                  save = TRUE)

# Velocity YD decrease:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Past_CCVelYoungerDryas_mean.voccMag",
                  grid = grid_50km,
                  col_pal = c("white", "#0881bd"),
                  land_mask = land_mask,
                  save = TRUE)

# Velocity YD increase:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Past_CCVelHolocene_mean.voccMag",
                  grid = grid_50km,
                  col_pal = c("white", "#0881bd"),
                  land_mask = land_mask,
                  save = TRUE)

# Present MAT mean:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Present_MAT_mean",
                  grid = grid_50km,
                  col_pal = c( "white", "#2dae99"),
                  land_mask = land_mask,
                  save = TRUE)

# Present AI mean:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Present_AI_mean",
                  grid = grid_50km,
                  col_pal = c( "white", "#2dae99"),
                  land_mask = land_mask,
                  save = TRUE)

# Elevation mean:
envdriv_full_db$Elv_mean <- as.numeric(envdriv_full_db$Elv_mean)
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Elv_mean",
                  grid = grid_50km,
                  col_pal = c("white", "#2dae99"),
                  land_mask = land_mask,
                  save = TRUE)

# Depth mean:
envdriv_full_db$Depth_mean <- as.numeric(envdriv_full_db$Depth_mean)
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Depth_mean",
                  grid = grid_50km,
                  col_pal = c("white", "#2dae99"),
                  land_mask = land_mask,
                  save = TRUE)

# Growth Rate Pop:
drivers.maps.plot(drivers_df = envdriv_full_db,
                  driver_nm = "Pr_RatePop_2020_mean",
                  grid = grid_50km,
                  col_pal = c("white", "#882255"),
                  land_mask = land_mask,
                  save = TRUE)


