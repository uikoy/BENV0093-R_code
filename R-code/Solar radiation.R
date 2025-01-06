
library(sf)
library(ncdf4)
library(ggplot2)
library(raster)
library(viridis)
library(terra)

boundary_path <- "/Users/kw/Desktop/0093数据集2/Indonesia.gdb"

admin_boundaries <- st_read(dsn = boundary_path, layer = "ID_GEOG_ADM1_2020_uscb_202405")


nc_file_july <- "/Users/kw/Desktop/0093数据集2/july.nc"
nc_file_december <- "/Users/kw/Desktop/0093数据集2/December.nc"


nc_data_july <- nc_open(nc_file_july)
nc_data_december <- nc_open(nc_file_december)



solar_radiation_july <- ncvar_get(nc_data_july, "ssrd") 
solar_radiation_december <- ncvar_get(nc_data_december, "ssrd")

lon <- ncvar_get(nc_data_july, "longitude")
lat <- ncvar_get(nc_data_july, "latitude")
time_july <- ncvar_get(nc_data_july, "valid_time")
time_december <- ncvar_get(nc_data_december, "valid_time")


nc_close(nc_data_july)
nc_close(nc_data_december)


solar_raster_july <- brick(nc_file_july, varname = "ssrd")
solar_raster_december <- brick(nc_file_december, varname = "ssrd")


target_hours <- c(8, 12, 14, 16)
indices_july <- which(format(as.POSIXct(time_july, origin = "1970-01-01", tz = "UTC"), "%H") %in% target_hours)
indices_december <- which(format(as.POSIXct(time_december, origin = "1970-01-01", tz = "UTC"), "%H") %in% target_hours)


solar_raster_july_subset <- solar_raster_july[[indices_july]]
solar_raster_december_subset <- solar_raster_december[[indices_december]]


solar_raster_combined <- stack(solar_raster_july_subset, solar_raster_december_subset)


solar_raster_clipped <- mask(crop(solar_raster_combined, admin_boundaries), admin_boundaries)


solar_df <- as.data.frame(rasterToPoints(solar_raster_clipped), stringsAsFactors = FALSE)
colnames(solar_df) <- c("lon", "lat", "radiation")


p <- ggplot() +
  geom_raster(data = solar_df, aes(x = lon, y = lat, fill = radiation)) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis(name = "Solar Radiation", option = "C") +
  labs(title = "Solar Radiation Map (July and December)", x = "Longitude", y = "Latitude") +
  theme_minimal()


ggsave("solar_radiation_map.png", p, width = 10, height = 8)


print(p)


min_val <- cellStats(solar_raster_clipped, stat = "min", na.rm = TRUE)
max_val <- cellStats(solar_raster_clipped, stat = "max", na.rm = TRUE)


solar_mean <- calc(solar_raster_clipped, fun = mean, na.rm = TRUE)
solar_suit <- (solar_mean - min_val) / (max_val - min_val)


solar_suit <- rast(solar_suit)  # 转换为 terra::SpatRaster

boundary_vect <- vect(admin_boundaries)

solar_suit <- crop(solar_suit, boundary_vect)
solar_suit <- mask(solar_suit, boundary_vect)

plot(solar_suit)