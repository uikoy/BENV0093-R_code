
library(ggplot2)
library(terra)
library(sf)
library(ggspatial)


boundary_path <- "/Users/kw/Desktop/0093数据集2/Indonesia.gdb"
land_cover_path <- "/Users/kw/Desktop/0093数据集2/IDN_cov.tif"


admin_boundaries <- st_read(dsn = boundary_path, layer = "ID_GEOG_ADM1_2020_uscb_202405")


land_cover <- rast(land_cover_path)


boundary_vect <- vect(admin_boundaries)
boundary_vect <- project(boundary_vect, crs(land_cover))


land_cover_cropped <- crop(land_cover, boundary_vect)
land_cover_masked <- mask(land_cover_cropped, boundary_vect)


suitable_values <- c(13, 14, 16)
suitable_land <- classify(land_cover_masked, cbind(suitable_values, 1), others = 0)


suitable_land_df <- as.data.frame(suitable_land, xy = TRUE, na.rm = TRUE)
colnames(suitable_land_df) <- c("x", "y", "suitability")
suitable_land_df$suitability <- as.factor(suitable_land_df$suitability)


boundary_vect_sf <- st_as_sf(boundary_vect)


ggplot() +
  geom_tile(data = suitable_land_df, aes(x = x, y = y, fill = suitability)) +
  geom_sf(data = boundary_vect_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(
    values = c("0" = "purple", "1" = "yellow"),
    labels = c("Unsuitable", "Suitable"),
    name = "Land Suitability"
  ) +
  labs(
    title = "Suitable Land for Solar Power Plants",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering
  )


suitable_values <- c(13, 14, 16)

land_cover_suit <- classify(
  land_cover_masked,
  rcl = cbind(suitable_values, 1),
  others = 0
)
land_cover_suit <- resample(land_cover_suit, solar_suit, method = "near")
plot(land_cover_suit)