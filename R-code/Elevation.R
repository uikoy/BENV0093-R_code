
library(terra)
library(ggplot2)
library(ggspatial)
library(sf)


elevation_path <- "/Users/kw/Desktop/0093数据集2/IDN_alt.tif"  
boundary_path <- "/Users/kw/Desktop/0093数据集2/Indonesia.gdb"  


st_layers(dsn = boundary_path)


admin_boundaries <- st_read(dsn = boundary_path, layer = "ID_GEOG_ADM1_2020_uscb_202405")


elevation <- rast(elevation_path)


boundary_vect <- vect(admin_boundaries)
boundary_vect <- project(boundary_vect, crs(elevation))


slope <- terrain(elevation, v = "slope", unit = "degrees")


slope_clipped <- mask(slope, boundary_vect)


plot(slope_clipped, main = "Slope Clipped to Boundary")


suitable_degree <- slope_clipped < 3


suitable_degree_df <- as.data.frame(suitable_degree, xy = TRUE, na.rm = TRUE)
boundary_vect_sf <- st_as_sf(boundary_vect)  


ggplot() +
  geom_tile(data = suitable_degree_df, aes(x = x, y = y, fill = slope)) +
  geom_sf(data = boundary_vect_sf, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(values = c("white", "green"), labels = c("Unsuitable", "Suitable"), name = "Land Suitability") +
  labs(title = "Suitable Areas for Solar Power Stations",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering)

m <- c(-Inf, 3, 1,
       3, Inf, 0)
elev_suit <- classify(slope, rcl = matrix(m, ncol=3, byrow=TRUE))


elev_suit <- resample(elev_suit, solar_suit, method="near")

plot(elev_suit)
