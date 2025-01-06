library(ggplot2)
library(terra)
library(sf)
library(ggspatial)


boundary_path <- "/Users/kw/Desktop/0093数据集2/Indonesia.gdb"


admin_boundaries <- st_read(dsn = boundary_path, layer = "ID_GEOG_ADM1_2020_uscb_202405")


road_path <- "/Users/kw/Desktop/0093数据集2/IDN_rds/IDN_roads.shp"  


roads <- st_read(road_path)


print(roads)


roads_cropped <- st_intersection(roads, admin_boundaries)


ggplot() +
  geom_sf(data = roads_cropped, color = "red", size = 0.3) +
  geom_sf(data = admin_boundaries, fill = NA, color = "black", size = 0.5) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  labs(title = "Cropped Road Network and Administrative Boundaries in Indonesia", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()



roads_cropped <- st_intersection(roads, admin_boundaries)


roads_vect <- vect(roads_cropped)


base_ras <- solar_suit


road_ras <- rasterize(roads_vect, base_ras, field=1, background=0)
road_ras <- mask(road_ras, boundary_vect)  


distance_to_road <- distance(road_ras, filename=NULL)  # terra::distance


max_dist <- global(distance_to_road, "max", na.rm=TRUE)[[1]]
road_suit <- 1 - (distance_to_road / max_dist)


plot(road_suit)