
library(sf)
library(ggplot2)
library(ggspatial)



grid_path <- "/Users/kw/Desktop/0093数据集2/exp-sea-electric_grid/IDN_gridfinder.gpkg"  # 修改为您的实际路径
boundary_path <- "/Users/kw/Desktop/0093数据集2/Indonesia.gdb"    # 印尼边界数据路径


grid_data <- st_read(grid_path)


boundary_data <- st_read(dsn = boundary_path, layer = "ID_GEOG_ADM1_2020_uscb_202405")


crs_grid <- st_crs(grid_data)
crs_boundary <- st_crs(boundary_data)


if (!st_crs(grid_data) == st_crs(boundary_data)) {
  grid_data <- st_transform(grid_data, crs = st_crs(boundary_data))
}



grid_clipped <- st_intersection(grid_data, boundary_data)



map <- ggplot() +
  geom_sf(data = boundary_data, fill = "white", color = "black") + 
  geom_sf(data = grid_clipped, color = "red", size = 0.3) +       
  labs(title = "Electric Grid Distribution in Indonesia",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.5) +           
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering)  


ggsave(filename = "Electric_Grid_Distribution.png", plot = map, 
       width = 10, height = 8, dpi = 300)


ggsave(filename = "Electric_Grid_Distribution.pdf", plot = map, 
       width = 10, height = 8)



grid_vect <- vect(grid_clipped)


grid_ras <- rasterize(grid_vect, base_ras, field=1, background=0)
grid_ras <- mask(grid_ras, boundary_vect)


distance_to_grid <- distance(grid_ras)
max_dist_grid <- global(distance_to_grid, "max", na.rm=TRUE)[[1]]


grid_suit <- 1 - (distance_to_grid / max_dist_grid)