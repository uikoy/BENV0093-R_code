
w1 <- 0.505  # solar
w2 <- 0.160  # land
w3 <- 0.118  # slope
w4 <- 0.101  # population
w5 <- 0.092  # road


final_suit <- w1 * solar_suit +
  w2 * land_cover_suit +
  w3 * elev_suit +
  w4 * pop_suit +
  w5 * road_suit


final_df <- as.data.frame(final_suit, xy=TRUE, na.rm=TRUE)
colnames(final_df)[3] <- "suit_val" 

ggplot() +
  geom_raster(data = final_df, aes(x = x, y = y, fill = suit_val)) +
  geom_sf(data = admin_boundaries, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis(name = "Suitability (0-1)") +
  labs(
    title = "Final Suitability Map", 
    x = "Longitude", 
    y = "Latitude"
  ) +
  theme_minimal() +

  annotation_scale(
    location = "bl",    
    width_hint = 0.5    
  ) +
  annotation_north_arrow(
    location = "tl",    
    which_north = "true",
    style = north_arrow_fancy_orienteering
  )
