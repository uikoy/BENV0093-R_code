
install.packages("tidyverse") 
library(tidyverse)
library(sf)           
library(ggplot2)       
library(ggspatial)    
library(rnaturalearth) 
library(rnaturalearthdata)


file_path <- "/Users/kw/Desktop/0093 sptial/globalpowerplantdatabasev130/global_power_plant_database.csv"


power_plant_data <- read_csv(file_path)
new_data <- read_csv("/Users/kw/Desktop/0093 sptial/assignment2/后添加的数据.csv")


indonesia_power_plants <- power_plant_data %>%
  filter(country_long == "Indonesia")


indonesia_power_plants <- indonesia_power_plants %>%
  mutate(status = case_when(
    rowSums(select(., starts_with("estimated_generation_gwh")), na.rm = TRUE) > 0 ~ "operational",  # 任何一年有发电量
    capacity_mw > 0 & rowSums(select(., starts_with("estimated_generation_gwh")), na.rm = TRUE) == 0 ~ "construction",  # 无发电量但有装机容量
    TRUE ~ "planned"
  ))


common_cols <- intersect(names(new_data), names(indonesia_power_plants))


new_data_filtered <- new_data %>%
  select(all_of(common_cols))


combined_data <- bind_rows(
  indonesia_power_plants,
  new_data_filtered
)


combined_data_sf <- st_as_sf(
  combined_data,
  coords = c("longitude", "latitude"),  
  crs = 4326                            
)


world <- ne_countries(scale = "medium", returnclass = "sf")
indonesia_map <- world %>%
  filter(admin == "Indonesia")


# 如果你想统一不同状态在图中的颜色，可以自定义一个向量：
status_colors <- c(
  "operational"        = "darkgreen",
  "construction" = "orange",
  "planned"            = "red"
)


ggplot() +
  geom_sf(data = indonesia_map,
          fill = "palegreen3",  
          color = "grey40",     
          size = 0.3) +
  

  geom_sf(data = combined_data_sf,
          aes(color = status,   
              size = capacity_mw), 
          alpha = 0.7) +             
  


  scale_color_manual(values = status_colors) +
  

  annotation_scale(
    location = "bl",  # bottom-left
    width_hint = 0.3
  ) +
  annotation_north_arrow(
    location = "tr",  # top-right
    style = north_arrow_fancy_orienteering
  ) +
  

  coord_sf(xlim = c(95, 142), ylim = c(-11, 7)) +
  

  labs(
    title    = "Indonesia Power Plants",
    subtitle = "Data Source: Global Power Plant Database",
    color    = "Power Plants Status",
    size     = "Power Capacity (MW)"
  ) +
  

  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle   = element_text(hjust = 0.5, size = 12),
    legend.position = "right"
  )


ggsave(
  filename = "indonesia_power_plants_map.png",
  plot     = p,
  width    = 10, 
  height   = 7,
  dpi      = 300  
)
