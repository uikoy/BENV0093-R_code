
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
    rowSums(select(., starts_with("estimated_generation_gwh")), na.rm = TRUE) > 0 ~ "operational",  
    capacity_mw > 0 & rowSums(select(., starts_with("estimated_generation_gwh")), na.rm = TRUE) == 0 ~ "construction",  
    TRUE ~ "planned" 
  ))


common_cols <- intersect(names(new_data), names(indonesia_power_plants))


new_data_filtered <- new_data %>%
  select(all_of(common_cols))


combined_data <- bind_rows(
  indonesia_power_plants,
  new_data_filtered
)



indonesia_power_plants_hydro_wind_geo <- combined_data %>%
  filter(primary_fuel %in% c("Hydro", "Wind", "Geothermal"))


total_capacity1 <- sum(indonesia_power_plants_hydro_wind_geo$capacity_mw, na.rm = TRUE)


print(paste("总装机容量为:", total_capacity1, "MW"))


indonesia_power_plants_solar <- combined_data %>%
  filter(primary_fuel %in% c("solar"))


total_capacity2 <- sum(indonesia_power_plants_solar$capacity_mw, na.rm = TRUE)


print(paste("总装机容量为:", total_capacity2, "MW"))


indonesia_power_plants_rest <- combined_data %>%
  filter(primary_fuel %in% c("Gas", "Coal", "Oil"))


total_capacity3 <- sum(indonesia_power_plants_rest$capacity_mw, na.rm = TRUE)


print(paste("总装机容量为:", total_capacity3, "MW"))



