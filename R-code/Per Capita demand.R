
years <- 1971:2019
demand_per_capita <- c(14.2383849995686, 15.7653433848705, 15.6837999077569, 19.0190902270697, 
                       21.2599984187355, 18.169220918085, 21.5326914945457, 31.4610812863677, 
                       38.3058160605457, 45.7064472542362, 51.7493405172514, 60.8065949644834, 
                       65.4175362253835, 70.688640404093, 78.9021526065007, 91.0826741861388, 
                       103.26713635943, 118.266332943306, 135.992110615204, 160.674787896909, 
                       175.871666591051, 191.976853497477, 210.149580687879, 236.779933701008, 
                       259.725372186693, 292.37071246133, 325.347489502985, 324.639197164854, 
                       349.663698450026, 382.177177950589, 402.696690358998, 408.602885980203, 
                       419.825977753156, 464.715295314152, 490.714212853765, 505.813910645093, 
                       535.352859530782, 558.932262162532, 582.711058646864, 624.440425205266, 
                       668.408941399422, 721.048925790129, 761.747589480933, 800.150962579781, 
                       910, 956, 1021, 1064, 1084)


years_2000_2019 <- years[years >= 2000 & years <= 2019]
demand_2000_2019 <- demand_per_capita[years >= 2000 & years <= 2019]
growth_rates <- (demand_2000_2019[-1] / demand_2000_2019[-length(demand_2000_2019)] - 1) * 100
average_growth_rate <- mean(growth_rates)


declining_growth_rate <- function(start_rate, target_avg_rate, years_range) {
  n_years <- length(years_range)
  total_reduction <- start_rate - target_avg_rate
  reduction_per_year <- total_reduction / (n_years - 1)
  return(start_rate - reduction_per_year * (0:(n_years - 1)))
}


forecast_years <- 2019:2040
target_avg_growth_rate <- 1.5

growth_rate_forecast <- declining_growth_rate(average_growth_rate, target_avg_growth_rate, forecast_years)


demand_forecast_per_capita <- numeric(length(forecast_years))
demand_forecast_per_capita[1] <- demand_per_capita[length(demand_per_capita)] # 从2019年的需求开始
for (i in 2:length(forecast_years)) {
  demand_forecast_per_capita[i] <- demand_forecast_per_capita[i - 1] * (1 + growth_rate_forecast[i - 1] / 100)
}


population_2040 <- 311797395 
total_demand_2040 <- demand_forecast_per_capita[length(demand_forecast_per_capita)] * population_2040 / 1e6 # 转为TWh


list(
  average_growth_rate_2000_2019 = average_growth_rate,
  growth_rate_forecast = growth_rate_forecast,
  demand_forecast_per_capita = demand_forecast_per_capita,
  total_demand_2040 = total_demand_2040
)
