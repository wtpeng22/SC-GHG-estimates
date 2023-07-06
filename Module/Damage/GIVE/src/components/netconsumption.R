# 定义变量
# 定义函数

netconsumption_run_timestep <- function(total_damage) {
  # Sum the population and gdp of all countries for the current timestep
  global_population <<- rowSums(population)
  global_gdp <<- rowSums(gdp)
  
  # Convert damages to billions
  # total_damage <- total_damage[t] / 1e9
  
  # Compute net consumption as GDP - damages
  net_consumption <<- global_gdp - total_damage
  
  # We multiply by 1e3 because net_consumption is in billion, and population is in million
  net_cpc <<- net_consumption * 1e3 / global_population
}
