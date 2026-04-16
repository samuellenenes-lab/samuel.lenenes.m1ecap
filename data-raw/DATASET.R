# Préparation du dataset world_data
# Source : https://www.kaggle.com/datasets/nelgiriyewithana/countries-of-the-world-2023

library(readr)
library(dplyr)
library(stringr)
library(janitor)

world_data <- read_csv("data-raw/world-data-2023.csv") |>
  janitor::clean_names() |>
  mutate(
    # Nettoyer les colonnes numériques encodées en texte
    gdp = str_remove_all(gdp, "[\\$, ]") |> as.numeric(),
    population = str_remove_all(population, "[, ]") |> as.numeric(),
    co2_emissions = str_remove_all(co2_emissions, "[, ]") |> as.numeric(),
    urban_population = str_remove_all(urban_population, "[, ]") |> as.numeric(),
    land_area_km2 = str_remove_all(land_area_km2, "[, ]") |> as.numeric(),
    armed_forces_size = str_remove_all(armed_forces_size, "[, ]") |> as.numeric(),
    gasoline_price = str_remove_all(gasoline_price, "[\\$ ]") |> as.numeric(),
    minimum_wage = str_remove_all(minimum_wage, "[\\$ ]") |> as.numeric(),
    # Nettoyer les pourcentages
    agricultural_land_percent = str_remove(agricultural_land_percent, "%") |> as.numeric(),
    forested_area_percent = str_remove(forested_area_percent, "%") |> as.numeric(),
    unemployment_rate = str_remove(unemployment_rate, "%") |> as.numeric(),
    cpi_change_percent = str_remove(cpi_change_percent, "%") |> as.numeric(),
    tax_revenue_percent = str_remove(tax_revenue_percent, "%") |> as.numeric(),
    out_of_pocket_health_expenditure = str_remove(out_of_pocket_health_expenditure, "%") |> as.numeric()
  )

usethis::use_data(world_data, overwrite = TRUE)
