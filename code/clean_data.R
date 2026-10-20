library(sf)
library(dplyr)
library(readr)
library(stringr)


crime <- read_csv(
  "data/raw_data/boston_crime_2025.csv",
  show_col_types = FALSE
)

neighborhoods <- st_read(
  "data/raw_data/boston_neighborhoods_2025.geojson",
  quiet = TRUE
)


glimpse(crime)

print(
  names(neighborhoods)
)

message(
  "Raw crime rows: ",
  nrow(crime)
)


clean_name <- function(x) {
  
  x |>
    str_to_lower() |>
    str_replace_all("&", "and") |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}



crime <- crime |>
  mutate(
    neighborhood_key =
      clean_name(NEIGHBORHOOD)
  )



neighborhoods <- neighborhoods |>
  mutate(
    
    neighborhood =
      NeighborhoodName,
    
    neighborhood_key =
      clean_name(NeighborhoodName),
    
    population =
      as.numeric(TotalPopulation),
    
    square_miles =
      as.numeric(SquareMiles)
  )



crime_valid <- crime |>
  filter(
    !is.na(NEIGHBORHOOD),
    NEIGHBORHOOD != "",
    !is.na(INC_NUM)
  )


message(
  "Crime rows with valid neighborhood: ",
  nrow(crime_valid)
)


crime_incidents <- crime_valid |>
  distinct(
    INC_NUM,
    .keep_all = TRUE
  )


message(
  "Unique incidents: ",
  nrow(crime_incidents)
)



crime_by_neighborhood <- crime_incidents |>
  group_by(
    neighborhood_key
  ) |>
  summarise(
    crime_count = n(),
    .groups = "drop"
  )


message("")
message("Crime neighborhood names NOT matched to GIS:")

unmatched_crime <- anti_join(
  crime_by_neighborhood,
  neighborhoods |>
    st_drop_geometry(),
  by = "neighborhood_key"
)

print(
  unmatched_crime
)


message("")
message("GIS neighborhood names without crime match:")

unmatched_map <- anti_join(
  neighborhoods |>
    st_drop_geometry(),
  crime_by_neighborhood,
  by = "neighborhood_key"
)

print(
  unmatched_map |>
    select(
      neighborhood,
      neighborhood_key
    )
)


crime_map <- neighborhoods |>
  
  left_join(
    crime_by_neighborhood,
    by = "neighborhood_key"
  ) |>
  
  mutate(
    
    crime_count =
      if_else(
        is.na(crime_count),
        0L,
        crime_count
      )
  )



# Calculate crime rate

crime_map <- crime_map |>
  
  mutate(
    
    crime_rate_per_1000 =
      if_else(
        population > 0,
        crime_count / population * 1000,
        NA_real_
      ),
    
    incidents_per_sq_mile =
      if_else(
        square_miles > 0,
        crime_count / square_miles,
        NA_real_
      )
  )


crime_categories <- crime_valid |>
  filter(
    !is.na(CRIME),
    CRIME != ""
  ) |>
  count(
    CRIME,
    sort = TRUE,
    name = "offense_count"
  )


crime_monthly <- crime_incidents |>
  
  filter(
    !is.na(MONTH)
  ) |>
  
  count(
    MONTH,
    name = "crime_count"
  ) |>
  
  arrange(MONTH)



st_write(
  crime_map,
  "data/processed_data/boston_crime_map_2025.geojson",
  delete_dsn = TRUE,
  quiet = TRUE
)


write_csv(
  crime_incidents,
  "data/processed_data/boston_crime_incidents_2025.csv"
)


write_csv(
  crime_categories,
  "data/processed_data/crime_categories_2025.csv"
)


write_csv(
  crime_monthly,
  "data/processed_data/crime_monthly_2025.csv"
)



message("")
message("CLEANING COMPLETE")


message("")
message("Neighborhood results:")

print(
  crime_map |>
    
    st_drop_geometry() |>
    
    select(
      neighborhood,
      population,
      crime_count,
      crime_rate_per_1000,
      incidents_per_sq_mile
    ) |>
    
    arrange(
      desc(crime_count)
    )
)


message("")
message("Top crime categories:")

print(
  head(
    crime_categories,
    10
  )
)


message("")
message("Monthly crime incidents:")

print(
  crime_monthly
)