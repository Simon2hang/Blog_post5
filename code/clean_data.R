library(sf)
library(dplyr)
library(readr)
library(stringr)


crimes <- read_csv(
  "data/raw_data/boston_reported_crimes_2025.csv",
  show_col_types = FALSE
)


neighborhoods <- st_read(
  "data/raw_data/boston_neighborhoods_2025.geojson",
  quiet = TRUE
)


message(
  "Reported crime rows loaded: ",
  nrow(crimes)
)

message(
  "Neighborhood polygons loaded: ",
  nrow(neighborhoods)
)


message("")
message("Crime classifications:")

print(
  crimes |>
    count(
      CRIME_PART,
      sort = TRUE
    )
)


clean_name <- function(x) {
  
  x |>
    str_to_lower() |>
    str_replace_all("&", "and") |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}


# Crime data

crimes <- crimes |>
  
  filter(
    !is.na(INC_NUM),
    !is.na(NEIGHBORHOOD),
    NEIGHBORHOOD != ""
  ) |>
  
  mutate(
    neighborhood_key =
      clean_name(NEIGHBORHOOD)
  )


# GIS neighborhood data

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


crime_incidents <- crimes |>
  
  distinct(
    INC_NUM,
    .keep_all = TRUE
  )


message("")
message(
  "Unique reported crime incidents: ",
  nrow(crime_incidents)
)



crime_by_neighborhood <- crime_incidents |>
  
  group_by(
    neighborhood_key
  ) |>
  
  summarise(
    
    crime_count =
      n(),
    
    .groups = "drop"
  )



message("")
message("Crime neighborhoods not matched to GIS:")


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
message("GIS neighborhoods without a crime match:")


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



crime_map <- crime_map |>
  
  mutate(
    
    crime_rate_per_1000 =
      if_else(
        population > 0,
        crime_count / population * 1000,
        NA_real_
      )
    
  )


crime_types <- crimes |>
  
  filter(
    !is.na(CRIME),
    CRIME != ""
  ) |>
  
  count(
    CRIME,
    sort = TRUE,
    name = "crime_count"
  )


crime_monthly <- crime_incidents |>
  
  filter(
    !is.na(MONTH)
  ) |>
  
  count(
    MONTH,
    name = "crime_count"
  ) |>
  
  arrange(
    MONTH
  )


crime_part_summary <- crimes |>
  
  count(
    CRIME_PART,
    sort = TRUE,
    name = "crime_count"
  )


st_write(
  crime_map,
  "data/processed_data/boston_crime_map_2025.geojson",
  delete_dsn = TRUE,
  quiet = TRUE
)


write_csv(
  crime_incidents,
  "data/processed_data/boston_reported_crime_incidents_2025.csv"
)


write_csv(
  crime_types,
  "data/processed_data/crime_types_2025.csv"
)


write_csv(
  crime_monthly,
  "data/processed_data/crime_monthly_2025.csv"
)


write_csv(
  crime_part_summary,
  "data/processed_data/crime_part_summary_2025.csv"
)



message("")
message("========================================")
message("CLEANING COMPLETE")
message("========================================")


message("")
message("Neighborhood crime results:")

print(
  crime_map |>
    
    st_drop_geometry() |>
    
    select(
      neighborhood,
      population,
      crime_count,
      crime_rate_per_1000
    ) |>
    
    arrange(
      desc(crime_count)
    )
)


message("")
message("Top 15 reported crime types:")

print(
  head(
    crime_types,
    15
  )
)


message("")
message("Monthly reported crime incidents:")

print(
  crime_monthly
)


message("")
message("Part One vs Part Two:")

print(
  crime_part_summary
)