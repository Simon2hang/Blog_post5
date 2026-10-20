library(sf)
library(httr2)
library(dplyr)
library(readr)
library(stringr)


dir.create(
  "data/raw_data",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed_data",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "figures",
  recursive = TRUE,
  showWarnings = FALSE
)


message("")
message("========================================")
message("Downloading Boston neighborhoods...")
message("========================================")


neighborhood_url <- paste0(
  "https://services.arcgis.com/",
  "sFnw0xNflSi8J0uh/ArcGIS/rest/services/",
  "Boston_in_Context_by_Neighborhood_2025/",
  "FeatureServer/1/query?",
  "where=1%3D1",
  "&outFields=*",
  "&returnGeometry=true",
  "&outSR=4326",
  "&f=geojson"
)


neighborhoods <- st_read(
  neighborhood_url,
  quiet = TRUE
)


message(
  "Neighborhoods downloaded: ",
  nrow(neighborhoods)
)


st_write(
  neighborhoods,
  "data/raw_data/boston_neighborhoods_2025.geojson",
  delete_dsn = TRUE,
  quiet = TRUE
)


download_arcgis_table <- function(
    layer_url,
    where = "1=1",
    out_fields = "*",
    chunk_size = 2000
) {
  
  offset <- 0
  pieces <- list()
  
  repeat {
    
    message(
      "Downloading records starting at ",
      offset,
      "..."
    )
    
    response <- request(
      paste0(layer_url, "/query")
    ) |>
      req_url_query(
        where = where,
        outFields = out_fields,
        returnGeometry = "false",
        resultOffset = offset,
        resultRecordCount = chunk_size,
        f = "json"
      ) |>
      req_retry(
        max_tries = 3
      ) |>
      req_perform()
    
    
    body <- resp_body_json(
      response,
      simplifyVector = FALSE
    )
    
    
    if (!is.null(body$error)) {
      
      stop(
        "ArcGIS error: ",
        body$error$message
      )
    }
    
    
    features <- body$features
    
    
    if (length(features) == 0) {
      break
    }
    
    
    chunk <- bind_rows(
      lapply(
        features,
        function(x) x$attributes
      )
    )
    
    
    pieces[[length(pieces) + 1]] <- chunk
    
    
    if (nrow(chunk) < chunk_size) {
      break
    }
    
    
    offset <- offset + chunk_size
  }
  
  
  if (length(pieces) == 0) {
    
    stop(
      "No records were downloaded."
    )
  }
  
  
  bind_rows(pieces)
}



message("")
message("========================================")
message("Downloading 2025 Boston Police source data...")
message("========================================")


bpd_url <- paste0(
  "https://services.arcgis.com/",
  "sFnw0xNflSi8J0uh/ArcGIS/rest/services/",
  "Boston_Incidents_View/",
  "FeatureServer/0"
)


bpd_source_2025 <- download_arcgis_table(
  
  layer_url = bpd_url,
  
  where = "YEAR = 2025",
  
  out_fields = paste(
    "INC_NUM",
    "CRIME",
    "OFFENSE_CODE",
    "OFFENSE_DESC",
    "CRIME_PART",
    "CRIME_CATEGORY",
    "REPORT_DATE",
    "YEAR",
    "MONTH",
    "DAY_OF_WEEK",
    "HOUR_OF_DAY",
    "NEIGHBORHOOD",
    "DISTRICT",
    sep = ","
  ),
  
  chunk_size = 2000
)


message(
  "Total BPD source rows downloaded: ",
  nrow(bpd_source_2025)
)


message("")
message("CRIME_PART values found in the source:")

print(
  bpd_source_2025 |>
    count(
      CRIME_PART,
      sort = TRUE
    )
)



boston_crimes_2025 <- bpd_source_2025 |>
  
  mutate(
    
    crime_part_clean =
      CRIME_PART |>
      str_to_lower() |>
      str_squish()
    
  ) |>
  
  filter(
    
    crime_part_clean %in% c(
      "part one",
      "part two",
      "part 1",
      "part 2"
    )
    
  ) |>
  
  select(
    -crime_part_clean
  )



if (nrow(boston_crimes_2025) == 0) {
  
  stop(
    paste0(
      "No Part One or Part Two crime records were found. ",
      "Check the printed CRIME_PART values above before continuing."
    )
  )
}


message("")
message(
  "Reported crime rows retained: ",
  nrow(boston_crimes_2025)
)


message("")
message("Crime classification after filtering:")

print(
  boston_crimes_2025 |>
    count(
      CRIME_PART,
      sort = TRUE
    )
)



write_csv(
  boston_crimes_2025,
  "data/raw_data/boston_reported_crimes_2025.csv"
)



message("")
message("Most common crime types:")

print(
  boston_crimes_2025 |>
    filter(
      !is.na(CRIME),
      CRIME != ""
    ) |>
    count(
      CRIME,
      sort = TRUE
    ) |>
    head(20)
)


message("")
message("========================================")
message("DOWNLOAD COMPLETE")
message("========================================")


message(
  "Neighborhoods: ",
  nrow(neighborhoods)
)

message(
  "Reported crime rows: ",
  nrow(boston_crimes_2025)
)


message("")
message("Files created:")

message(
  "data/raw_data/boston_neighborhoods_2025.geojson"
)

message(
  "data/raw_data/boston_reported_crimes_2025.csv"
)