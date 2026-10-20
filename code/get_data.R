library(sf)
library(httr2)
library(dplyr)
library(readr)

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
message("Downloading Boston neighborhoods...")


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


# Save GIS file

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
      "Downloading crime records starting at ",
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
    
    
    # Last page
    
    if (nrow(chunk) < chunk_size) {
      break
    }
    
    
    offset <- offset + chunk_size
  }
  
  
  if (length(pieces) == 0) {
    
    stop(
      "No crime records were downloaded."
    )
  }
  
  
  bind_rows(pieces)
}


message("")
message("Downloading data")

crime_url <- paste0(
  "https://services.arcgis.com/",
  "sFnw0xNflSi8J0uh/ArcGIS/rest/services/",
  "Boston_Incidents_View/",
  "FeatureServer/0"
)


crime_2025 <- download_arcgis_table(
  
  layer_url = crime_url,
  
  where = "YEAR = 2025",
  
  out_fields = paste(
    "INC_NUM",
    "CRIME",
    "CRIME_CATEGORY",
    "YEAR",
    "MONTH",
    "NEIGHBORHOOD",
    sep = ","
  ),
  
  chunk_size = 2000
)


message(
  "Crime rows downloaded: ",
  nrow(crime_2025)
)


write_csv(
  crime_2025,
  "data/raw_data/boston_crime_2025.csv"
)


# CHECK RESULTS

message("")
message("========================================")
message("DOWNLOAD COMPLETE")
message("========================================")


message(
  "Neighborhoods: ",
  nrow(neighborhoods)
)

message(
  "Crime rows: ",
  nrow(crime_2025)
)



message("")
message("Neighborhood data preview:")

print(
  neighborhoods |>
    st_drop_geometry() |>
    head()
)


message("")
message("Crime data preview:")

print(
  crime_2025 |>
    head()
)



message("")
message("Years in crime dataset:")

print(
  unique(crime_2025$YEAR)
)


message("")
message("Crime neighborhood names:")

print(
  sort(
    unique(
      crime_2025$NEIGHBORHOOD
    )
  )
)