## Rebuild internal package data (api_urls + measures)
## Queries the Socrata Discovery API to find all PLACES endpoints.

library(jsonlite)

# --- 1. Discover PLACES datasets via Socrata catalog API --------------------

catalog_url <- paste0(

"https://api.us.socrata.com/api/catalog/v1",
  "?domains=data.cdc.gov",
  "&q=PLACES+Local+Data+for+Better+Health",
  "&limit=50"
)

catalog <- fromJSON(catalog_url)

# Extract id + name for every result
all_results <- data.frame(
  id   = catalog$results$resource$id,
  name = catalog$results$resource$name,
  stringsAsFactors = FALSE
)

# Keep only the PLACES long-format datasets
places_ds <- all_results[grepl("^PLACES: Local Data for Better Health", all_results$name), ]

# --- 2. Parse geography and release year from dataset name ------------------

# Geography label sits between the last comma and "Data"
# Examples:
#   "PLACES: Local Data for Better Health, County Data, 2025 release"
#   "PLACES: Local Data for Better Health, Census Tract Data 2023 release"
geo_pattern  <- "(County|Census Tract|Place|ZCTA) Data"
year_pattern <- "(20\\d{2}) release"

places_ds$geo_label <- regmatches(places_ds$name, regexpr(geo_pattern, places_ds$name))
places_ds$geo_label <- sub(" Data$", "", places_ds$geo_label)

places_ds$release <- as.integer(
  sub(year_pattern, "\\1", regmatches(places_ds$name, regexpr(year_pattern, places_ds$name)))
)

# Map labels to package geography codes
geo_map <- c(
  "County"       = "county",
  "Census Tract" = "tract",
  "Place"        = "place",
  "ZCTA"         = "zcta"
)

places_ds$geography <- unname(geo_map[places_ds$geo_label])

# Build resource URL
places_ds$url <- paste0("https://data.cdc.gov/resource/", places_ds$id, ".json")

# --- 3. Build api_urls data frame -------------------------------------------

api_urls <- places_ds[, c("release", "geography", "url")]
api_urls$release <- as.character(api_urls$release)
api_urls <- api_urls[order(api_urls$release, api_urls$geography), ]
rownames(api_urls) <- NULL

# Insert NA row for 2024 ZCTA (known missing dataset)
if (!any(api_urls$release == "2024" & api_urls$geography == "zcta")) {
  api_urls <- rbind(
    api_urls,
    data.frame(release = "2024", geography = "zcta", url = NA_character_)
  )
  api_urls <- api_urls[order(api_urls$release, api_urls$geography), ]
  rownames(api_urls) <- NULL
}

cat("api_urls:\n")
print(api_urls)

# --- 4. Build measures from get_dictionary() --------------------------------

devtools::load_all()
measures <- get_dictionary()

# --- 5. Save to R/sysdata.rda ----------------------------------------------

usethis::use_data(measures, api_urls, internal = TRUE, compress = "xz", overwrite = TRUE)
