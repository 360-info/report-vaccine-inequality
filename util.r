library(readr)
library(jsonlite)
library(rvest)
library(here)

#' get_owid: return either a freshly downloaded or cached copy of OWID vacine
#' data
#' 
#' @param cached_date if supplied, use a specified cached copy. if NULL (the
#' default), use the latest cached copy
#' @param update if TRUE, download a new version even if there is a cached copy
#' available
#' @return the JSON representation of the OWID data
get_owid <- function(cached_date = NULL, update = FALSE) {

  # create the `data` folder (if it doesn't already exist)
  dir.create(here("data"))

  # look for a saved owid json in `data`
  cached_owid <- list.files(here("data"), pattern = glob2rx("owid-*.json"))

  if (update || length(cached_owid) == 0L) {
    # if forcing update or if no cached copy available, get a new one
    message("Downloading fresh copy of OWID data...")
    download.file(
      "https://covid.ourworldindata.org/data/owid-covid-data.json",
      here("data", paste0("owid-", Sys.Date(), ".json")))
    
    cached_file <- paste0("owid-", Sys.Date(), ".json")
    message("Loading new copy of OWID data..")
  } else if (is.null(cached_date)) {
    cached_file <- sort(cached_owid, decreasing = TRUE)[1]
    message("Loading latest cached OWID data: ", cached_file)
  } else {
    cached_file <- paste0("owid-", cached_date, ".json")
    message("Loading requested cached OWID data: ", cached_file)
  }

  # load and return that data
  return(fromJSON(here("data", cached_file)))
}

#' get_unicef_totals: load the unicef data on receipt of vaccines by source
#' program and by country
#'
#' @return the JSON representation of the OWID data
get_unicef_totals <- function() {
  read_csv(
    here("data", "unicef-vaccine-programs.csv"),
    col_names = c("country", "commercial", "donations", "covax", "avat",
      "unknown", "total"),
    skip = 1,
    col_types = "cnnnnnn",
    na = c("(Blank)"))
}

#' get_india_exports: scrape covax and other export figures from mea.gov.in.
#' (this table comes with a grouped header that we'll replace manually)
#'
#' @return a data frame of the data
get_india_exports <- function() {
  read_html("https://www.mea.gov.in/vaccine-supply.htm") %>%
    html_element("#innerContent table.tableData") %>%
    html_table()
    # remove the first 2 rows (grouped header) and the last 3 (total/footer)...
    head(-3) %>%
    tail(-2) %>%
    # ... and overwrite the default headers
    set_names(c(
      "row", "country",
        paste0("grant_", c("n", "date")),
        paste0("commercial_", c("n", "date")),
        paste0("covax_", c("n", "date")),
        "total_n"))
}