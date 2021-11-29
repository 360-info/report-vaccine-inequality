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

