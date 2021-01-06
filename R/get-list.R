#' Get the list of geographical levels
#'
#' This function will download the codes of selected geographical levels via API.
#'
#' @param from Year from
#' @inheritParams geo_set
#'
#' @export

get_list <- function(type = c("fylke",
                              "kommune",
                              "bydel",
                              "grunnkrets"),
                     year = NULL,
                     from = NULL){

  type <- match.arg(type)

  if(is.null(year))
    year <- as.character(format(Sys.Date(), "%Y"))
  
  klass <- switch(type,
                  fylke = 104,
                  kommune = 131,
                  bydel = 103,
                  grunnkrets = 1)

  baseUrl <- "http://data.ssb.no/api/klass/v1/classifications/"
  klsUrl <- paste0(baseUrl, klass)

  ## choose specific date or fromTo dates
  kodeUrl <- paste0(klsUrl, ifelse(is.null(from), "/codesAt", "/codes"))

  ## use current dates if year is current year
  is.curYr <- identical(format(Sys.Date(), "%Y"), year)
  ## valid codes for current year should NOT start with 01.jan
  is.errYr <- identical(format(Sys.Date(), "%m-%d"), "01-01")
  if (is.errYr)
    warning("OBS!! Run this again tomorrow!")
  dateTo <- paste0(year, "-01-02")

  if(is.curYr)
    dateTo <- Sys.Date()

  if (!is.null(from)){
    dateFrom <- paste0(from, "-01-01")
    kodeQ <- list(from = dateFrom, to = dateTo)
  } else {
    kodeQ <- list(date = dateTo)
  }

  koGET <- httr::GET(kodeUrl, query = kodeQ)
  koTxt <- httr::content(koGET, as = "text")
  koJS <- jsonlite::fromJSON(koTxt)

  koDT <- koJS[["codes"]]
  data.table::setDT(koDT)

  keepName <-  c("code", "name")

  if (is.null(from)){
    koDT[, setdiff(names(koDT), keepName) := NULL]
    koDT[, validTo := dateTo][]
  } else {
    indN <- grep("InRequestedRange", names(koDT))
    valN <- names(koDT)[indN]
    koDT[, setdiff(names(koDT), c(keepName, valN)) := NULL][]
    data.table::setnames(koDT, old = valN, new = c("from", "validTo"))
    data.table::setorderv(koDT, c("validTo", "code"))
  }

  ## invisible(koDT)
  koDT
}
