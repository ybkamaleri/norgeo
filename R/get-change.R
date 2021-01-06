#' Get geo code changes
#'
#' This function will download all registered geo code changes via API from SSB.
#'
#' @param from Year from
#' @inheritParams geo_set
#'
#' @export

get_change <- function(type = c("fylke",
                                "kommune",
                                "bydel",
                                "grunnkrets"),
                       year = NULL,
                       from = NULL){

  type <- match.arg(type)
  
  klass <- switch(type,
                  fylke = 104,
                  kommune = 131,
                  bydel = 103,
                  grunnkrets = 1)

  if (is.null(year))
    year <- as.integer(format(Sys.Date(), "%Y"))

  if (is.null(from))
    from <- year - 1
  
  baseUrl <- "http://data.ssb.no/api/klass/v1/classifications/"
  klsUrl <- paste0(baseUrl, klass)
  chgUrl <- paste0(klsUrl, "/changes")

  vecYr <- from:year
  nYr <- length(vecYr)

  ##reference table for year from and to
  allRef <- data.table::CJ(1:nYr, 1:nYr)
  tblRef <- allRef[V2 - V1 == 1]

  ## Create empty list
  listDT <- vector(mode = "list", length = nrow(tblRef))

  for (i in seq_len(nrow(tblRef))) {

    indFrom <- tblRef$V1[i]
    indTo <- tblRef$V2[i]
    yrFrom <- vecYr[indFrom]
    yrTo <- vecYr[indTo]

    dateFrom <- set_year(yrFrom, FALSE)
    dateTo <- set_year(yrTo, TRUE)
    
    ## specify query
    chgQ <- list(from = dateFrom, to = dateTo)
    chgGET <- httr::GET(chgUrl, query = chgQ)
    chgTxt <- httr::content(chgGET, as = "text")

    chgJS <- tryCatch({
      jsonlite::fromJSON(chgTxt)
    },
    error = function(err){
      message("*** Change table for ", type,
              " doesn't exist. From ", dateFrom, " to ", dateTo, " ***")
    })
    
    chgDT <- as.data.table(chgJS[[1]])
    
    ## no error produced but table is empty
    if (!is.null(chgJS) && is.null(chgDT))
      message("No changes from ", dateFrom, " to ", dateTo)

    ## ## no error but no table either
    ## if (is.null(chgJS) && !is.null(chgDT))
    ##   message("Neither table nor changes found from", dateFrom, " to ", dateTo)

    listDT[[i]] <- chgDT

  }

  cat("\n")
  DT <- data.table::rbindlist(listDT)
  
  DT
  
}


## to == FALSE is dateFrom and to == TRUE is dateTo
## dateTo should start at 02.jan because
## GET request for 'to' excludes the specified date in ssb.no KLASS API
set_year <- function(x, to = TRUE){
  
  x <- as.character(x)
  dy <- paste0(x, "-01-01")
  
  ## valid codes for current year should NOT start with 01.jan
  is.errYr <- identical(format(Sys.Date(), "%m-%d"), "01-01")
  if (is.errYr || isTRUE(to))
    dy <- paste0(x, "-01-02")

  ## now is TRUE if current year
  now <- identical(format(Sys.Date(), "%Y"), x)
  
  if (now)
    dy <- Sys.Date() + 1
  
  invisible(dy)

}

