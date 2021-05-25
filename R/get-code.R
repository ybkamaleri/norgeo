#' Get the codes of geographical levels
#'
#' This function will download the codes of selected geographical levels via API.
#'
#' @param date If TRUE then give complete date else year only
#' @inheritParams get_correspond
#'
#' @import data.table
#'
#' @export

get_code <- function(type = c(
                       "fylke",
                       "kommune",
                       "bydel",
                       "grunnkrets"
                     ),
                     from = NULL,
                     to = NULL,
                     date = FALSE) {
  ## globalVariables
  dateTo <- NULL

  type <- match.arg(type)

  klass <- switch(type,
    fylke = 104,
    kommune = 131,
    bydel = 103,
    grunnkrets = 1
  )

  if (is.null(from)) {
    from <- date_now()
  } else {
    from <- paste0(from, "-01-01")
  }

  if (!is.null(to)) to <- paste0(to, "-01-02")
  base <- "http://data.ssb.no/api/klass/v1/classifications/"

  dt <- set_url(
    base = base,
    from = from,
    to = to,
    klass = klass,
    source = "codes"
  )

  koDT <- dt[["codes"]]
  data.table::setDT(koDT)

  keepName <- c("code", "name")

  if (is.null(to)) {
    koDT[, setdiff(names(koDT), keepName) := NULL]
    koDT[, validTo := from][]
  } else {
    indN <- grep("InRequestedRange", names(koDT))
    valN <- names(koDT)[indN]
    koDT[, setdiff(names(koDT), c(keepName, valN)) := NULL][]
    data.table::setnames(koDT, old = valN, new = c("validFrom", "validTo"))
    data.table::setorderv(koDT, c("validTo", "code"))
  }

  if (isFALSE(date)) {
    dateVar <- c("validFrom", "validTo")
    selectCol <- dateVar[is.element(dateVar, names(koDT))]

    for (j in selectCol) {
      set(koDT, , j = j, value = format(as.Date(koDT[[j]]), "%Y"))
    }
  }

    return(koDT)
}

## base - What is base url
## source - Where the data is from eg. codesAt, corresponds etc
set_url <- function(base = NULL,
                    from = NULL,
                    to = NULL,
                    klass = NULL,
                    source = NULL) {
    baseUrl <- paste0(base, klass)

    if (is.null(to)) {
        sourceUrl <- paste0(source, "At")
        endUrl <- paste(baseUrl, sourceUrl, sep = "/")
        codeQry <- list(date = from)
    } else {
        endUrl <- paste(baseUrl, source, sep = "/")
        codeQry <- list(from = from, to = to)
    }

    koGET <- httr::GET(endUrl, query = codeQry)
    koTxt <- httr::content(koGET, as = "text")
    koJS <- jsonlite::fromJSON(koTxt)
}

## Ensure date is the required format
date_now <- function() {
    format(Sys.Date(), "%Y-%m-%d")
}

## Defunc
get_list <- function() {
    .Defunct("get_code")
}
