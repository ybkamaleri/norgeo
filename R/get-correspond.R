#' Get geo corresponds
#'
#' This function will get the corresponding geo code of specific granuality via API from SSB whenever available.
#'
#' @param type Higer granularity from specified correspond arg.
#' @param correspond Lower granularity from the specified type arg.
#' @param from Specify the starting year for range period. Current year is the default.
#' @param to Specify the year to end the range period. Indefinite is used when not specified.
#' @param dt Output as data.table
#'
#' @examples
#' \dontrun{
#' df <- get_correspond("fylke", "kommune", 2020)
#' }
#'
#' @export

get_correspond <- function(type = c(
                             "fylke",
                             "kommune",
                             "bydel",
                             "grunnkrets"
                           ),
                           correspond = c(
                             "fylke",
                             "kommune",
                             "bydel",
                             "grunnkrets"
                           ),
                           from = NULL,
                           to = NULL,
                           dt = TRUE) {
  type <- match.arg(type)
  klass <- switch(type,
    fylke = 104,
    kommune = 131,
    bydel = 103,
    grunnkrets = 1
    )

  correspond <- match.arg(correspond)
  corr <- switch(correspond,
    fylke = 104,
    kommune = 131,
    bydel = 103,
    grunnkrets = 1
    )
  baseUrl <- "http://data.ssb.no/api/klass/v1/classifications/"
  klsUrl <- paste0(baseUrl, klass)

  if (is.null(from)) {
    year <- as.character(format(Sys.Date(), "%Y"))
    from <- paste0(year, "-01-02")
  } else {
    from <- paste0(from, "-01-02")
  }

  if (!is.null(to)) to <- paste0(to, "-01-02")

  dt <- set_corr(
    from = from,
    to = to,
    id = corr,
    url = klsUrl,
    dt = dt
  )
}


set_corr <- function(from = NULL,
                     to = NULL,
                     id = NULL,
                     url = NULL,
                     dt = TRUE) {
  if (is.null(to)) {
    corrUrl <- paste0(url, "/correspondsAt")
    codeQry <- list(targetClassificationId = id, date = from)
  } else {
    corrUrl <- paste0(url, "/corresponds")
    codeQry <- list(targetClassificationId = id, from = from, to = to)
  }

  koGET <- httr::GET(corrUrl, query = codeQry)
  koTxt <- httr::content(koGET, as = "text")
  koJS <- jsonlite::fromJSON(koTxt)
  koDT <- koJS[["correspondenceItems"]]

  if (dt) {
    data.table::setDT(koDT)
  }

  return(koDT)
}
