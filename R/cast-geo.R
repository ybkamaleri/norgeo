#' Cast geo granularity from API
#'
#' Add geo granularity levels to all sides
#'
#' @param year Which year the codes are valid from. If NULL then current year
#'   will be selected.
#'
#' @import data.table
#' @export

cast_geo <- function(year = NULL) {
  level <- sourceCode <- kommune <- fylke <- grunnkrets <- bydel <- NULL

  if (is.null(year)) {
    year <- as.integer(format(Sys.Date(), "%Y"))
  }

  geos <- c("fylke", "kommune", "bydel", "grunnkrets")

  DT <- list()
  ## Get geo codes
  for (i in seq_along(geos)) {
    DT[[geos[i]]] <- norgeo::get_code(geos[i], from = year)
    DT[[geos[i]]][, level := geos[i]]
  }
  dt <- data.table::rbindlist(DT)

  ## SSB has correspond data only for
  ## - bydel-grunnkrets
  ## - kommune-grunnkrets
  ## - fylke-kommue

  COR <- list(
    gr_bydel = c("bydel", "grunnkrets"),
    gr_kom = c("kommune", "grunnkrets"),
    kom_fylke = c("fylke", "kommune")
  )

  for (i in seq_along(COR)) {
    COR[[i]] <- norgeo::find_correspond(COR[[i]][1], COR[[i]][2], from = year)

    keepCols <- c("sourceCode", "targetCode")
    delCol <- base::setdiff(names(COR[[i]]), keepCols)
    COR[[i]][, (delCol) := NULL]
    data.table::setnames(COR[[i]], "targetCode", "code")
  }

  dt[COR$gr_bydel, on = "code", bydel := sourceCode]
  dt[COR$gr_kom, on = "code", kommune := sourceCode]
  dt[COR$kom_fylke, on = c(kommune = "code"), fylke := sourceCode]

  ## Merge geo code
  ## Add higher granularity that aren't available via correspond API
  dt[
    level == "bydel", kommune := gsub("\\d{2}$", "", code)
  ][
    level == "bydel", fylke := gsub("\\d{4}$", "", code)
  ][
    level == "bydel", bydel := code
  ]

  dt[
    level == "kommune", fylke := gsub("\\d{2}$", "", code)
  ][
    level == "kommune", kommune := code
  ]

  ## Only run this after adding lower granularity
  dt[level == "grunnkrets", grunnkrets := code]
  dt[level == "fylke", fylke := code]

  data.table::setcolorder(dt, c(
    "code", "name", "validTo", "level",
    "grunnkrets", "kommune", "fylke", "bydel"
  ))

  return(dt[])
}

## Correspond codes can be empty if nothing has changed in
## that specific year and need to get from previous year or even
## year before before previous year etc..etc..

#' @export
#' @rdname get_correspond
find_correspond <- function(type, correspond, from) {
  ## type: Higher granularity eg. fylker
  ## correspond: Lower granularity eg. kommuner
  stat <- list(rows = 0, from = from)
  nei <- 0
  while (nei < 1) {
    dt <- norgeo::get_correspond(type, correspond, from)
    nei <- nrow(dt)
    stat$rows <- nei
    stat$from <- from
    from <- from - 1
  }
  message("Data for ", correspond, " to ", type, " is from ", stat$from, " with ", stat$rows, " rows")
  return(dt)
}
