#' Set geo codes changes with API
#'
#' This is a wrapper function to get geo code list and code changes. But code changes
#' will only valid for every two years. To find codes changes for multiple years, use
#' \code{get_change} function.
#'
#' The following dataset will be generated:
#' \itemize{
#'   \item data   : The whole merged dataset
#'   \item change : The code changes
#'   \item type   : The granularity level
#' }
#'
#' @param type    Type of regional granularity ie. fylke, kommune etc.
#' @param year    Year of the selected code list
#' @inheritParams get_list
#'
#' @import data.table
#' @export


get_set <- function(type = c(
                      "fylke",
                      "kommune",
                      "bydel",
                      "grunnkrets"
                    ),
                    year = NULL,
                    from = NULL) {
  type <- match.arg(type)

  ## if (is.null(year))
  ##   year <- as.integer(format(Sys.Date(), "%Y"))

  ## if (is.null(from))
  ##   from <- as.numeric(year) - 1


  ## Get changes in the geo codes
  xlTbl <- get_change(type, year, from)
  oldCols <- c("oldCode", "oldName", "newCode", "newName", "changeOccurred")
  newCols <- c("prev", "prevName", "curr", "currName", "year")
  data.table::setnames(xlTbl, oldCols, newCols, skip_absent = TRUE)

  ## Empty data.table needed for merging with all data
  if (nrow(xlTbl) == 0) {
    xlTbl <- stats::setNames(data.table(matrix(nrow = 0, ncol = length(newCols))), newCols)
  }

  ## Get the current geo codes
  dt <- get_list(type, year, from)

  mainCols <- c("code", "name")
  nCols <- names(dt)
  same <- identical(mainCols, nCols)
  ## keep only Code and Name
  if (same == 0) {
    dt[, setdiff(names(dt), mainCols) := NULL]
  }

  ## Merge everything
  ## ----------------
  if (nrow(xlTbl) != 0) {
    DT <- xlTbl[dt, on = c(curr = "code")]
    DT[, currName := NULL]
    setnames(DT, "curr", "code")
  } else {
    DT <- cbind(dt, xlTbl[, -c("curr", "currName")])
  }

  otherCols <- setdiff(names(DT), mainCols)
  setcolorder(DT, c(mainCols, otherCols))

  list(
    data = DT[],
    change = xlTbl[],
    type = type,
    year = year
  )
}
