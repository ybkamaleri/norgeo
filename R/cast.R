#' Add lower granularity when applicable
#'
#' This function will add lower granularity to the current granularity when applicable.
#' It means that when casting a kommune file, then columns for fylke will be added.
#' For grunnkrets file then both kommune and fylke code will be added. This is based on
#' the available table that can be downloaded from SSB. The file should be in a CSV format.
#'
#' @param file File names
#' @param keep.col Exisiting column names to be kept
#' @inheritParams add_change
#'
#' @examples
#'
#' \dontrun{
#' file = "ssb_grunnkrets_jan2020.csv"
#' type = "grunnkrets"
#' year = 2020
#' keep.col = c("code", "name")
#' folder = "C:/Users/geo/grunnkrets"
#'
#' DT <- cast_geo(file = file, type = type, year = year, folder.path = folder.path)
#' }
#'
#' @import data.table
#' @export

cast_geo <- function(file, type, year, folder.path, keep.col = c("code", "name")){

  fName <- file.path(folder.path, file)
  dt <- data.table::fread(fName, fill = TRUE)

  outCol <- setdiff(names(dt), keep.col)
  dt[, (outCol) := NULL]

  dt[, `:=`(border = year, geo = type)]

  ## Create reference tables
  kommune <- data.table(v1 = "fylke", v2 = 2)
  bydel <- data.table(v1 = c("kommune", "fylke"), v2 = c(2, 4))
  grunnkrets <- data.table(v1 = c("kommune", "fylke"), v2 = c(4, 6))
  refTab <- list(kommune = kommune, bydel = bydel, grunnkrets = grunnkrets)

  numRow <- nrow(refTab[[type]])

  for (i in seq_len(numRow)){

    colName <- refTab[[type]]$v1[i]
    numD <- refTab[[type]]$v2[i]
    subDigit <- paste0("\\d{", numD, "}$")

    dt[, (colName) := as.numeric(gsub(subDigit, "", code))]

  }

  return(dt[])
}
