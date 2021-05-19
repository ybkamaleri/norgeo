#' Get change from complete geo list
#'
#' Create table for code changes when table that track the changes isn't already available from SSB.
#' A reference column name for merging id other than geo code is needed. The
#' most relevant will be `name` column which is more likely relatively consistent over the
#' years. Therefore this function should be used with caution.
#' The output can be as `xlsx` or `csv` file.
#'
#' @param files A list of files to find geo codes changes
#' @param years A list of years for these files
#' @param key.col Column name as an id for merging eg. `name`
#' @param file.type Which format to save the output. Option are `xls` or `csv`.
#' @param des.path Destination folder where the file to be saved
#' @inheritParams geo_set
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' files <- c("ssb_bydel_jan2004.csv", "ssb_bydel_jan2018.csv", "ssb_bydel_jan2020.csv")
#' years <- c(2004, 2018, 2020)
#' folder <- "C:/geo/bydel"
#' des <- "C:/geo/bydel/output"
#'
#' geo_change(files = files, years = years, type = "bydel", folder.path = folder, file.type = "xls", des.path = des)
#' }
#'
#' @export

geo_change <- function(files = NULL,
                       years = NULL,
                       type = NULL,
                       key.col = NULL,
                       folder.path = NULL,
                       file.type = c("none", "xls", "csv", "excel"),
                       des.path = NULL) {
  if (is.null(key.col)) stop("Need column name as an ID for merging. Use key.col argument.")

  if (length(file.type) > 1) file.type <- "none"
  outputFile <- switch(file.type,
                       "xls" = ".xlsx",
                       "excel" = ".xlsx",
                       "csv" = ".csv",
                       "none"
                       )

  if (inherits(files, "list") == 0) {
    files <- unlist(files)
  }
  if (inherits(years, "list") == 0) {
    years <- unlist(years)
  }

  nFiles <- length(files)
  nYears <- length(years)

  nChk <- identical(nFiles, nYears)
  if (isFALSE(nChk)) stop("Number of files and years are not equal!")

  cjtbl <- CJ(1:nFiles, 1:nYears)
  reftbl <- cjtbl[V2 - V1 == 1, ]

  ## Make empty list for memory allocation
  listDT <- vector(mode = "list", length = nrow(reftbl))

  for (i in seq_len(nrow(reftbl))) {
    newRef <- reftbl[[2]][i]
    preRef <- reftbl[[1]][i]

    newFile <- file_folder(files[newRef], folder.path)
    preFile <- file_folder(files[preRef], folder.path)

    newYr <- years[newRef]
    preYr <- years[preRef]

    DT <- change_table(
      dt = list(
        newD = newFile,
        preD = preFile
      ),
      year = list(
        y1 = newYr,
        y2 = preYr
      ),
      key.col = key.col
    )


    if (outputFile == "none") {
      listDT[[i]] <- DT
    } else {
      if (is.null(des.path)) stop("Destination folder to save file is missing!")
      tempName <- paste0(type, "_change_", newYr)
      fileName <- file.path(des.path, tempName)
      write_tbl(DT, fileName, file.type) # from utils.R

      listDT[[i]] <- list(normalizePath(paste0(fileName, outputFile), winslash = "/"))
    }
  }

  allDT <- rbindlist(listDT)
}


## Create reference table for change 1 x 1
change_table <- function(dt, year, key.col) {
  newdt <- data.table::fread(dt$newD, fill = TRUE)
  predt <- data.table::fread(dt$preD, fill = TRUE)

  newdt[, year := year$y1]
  predt[, year := year$y2]

  newdt[predt, on = key.col, `:=`(preCode = i.code, preYear = i.year)]

  chgDT <- newdt[code != preCode][]
}
