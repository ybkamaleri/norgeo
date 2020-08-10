#' Get change from complete geo list
#'
#' Create table for code changes when table tracking the changes isn't already available from SSB.
#' This function will be using a reference colum other than geo code ie. names. Therefore this
#' function should be used with caution. The output can be save as `xlsx` or `csv`.
#'
#' @param files A list of files to find geo codes changes
#' @param years A list of years for these files
#' @param save Which format to use to save the output
#' @param des.path Destination folder where the file to be saved
#' @inheritParams geo_set
#'
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' files = c("ssb_bydel_jan2004.csv", "ssb_bydel_jan2018.csv", "ssb_bydel_jan2020.csv")
#' years = c(2004, 2018, 2020)
#' folder = "C:/geo/bydel"
#' des = "C:/geo/bydel/output"
#'
#' get_change(files=files, years=years, type="bydel",folder.path=folder, save="xls", des.path=des)
#' }
#'
#' @export


get_change <- function(files,
                       years,
                       type,
                       folder.path,
                       save = c("no", "xls", "csv"),
                       des.path){

  if (missing(save)) save = "no"
  xl <- grepl("xl", save, ignore.case = TRUE)
  if (xl) save = "xls"
  save = tolower(save)

  if (inherits(files, "list") == 0) {files <- unlist(files)}
  if (inherits(years, "list") == 0) {years <- unlist(years)}

  nFiles <- length(files)
  nYears <- length(years)

  nChk <- identical(nFiles, nYears)
  if (isFALSE(nChk)) stop("Number of files and years are not equal!")

  cjtbl <- CJ(1:nFiles, 1:nYears)
  reftbl <- cjtbl[V2 - V1 == 1, ]

  ## Make empty list for memory allocation
  listDT <- vector(mode = "list", length = nrow(reftbl))

  for (i in seq_len(nrow(reftbl))){

    newRef <- reftbl[[2]][i]
    preRef <- reftbl[[1]][i]

    newFile <- file.path(folder.path, files[newRef])
    preFile <- file.path(folder.path, files[preRef])

    newdt <- data.table::fread(newFile, fill = TRUE)
    predt <- data.table::fread(preFile, fill = TRUE)

    newdt[, year := years[newRef]]
    predt[, year := years[preRef]]

    ## Find codes that have changed
    codeChg <- setdiff(newdt$code, predt$code)

    ## merge by reference ie. 2018 to 2020 data
    newdt[predt, on = "name", `:=`(precode = i.code, preyr = i.year)]

    ## Filter those names that have changed codes
    newCol <- paste0("jan", years[newRef])
    preCol <- paste0("jan", years[preRef])
    chgDT <- newdt[code %in% codeChg, ]

    chgDT[, (newCol) := paste(code, name, sep = " - ")]
    chgDT[, (preCol) := paste(precode, name, sep = " - ")]

    colDel <- setdiff(names(chgDT), c(newCol, preCol))
    chgDT[, (colDel) := NULL]

    if (save == "no"){
      listDT[[i]] <- chgDT
    } else {
      tempName <- paste0(type, "_change_", newCol, ".xlsx")
      fileName <- file.path(des.path, tempName)
      openxlsx::write.xlsx(chgDT, fileName)
    }
  }

  allDT <- rbindlist(listDT)

  return(allDT[])
}
