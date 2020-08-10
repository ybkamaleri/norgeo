#' Multiple changes
#'
#' Track codes that have changed multiple times. Join changes of geo codes from previous change ie.
#' code that have changed in 2018 # and again have new changes in 2020. Then get the previous codes
#' in 2018 from previous code columns.
#' For example the code 30240317 (in 2020) was 2190317 (2019) but previously was 2190314 (2018)
#'
#' @param newCode Object produced from `geo_set` for newer codes
#' @param preCode Object produced from `geo_set` for previous codes
#' @param raw     If FALSE then specify a data.table object
#'
#' @import data.table
#' @export

merge_multi <- function(newCode = NULL, preCode = NULL, raw = TRUE){

  if (raw){
    elMix <- check_element(newCode, preCode)
  } else {
    vecNew <- newCode[["DT"]]$prev
    indelm <- is.element(vecNew, preCode[["code"]])
    elMix <- data.table(chg = vecNew[indelm])
  }

  dtNew <- newCode[["DT"]]
  altNew <- dtNew[prev  %in% elMix$chg, ]

  if (raw){
    dtPre <- preCode[["DT"]]
    altPre <- dtPre[code  %in% elMix$chg, ]
  } else {
   altPre <- preCode
  }

  allFile <- merge(altNew, altPre,
                   by.x = "prev", by.y = "code",
                   all = TRUE)

  keepCols <- c("code", "name.x", "prev.y", "prevName.y", "year.y")
  newName <- c("name", "prev", "prevName", "year")

  allFile[, setdiff(names(allFile), keepCols) := NULL]
  setnames(allFile, keepCols[-1], newName)

  fileOut <- rbindlist(list(allFile, altNew))
  setkey(fileOut, code, year)

  return(fileOut[])
}



## Check for multiple changes
##
## Check if current codes in previous year is.element in previous codes of current year
## showing that the codes have changed again since previous change

check_element <- function(newCode, preCode){

  DT <- newCode[["DT"]]
  dt <- preCode[["DT"]]
  vecNew <- DT$prev
  vecOld <- dt[!is.na(year), code]
  chg <- is.element(vecOld, vecNew)
  sumChg <- sum(chg)
  vecChg <- vecOld[chg]

  list(total = sumChg, chg = vecChg)
}
