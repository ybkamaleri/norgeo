#' Legg til geokoder endringer
#'
#' Filen for gjeldende geokoder i \code{csv} format og filen om koder som har
#' endret i \code{xlsx} skal legges sammen. Disse filene hentes fra SSB
#' hjemmeside.
#'
#' Følgende dataset produseres:
#' \itemize{
#'   \item DT     : Hele datasettet med kode endringer
#'   \item xl     : Endringer fra Excel filen
#'   \item change : Sti til fil endringe hvis \code{raw = FALSE}
#'   \item code   : Sti til kodefilen hvis \code{raw = FALSE}
#' }
#'
#' @param filegeo CSV fil for alle geokoder
#' @param filechg Excel fil for kode endringer
#' @param year    Året geokoder gjelder
#' @param type    Hvilket nivå f.eks fylke, kommune osv.
#' @param path    Sti til mappen for filene ligger
#' @param raw     Hvis FALSE så er \code{filegeo} og \code{filechg} en ferdig
#' laget object i .GlobalEnv dvs. ikke er CSV og Excel fil
#'
#' @import data.table
#' @export


add_change <- function(filegeo,
                       filechg,
                       year,
                       type = "land",
                       path = NULL,
                       raw = TRUE){

  ## Files
  if (!is.null(path)){
    filePath <- normalizePath(path, winslash = "/")

    fileNew <- path(filePath, filegeo) #geo New
    fileChg <- path(filePath, filechg) #geo Change
  } else {
    fileNew <- filegeo
    fileChg <- filechg
  }


  ## Geo change
  ## Use Excel for changes file
  if (raw){
    xlTbl <- readxl::read_excel(fileChg)
  } else {
    xlTbl <- filechg
  }

  names(xlTbl) <- c("new", "old")
  setDT(xlTbl)

  expNum <- switch(type,
                   "kommune" = "[^0-9]+",
                   "grunnkrets" = "\\s.*",
                   "fylke" = "[^0-9]+",
                   "[^0-9]\\s.*")

  expName <- switch(type,
                    "kommune" = "\\d+\\D[^\\s]",
                    "grunnkrets" = "[^A-Za-z]",
                    "[^A-Za-z]")

  ## Extract code and name separately
  xlTbl[!is.na(new), curr := as.numeric(gsub(expNum, "", new))]
  xlTbl[!is.na(new), currName := gsub(expName, "", new)]
  xlTbl[!is.na(old), prev := as.numeric(gsub(expNum, "", old))]
  xlTbl[!is.na(old), prevName := gsub(expName, "", old)]
  xlTbl[, year := year, ]

  ## replace missing string with last observed carried forward (locf)
  setnafill(xlTbl, type = "locf", cols = "curr") #only for numeric
  ## For string
  while(length(ind <- which(is.na(xlTbl$currName))) > 0){
    xlTbl$currName[ind] <- xlTbl$currName[ind - 1]
  }

  xlTbl[, c("new", "old") := NULL]

  mainCols <- c("code", "name")

  ## New geo
  if (raw){
    dt <- data.table::fread(fileNew, fill = TRUE)
  } else {
    dt <- filegeo
  }

  nCols <- names(dt)
  same <- identical(mainCols, nCols)
  ##keep only Code and Name
  if (same == 0){
    dt[, setdiff(names(dt), mainCols) := NULL]
  }

  ## Merge
  DT <- xlTbl[dt, on = c(curr = "code")]
  DT[, currName := NULL]
  setnames(DT, "curr", "code")
  otherCols <- setdiff(names(DT), mainCols)
  setcolorder(DT, c(mainCols, otherCols))

  list(DT = DT[], xl = xlTbl[], change = fileChg, code = fileNew)

}
