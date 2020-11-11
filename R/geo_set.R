#' Set geo codes changes
#'
#' This is a wrapper function to get geo code list and code changes.
#'
#' The following dataset will be generated:
#' \itemize{
#'   \item data   : The whole merged dataset
#'   \item xl     : The code changes
#'   \item type   : The granularity level
#' }
#'
#' @param type    Type of regional granularity ie. fylke, kommune etc.
#' @param year    Year of the selected code list
#'
#' @import data.table
#' @export


geo_set <- function(type = c("fylke",
                             "kommune",
                             "bydel",
                             "grunnkrets"),
                    year = NULL                    
                    ){

  type <- match.arg(type)
  
  if (is.null(year)) {stop("Missing year")}

  ## if (!is.null(filegeo) & !is.null(grep.file))
  ##                     {stop("Only one of them need info ie. filegeo or grep.file")}

  ## raw = FALSE
  ## if (!is.null(filegeo)) {
  ##   fileGeo = filegeo
  ##   raw = TRUE
  ## }
  
  ## if (!is.null(filechg)) {
  ##   fileChg = filechg
  ##   raw = TRUE
  ## }
  
  ## ## Files
  ## if (raw){

  ##   fileChg <- filechg
  ##   fileGeo <- filegeo

  ## } else {

  ##   ssb <- select_ssb(grep.file = grep.file,
  ##                     grep.change = grep.change,
  ##                     folder.path = folder.path)

  ##   fileGeo <- ssb$allfile
  ##   fileChg <- ssb$chgfile
  ## }


  ## fileErr <- length(fileGeo) + length(fileChg)
  ## if (fileErr != 2) {stop("File not found or too many! run `select_ssb()`")}
  
  ## ## Changes files - must be an Excel file
  ## ## -------------------------------------
  ## xlTbl <- readxl::read_excel(fileChg)

  ## names(xlTbl) <- c("new", "old")
  ## setDT(xlTbl)

  ## expNum <- switch(type,
  ##                  "kommune" = "[^0-9]+",
  ##                  "grunnkrets" = "\\s.*",
  ##                  "fylke" = "[^0-9]+",
  ##                  "[^0-9]\\s.*")

  ## expName <- switch(type,
  ##                   "kommune" = "\\d+\\D[^\\s]",
  ##                   "grunnkrets" = "[^A-Za-z]",
  ##                   "[^A-Za-z]")

  ## ## Extract code and name separately
  ## xlTbl[!is.na(new), curr := as.numeric(gsub(expNum, "", new))]
  ## xlTbl[!is.na(new), currName := gsub(expName, "", new)]
  ## xlTbl[!is.na(old), prev := as.numeric(gsub(expNum, "", old))]
  ## xlTbl[!is.na(old), prevName := gsub(expName, "", old)]
  ## xlTbl[, year := year, ]

  ## ## replace missing string with last observed carried forward (locf)
  ## data.table::setnafill(xlTbl, type = "locf", cols = "curr") #only for numeric
  ## ## For string
  ## while(length(ind <- which(is.na(xlTbl$currName))) > 0){
  ##   xlTbl$currName[ind] <- xlTbl$currName[ind - 1]
  ## }

  ## xlTbl[, c("new", "old") := NULL]

  xlTbl <- get_change(type, year)
  data.table::setnames(xlTbl,
                       c("oldCode", "oldName", "newCode", "newName", "changeOccurred"),
                       c("prev", "prevName", "curr", "currName", "year"))
  

  ## Current Geo - must be a CSV file
  ## --------------------------------
  ## dt <- data.table::fread(fileGeo, fill = TRUE)

  dt <- get_list(type, year)

  mainCols <- c("code", "name")
  nCols <- names(dt)
  same <- identical(mainCols, nCols)
  ##keep only Code and Name
  if (same == 0){
    dt[, setdiff(names(dt), mainCols) := NULL]
  }

  ## Merge everything
  ## ----------------
  DT <- xlTbl[dt, on = c(curr = "code")]
  DT[, currName := NULL]
  setnames(DT, "curr", "code")
  otherCols <- setdiff(names(DT), mainCols)
  setcolorder(DT, c(mainCols, otherCols))

  list(data = DT[],
       xl = xlTbl[],
       type = type,
       year = year)

}
