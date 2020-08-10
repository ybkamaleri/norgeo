#' Set geo codes changes
#'
#' The downloaded file of geo codes for respective granularity from SSB in `csv`
#' format and codes that have changed copied from *Endringer* tab and pasted into
#' an Excel file. The `.csv` and `.xlsx` files will than be merged by running this function.
#'
#' Følgende dataset produseres:
#' \itemize{
#'   \item DT     : The whole merged dataset
#'   \item xl     : The xlsx file
#'   \item change : Path where the change file is if \code{raw = FALSE}
#'   \item code   : Path where the `csv` file is if \code{raw = FALSE}
#'   \item type   : The granularity level
#' }
#'
#' @param grep.file Felles navn til filene. Funker med bare noen ord eg. "jan2019"
#' @param grep.change Navn til endringsfil eg. "change"
#' @param year    Året geokoder gjelder
#' @param type    Hvilket nivå f.eks fylke, kommune osv.
#' @param folder.path   Sti til mappen hvor filene ligger
#' @param filegeo CSV fil for alle geokoder
#' @param filechg Excel fil for kode endringer
#' @param raw     If TRUE then specify direct filename in \code{filegeo} and \code{filechg}
#'
#' @import data.table
#' @export


geo_set <- function(
                    grep.file = NULL,
                    grep.change = NULL,
                    year = NULL,
                    type = c("land",
                             "fylke",
                             "kommune",
                             "bydel",
                             "grunnkrets"),
                    folder.path = NULL,
                    filegeo = NULL,
                    filechg = NULL,
                    raw = FALSE
                    ){


    if (is.null(year)) {stop("Missing year")}

    if (!is.null(filegeo) & !is.null(grep.file))
    {stop("Only one of them need info ie. filegeo or grep.file")}

    ## Files
    if (raw){

        fileChg <- filechg
        fileGeo <- filegeo

    } else {

        ssb <- select_ssb(grep.file = grep.file,
                          grep.change = grep.change,
                          folder.path = folder.path)

        fileGeo <- ssb$allfile
        fileChg <- ssb$chgfile

    }

    fileErr <- length(fileGeo) + length(fileChg)
    if (fileErr != 2) {stop("File not found or too many! run `select_ssb()`")}

    ## Changes files - must be an Excel file
    ## -------------------------------------
    xlTbl <- readxl::read_excel(fileChg)

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

    ## Current Geo - must be a CSV file
    ## --------------------------------
    dt <- data.table::fread(fileGeo, fill = TRUE)

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

    list(DT = DT[], xl = xlTbl[], change = fileChg, code = fileGeo, type = type)

}
