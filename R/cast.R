#' Add lower granularity to all files
#'
#' Add all granularity levels. Here the structure is important.
#' The the order for types should be similar to the order in files.
#'
#' @param file Files names with complete path to the files
#' @param type Types equivalent to the file names. Must be the same order with file names
#' @inheritParams geo_set
#' @inheritParams cast_code
#'
#' @examples
#' \dontrun{
#' file = c("C:/Geo/fylke/fylke2020.csv",
#'           "C:/Geo/kommune/kommune2020.csv",
#'           "C:/folder2/bydel2020.csv" )
#'
#' types = c("fylke","kommune","bydel")
#'
#' DT <- geo_cast(files=files, type=types, year=2020)
#' }
#'
#' @import data.table
#' @export

geo_cast <- function(file,
                     type = NULL,
                     year = NULL,
                     keep.col = c("code", "name"),
                     folder.path = NULL){

  if (length(file) != length(type))
    stop("Length of file and type is different!")

  allFiles <- file_folder(file, file.path)
  tblFile <- data.table(file = allFiles, type = type)

  ## allocate template for memory use
  listDT <- vector(mode = "list", length = nrow(tblFile))

  for (i in seq_len(nrow(tblFile))){

    fileName <- tblFile[i, file]
    typeName <- tblFile[i, type]

    dt <- cast_code(file = fileName,
                   type = typeName,
                   year = year,
                   keep.col = keep.col)

    listDT[[i]] <- dt

  }

  DT <- rbindlist(listDT, fill = TRUE)

  ## Fill the granularity level columns with its own code
  for (i in type){
    DT[granularity == i, (i) := code]
  }

  ## Add Land
  norge <- list(code = 0, name = "Norge", border = 2020, granularity = "land")
  DTout <- rbindlist(list(DT, norge), fill = TRUE)
  data.table::setkey(DTout, code)

  return(DTout[])
}



#' Add lower granularity when applicable
#'
#' This function will add lower granularity to the current granularity when applicable.
#' It means that when casting a kommune file, then columns for fylke will be added.
#' For grunnkrets file then both kommune and fylke code will be added. This is based on
#' the available table that can be downloaded from SSB. The file should be in a CSV format.
#'
#' @param file File names
#' @param keep.col Exisiting column names to be kept
#' @inheritParams geo_set
#'
#' @examples
#'
#' \dontrun{
#' file = "c:/Users/geo/ssb_grunnkrets_jan2020.csv"
#' type = "grunnkrets"
#' year = 2020
#' cols = c("code", "name")
#'
#' DT <- cast_code(file = file, type = type, year = year, keep.col=cols)
#' }
#'
#' @import data.table
#' @export

cast_code <- function(file,
                      type,
                      year,
                      folder.path = NULL,
                      keep.col = c("code", "name")
                      ){

  fName <- file_folder(file, folder.path)

  dt <- data.table::fread(fName, fill = TRUE)

  ## Check keep.col exist
  colX <- sum(is.element(keep.col, names(dt)))
  if (colX == 0) stop("Selected columns to keep doesn't exist!")

  ## keep the selected columns
  outCol <- setdiff(names(dt), keep.col)
  dt[, (outCol) := NULL]

  dt[, `:=`(border = year, granularity = type)]

  ## Fylke has no lower granularity, so skip this
  type <- tolower(type)

  refTab <- switch(type,
                   "kommune" = data.table(v1 = "fylke",
                                          v2 = 2),
                   "bydel" = data.table(v1 = c("fylke", "kommune"),
                                        v2 = c(4, 2)),
                   "grunnkrets" = data.table(v1 = c("fylke", "kommune", "bydel"),
                                             v2 = c(6, 4, 2)),
                   data.table(v1 = NULL,
                              v2 = NULL)
                   )

  if (type != "fylke"){

    for (i in seq_len(nrow(refTab))){
      colName <- refTab$v1[i]
      numD <- refTab$v2[i]
      subDigit <- paste0("\\d{", numD, "}$")

      dt[, (colName) := as.numeric(gsub(subDigit, "", code))][]
    }
  }

  return(dt)
}
