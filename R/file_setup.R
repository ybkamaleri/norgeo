#' Select files
#'
#' Use \code{regex} to select files
#' @inheritParams add_change
#'
#' @export

select_ssb <- function(grep.file = NULL,
                       grep.change = NULL,
                       folder.path = NULL){

  files <- fs::dir_ls(folder.path)
  filInd <- grep(grep.file, files, ignore.case = TRUE)
  chgInd <- grep(grep.change, files[filInd])
  chgFil <- files[filInd][chgInd]
  codeList <- files[filInd][-chgInd]
  list(chgfile = chgFil, allfile = codeList)
}


#' Read raw CSV files from SSB to R object
#'
#' Read all CSV files in the folder specified. The objects can be available
#' in the .GlobalEnv if argument `global = TRUE`. **Important!** Any existing
#' objects of similar names will be replaced without warning.
#' The object name will be something like "kommune_01" if
#' \code{kommune} is specified in the \code{type} argument.
#'
#' @param global If TRUE the objects will be available globally
#' @param encoding Select the suitable encoding
#' @param sep The separate symbols in the CSV file
#' @inheritParams add_change
#' 
#'
#' @export

read_ssb <- function(folder.path,
                     type = NULL,
                     sep = c(";", "."),
                     encoding = NULL, 
                     global = FALSE){

  if (is.null(type)) stop("Please specify type eg. 'kommune'")
  
  files <- select_ssb(grep.file = type,
                      grep.change = "change",
                      folder.path = folder.path)
  
  allFiles <- files[["allfile"]]
  outDT <- vector(mode = "list", length = length(allFiles))

  if (missing(sep)) sep = ".."
  if (is.null(encoding)){
    encoding <- getOption("encoding")
  }

  
  for (i in 1:length(allFiles)){
    file <- allFiles[i]
    fnum <- paste0(type, "_0", i)

    dt <- use_csv(file = file, sep = sep, encoding = encoding)
    
    cols <- c("parentCode", "shortName", "validFrom", "validTo")
    for (j in cols) set(dt, j = j, value = as.numeric(dt[[j]]))
    DT <- list(file = file, dt = dt)

    if (global){
      assign(fnum, DT, envir = .GlobalEnv)
    }

    outDT[[i]] <- DT
    
  }

  return(outDT)
}



use_csv <- function(file, sep, encoding){

  ## obs!! problem with encoding for fread
  dt <- switch(sep,
               ";" = utils::read.csv2(file = file, encoding = encoding),
               "." = utils::read.csv(file = file, encoding = encoding),
               ".." = data.table::fread(file, fill = TRUE)
               )

  data.table::setDT(dt)
  return(dt)
}
