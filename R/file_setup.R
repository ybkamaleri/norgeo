#' Read raw CSV files downloaded from SSB to R object
#'
#' Read all CSV files in the folder specified if you prefer not to use `apply` family.
#' The objects can be available in the .GlobalEnv if argument `global = TRUE`.
#' **Important!** Any existing objects of similar names will be replaced without warning.
#' The object name will be something like "kommune200825_01" if granularity type is
#' \code{kommune} specified in the \code{type} argument.
#'
#' @param sep The separate symbols in the CSV files ie. `;` or `,`. If you leave it blank then 
#' appropriate symbols will be selected automatically. But if you should specify `encoding` type
#' then, you have to choose the right `sep` for the file.
#' @param encoding Select the suitable encoding
#' @param global If TRUE the objects will be available globally
#' 
#' @inheritParams geo_set 
#'
#' @export

read_csv <- function(folder.path,
                     type = NULL,
                     sep = c(NA, ";", ","),
                     encoding = NULL,
                     global = FALSE){

  if (is.null(type)) stop("Please specify granularity type eg. 'kommune'")
  
  ## args <- list(...)
  ## if(names(args) %in% c("grep.change", "change", "grep")) {
  ##   chgArg <- args[[1]]
  ## } else {
  ##   chgArg <- "change"
  ## }
  
  files <- select_ssb(grep.file = type,
                      grep.change = "change",
                      folder.path = folder.path)
  
  allFiles <- files[["allfile"]]
  outDT <- vector(mode = "list", length = length(allFiles))

  if (is.na(sep) || length(sep) > 1) sep = ".." #not specify sep

  if (is.null(encoding)){
    encoding <- getOption("encoding")
  }

  
  for (i in 1:length(allFiles)){
    file <- allFiles[i]
    fileName <- sub(".*\\/", "", allFiles[i])

    dt <- use_csv(file = file, sep = sep, encoding = encoding)
    
    cols <- c("parentCode", "shortName", "validFrom", "validTo")
    for (j in cols) set(dt, j = j, value = as.numeric(dt[[j]]))
    DT <- list(file = fileName, dt = dt)

    if (global){
      ymd <- format(Sys.Date(), '%y%m%d')
      fnum <- paste0(type, ymd, "_0", i)
      assign(fnum, DT, envir = .GlobalEnv)
    }

    outDT[[i]] <- DT
    
  }

  invisible(outDT)
}



#' Select files
#'
#' Use \code{regex} to select files
#'
#' @noRd

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



use_csv <- function(file, sep, encoding){

  ## obs!! problem with encoding for fread
  dt <- switch(sep,
               ";" = utils::read.csv2(file = file, encoding = encoding),
               "," = utils::read.csv(file = file, encoding = encoding),
               ".." = data.table::fread(file, fill = TRUE)
               )

  data.table::setDT(dt)
}


