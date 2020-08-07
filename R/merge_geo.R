#' Merge all geo codes
#'
#' Create a table with all geo change and year. The files for the argument must be
#' the output object from `add_change()` function.
#'
#' @param files A list of objects to be merged. The order must be from lowest to highest year.
#' @param output Data output which can be one of these:
#' \itemize{
#'   \item `complete` : A list of all different outputs
#'   \item `all`      : Dataset for all data
#'   \item `change`   : Dataset that include only codes that have changed
#'   \item `duplicate`: Dataset for codes that have duplicated of previous codes
#' }
#'
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' DT <- merge_geo(list(kommune2018, kommune2019, kommune2020))
#' }
#'
#' @export

merge_geo <- function(files, output = c("complete", "all", "change", "duplicate")){

  if (inherits(files, "list") == 0) stop("Object for 'files' should be a list", call. = TRUE)

  fileMx  <- length(files)
  ## create cross join as a reference table
  ind <- CJ(1:fileMx, 1:fileMx)
  indSel <- ind[V1 != V2, ][V1 < fileMx, ][V1 < V2, ]

  ## create empty list for multiple changes
  join_dt <- vector(mode = "list", length = nrow(indSel))

  for (i in seq_len(nrow(indSel))){

    newFile <- indSel[[2]][i]
    preFile <- indSel[[1]][i]

    d <- merge_multi(newCode = files[[newFile]],
                     preCode = files[[preFile]])

    join_dt[[i]] <- d

  }

  joinDT <- rbindlist(join_dt)

  ## Change once
  indChg <- ind[V1 - V2 == 1, ] #reference table
  chg_dt <- vector(mode = "list", length = nrow(indChg))

  for (i in seq_len(nrow(indChg))){

    newFile <- indChg[[1]][i]
    preFile <- indChg[[2]][i]

    d <- find_change(newCode = files[[newFile]],
                     preCode = files[[preFile]])

    chg_dt[[i]] <- d
  }

  chgDT <- rbindlist(chg_dt)

  ## Keep only those with valid codes for recent year
  recentCodes <- unique(files[[fileMx]]$DT$code)
  currDT <- chgDT[code  %in% recentCodes, ]

  ## Merge all changes ie. multiple and change once
  changeDT <- rbindlist(list(joinDT, currDT))
  setkey(changeDT, code)

  ## Clean up duplicated raws if exist and
  ## delete codes that have not changed ie. column for 'prev' is NA
  indX <- changeDT[, .I[duplicated(changeDT)]]

  if (sum(indX > 0)){
    dtx <- changeDT[-(indX)]
    dtz <- dtx[!is.na(prev), ]

    ## check duplicate for find_change() function.
    ## and keep only those that are in newest geo due to multiple changes
    dupInx <- dtz[, .I[(duplicated(prev) | duplicated(prev, fromLast = TRUE))]]
    ## split to 2 DT with and without duplicated 'prev'
    uniDT <- dtz[-dupInx]
    dupDT <- dtz[dupInx]

    ## Clean duplicated codes if codes aren't in newest geo
    dupCodes <- unique(dupDT$code)
    keepInd <- is.element(dupCodes, recentCodes)
    keepCodes <- dupCodes[keepInd]
    dupUni <- dupDT[code %in% keepCodes, ]

    ## Merge back to the other DT without duplicated previous codes
    CDT <- rbindlist(list(uniDT, dupUni))
    setkey(CDT, code)
  } else {
    CDT <- changeDT
    dupDT <- 0
  }

  ## Merge everything to recent geo list
  ## ---------------------------------------
  dupCodesChg <- unique(CDT$code)## codes that are allready in the changes table
  ## keep only codes in recent geo list that aren't in the changes table
  otherDT <- files[[fileMx]]$DT[!(code  %in% dupCodesChg), ]
  geoDT <- rbindlist(list(otherDT, CDT), fill = TRUE)
  setkey(geoDT, code, year)

  ## Recode mulitple codes that haven't been converted to recent geo code
  eks <- setdiff(geoDT$code, recentCodes) #get codes that aren't in recent geo

  for (i in eks){
    currGeo <- geoDT[prev == i, ][["code"]]
    geoDT[code == i, code := currGeo]
  }

  setkey(geoDT, code, year)

  DTout <- switch(output,
                  "complete" = list(dupDT = dupDT, chgDT = CDT, allDT = geoDT),
                  "all" = allDT,
                  "change" = CDT,
                  "duplicate" = dupDT,
                  list(dupDT = dupDT, chgDT = CDT, allDT = geoDT))

  return(DTout[])

}
