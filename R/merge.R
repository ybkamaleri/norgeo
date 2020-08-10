#' Merge all geo codes
#'
#' Create a table with all geo change and year. The files for the argument must be
#' the output object from `geo_set()` function.
#'
#' @param files A list of objects to be merged. The order must be from lowest to highest year.
#' @param output Data output which can be one of these:
#' \itemize{
#'   \item `all`      : Dataset for all data. This is the default output
#'   \item `change`   : Dataset that include only codes that have changed
#'   \item `split`    : Dataset for codes that have been divided to at least two newer codes
#'   \item `merge`    : Dataset for at least two codes that are been merged into one
#'   \item `complete` : A list of all different outputs
#' }
#'
#' @import data.table
#'
#' @examples
#'
#' \dontrun{
#' DT <- geo_merge(list(kommune2018, kommune2019, kommune2020))
#' }
#'
#' @export

geo_merge <- function(files,
                      output = c("all",
                                 "change",
                                 "split",
                                 "merge",
                                 "complete")){

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

    ## Find all codes that have changed during
    ## from the oldest code list to the most recent codes
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

    ## Keep only codes that are valid in the most recent year because
    ## those that aren't indicate they have multiple changes
    recentCodes <- unique(files[[fileMx]]$data$code)
    currDT <- chgDT[code  %in% recentCodes, ]

    ## Merge all changes ie. multiple and change once
    changeDT <- rbindlist(list(joinDT, currDT))
    setkey(changeDT, code)

    ## Clean up duplicated raws if exist and
    ## delete codes that have not changed ie. column for 'prev' is NA
    dtClean <- clean_dup(x = changeDT, xCodes = recentCodes)

    ## Merge everything to recent geo list
    ## ---------------------------------------
    dupCodesChg <- unique(dtClean$CDT$code)## codes that are allready in the changes table
    ## keep only codes in recent geo list that aren't in the changes table
    otherDT <- files[[fileMx]]$data[!(code  %in% dupCodesChg), ]
    geoDT <- rbindlist(list(otherDT, dtClean$CDT), fill = TRUE)
    setkey(geoDT, code, year)

    ## Recode mulitple codes that haven't been converted to recent geo code
    eks <- setdiff(geoDT$code, recentCodes) #get codes that aren't in recent geo

    for (i in eks){
        currGeo <- geoDT[prev == i, ][["code"]]
        geoDT[code == i, code := currGeo]
    }

    setkey(geoDT, code, year)

    ## Add granularity column
    granTyp <- files[[1]]$type
    geoDT$granularity <- granTyp

    completeDT <- list(split = dtClean$dupDT, merge = dtClean$mrgDT, change = dtClean$CDT, all = geoDT)

    ## Default output
    if (length(output) > 1) output = "all"

    DTout <- switch(output,
                    "all" = geoDT,
                    "change" = dtClean$CDT,
                    "split" = dtClean$dupDT,
                    "merge" = dtClean$mrgDT,
                    "complete" = completeDT
                    )

    return(DTout[])

}



## Clean up duplicated raws if exist and
## delete codes that have not changed ie. column for 'prev' is NA
clean_dup <- function(xDT, xCodes){

    indX <- xDT[, .I[duplicated(xDT)]]

    if (length(indX) > 0L){
        dtx <- xDT[-(indX)]
        dtz <- dtx[!is.na(prev), ]

        ## - Check duplicate for find_change() function.
        ## - Cuplicate can be due to codes in the previous year are split
        ## into 2 or more codes in the current year
        ## - Keep only those that are in newest geo due to multiple changes
        dupInx <- dtz[, .I[(duplicated(prev) | duplicated(prev, fromLast = TRUE))]]

        ## split to 2 DT with and without duplicated 'prev'
        uniDT <- dtz[-dupInx]
        dupDT <- dtz[dupInx]

        ## Clean duplicated codes if codes aren't in newest geo
        dupCodes <- unique(dupDT$code)
        ## keep duplicated only if it's in the newest codelist
        ## other codes that aren't have multiple changes
        keepInd <- is.element(dupCodes, xCodes)
        keepCodes <- dupCodes[keepInd]
        dupUni <- dupDT[code %in% keepCodes, ]

        ## - Get the previous codes that are merged into one area
        ## in the current code
        mrgInx <- dtz[, .I[(duplicated(code) | duplicated(code, fromLast = TRUE))]]
        mrgDT <- dtz[mrgInx]

        ## Merge back to the other DT without duplicated previous codes
        CDT <- rbindlist(list(uniDT, dupUni))
        setkey(CDT, code)
    } else {
        CDT <- xDT[!is.na(prev)]
        dupDT <- 0
        mrgDT <- 0
    }

    list(CDT = CDT, dupDT = dupDT, mrgDT = mrgDT)
}
