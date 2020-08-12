#' Find changes from previous year
#'
#' Which codes have changed since one of the previous years
#'
#' @inheritParams merge_multi
#' @export

find_change <- function(newCode, preCode, raw = TRUE){

  dd1 <- newCode[["data"]]

  if (raw){
    dd2 <- preCode[["data"]]
  } else {
    dd2 <- preCode
  }

  ## Keep only columns that exist in both
  delColName <- c(setdiff(names(dd1), names(dd2)),
                  setdiff(names(dd2), names(dd1)))

  dt1 <- delete_col(dd1, delColName)
  dt2 <- delete_col(dd2, delColName)

  DT <- dt2[dt1, on = "code"]

  colN <- names(dt1)[-1]
  for (j in colN){
    coli <- paste0("i.", j)
    DT[is.na(get(coli)), (coli) := get(j)]
  }

  DT[, (colN) := NULL]
  setnames(DT, names(DT)[-1], colN)

  return(DT[])

}


## Keep only colums that exist in both datasets
delete_col <- function(x, col){

  colSel <- col[is.element(col, x)]

  if (length(colSel) > 0){
    x[, (colSel) := NULL]
  }

  return(x)
}
