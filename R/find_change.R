#' Find changes from previous year
#'
#' Which codes have changed since one of the previous years
#'
#' @inheritParams merge_multi
#' @export

find_change <- function(newCode, preCode, raw = TRUE){

  dt1 <- newCode[["data"]]

  if (raw){
    dt2 <- preCode[["data"]]
  } else {
    dt2 <- preCode
  }

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
