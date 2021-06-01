#' Get geo code that are merged after code change
#'
#' @inheritParams get_correspond
#' @return Dataset with column 'merge' showing how many the codes have been merged to
#' @export

track_merge <- function(type = c(
  "fylke",
  "kommune",
  "bydel",
  "grunnkrets"
),
from = NULL,
to = NULL) {
  type <- match.arg(type)
  dt <- track_change(type, from, to)
  data.table::setkey(dt, currentCode, changeOccurred)
  dt[!is.na(currentCode), merge := .N, by = data.table::rleid(changeOccurred, currentCode)]
  out <- dt[merge > 1]
}
