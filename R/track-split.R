#' Get geo code that are split after code change
#'
#' @inheritParams get_correspond
#' @return Dataset with column 'split' showing how many the codes have been split to
#' @export

track_split <- function(type = c(
                          "fylke",
                          "kommune",
                          "bydel",
                          "grunnkrets"
                        ),
                        from = NULL,
                        to = NULL) {
  type <- match.arg(type)
  dt <- track_change(type, from, to)
  data.table::setkey(dt, oldCode, changeOccurred)
  dt[!is.na(oldCode), split := .N, by = data.table::rleid(changeOccurred, oldCode)]
  out <- dt[split > 1]
}
