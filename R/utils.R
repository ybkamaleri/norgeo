#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



## write data
## fname is like "~/dir/to/save/filename"
write_tbl <- function(x, fname, format) {
  if (format == "excel") {
    xlFile <- paste0(fname, ".xlsx")
    writexl::write_xlsx(x, path = xlFile)
  }

  if (format == "text") {
    xlFile <- paste0(fname, ".csv")
    data.table::fwrite(x, file = xlFile, sep = ";")
    ## utils::write.csv2(x, file = xlFile)
  }
}



## When file has complete path than no need for folder
file_folder <- function(file, folder = NULL) {
  if (is.null(folder)) {
    outFile <- file
  } else {
    outFile <- normalizePath(file.path(folder, file), winslash = "/")
  }
}

## From and To
check_range <- function(from, to) {
  from <- as.numeric(from)
  to <- as.numeric(to)

  trouble <- from > to
  if (trouble) {
    stop("From is higher than to. It should be the other way around!")
  }
}
