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



# write data
## fname is like "~/dir/to/save/filename"
write_tbl <- function(fname, format){

    if (format == "xls"){
          xlFile <- paste0(fname, ".xlsx")
          writexl::write_xlsx(obj, path = xlFile)
    }

    if (format == "csv"){
         xlFile <- paste0(fname, ".csv")
         data.table::fwrite(obj, file = xlFile, sep = ";")
    ## utils::write.csv2(obj, file = xlFile)
    }

}
