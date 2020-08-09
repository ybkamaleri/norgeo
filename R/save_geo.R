#' Save geo codes
#'
#' Geo codes can be saved either in a database management system (DBMS) or
#' as an Excel or text file.
#'
#' @param tblname Name of the table to be saved as
#' @param obj Object name to be saved
#' @param des.path Destination folder where the file to be saved
#' @param file.type Choose file type as Access, SQLite, Excel or Text
#' @param db.name When choosing a DBMS then specify the database name
#'
#' @export

save_geo <- function(tblname = NULL,
                     obj = NULL,
                     des.path = FALSE,
                     file.type = c("Access", "SQLite", "Excel", "Text"),
                     db.name = NULL
                     ){

  innTyp <- tolower(file.type)

  if (innTyp  %in% c("access", "sqlite") & is.null(db.name)){
    stop("Database name is missing!", .call = TRUE)
  }


  if (innTyp == "access"){

    get_access(db.name = db.name,
               db.path = des.path,
               tblname = tblname,
               obj = obj)
  }


  if (innTyp == "sqlite"){

    get_sqlite(db.name = db.name,
               db.path = des.path,
               tblname = tblname,
               obj = obj)

  }

  if (innTyp == "excel"){
    xlFile <- paste0(des.path, "/", tblname, ".xlsx")
    writexl::write_xlsx(obj, path = xlFile)
  }

  if (innTyp == "text"){
    xlFile <- paste0(des.path, "/", tblname, ".csv")
    data.table::fwrite(obj, file = xlFile, sep = ";")
    ## utils::write.csv2(obj, file = xlFile)
  }


}


## Connect to ACCESS
get_access <- function(db.name = NULL, db.path = NULL, tblname = NULL, obj = NULL){

    dbCon <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
    dbFile <- paste(db.path, db.name, sep = "/")
    cs <- paste0(dbCon, dbFile)
    con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
    DBI::dbWriteTable(con, tblname, obj, batch_rows = 1, overwrite = TRUE)
    DBI::dbDisconnect(con)

}


## Connect to SQLite
get_sqlite <- function(db.name = NULL, db.path = NULL, tblname = NULL, obj = NULL){

    dbFile <- paste(db.path, db.name, sep = "/")
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbFile)
    DBI::dbWriteTable(con, tblname, obj, batch_rows = 1, overwrite = TRUE)
    DBI::dbDisconnect(con)

}
