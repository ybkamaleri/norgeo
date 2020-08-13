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

geo_save <- function(tblname = NULL,
                     obj = NULL,
                     des.path = FALSE,
                     file.type = c("Access", "SQLite", "Excel", "Text"),
                     db.name = NULL
                     ){

  innTyp <- tolower(file.type)

  if (innTyp  %in% c("access", "sqlite") & is.null(db.name)){
    stop("Database name is missing!", .call = TRUE)
  }


  ## DBMS
  get_dbms(db.name = db.name,
           db.path = des.path,
           tblname = tblname,
           obj = obj,
           dbms = innTyp)


  if (innTyp == "excel"){
    xlFile <- paste0(des.path, "/", tblname)
    write_tbl(obj, xlFile, innTyp)
  }

  if (innTyp == "text"){
    xlFile <- paste0(des.path, "/", tblname)
    write_tbl(obj, xlFile, innTyp)
  }


}



get_dbms <- function(db.name = NULL,
                     db.path = NULL,
                     tblname = NULL,
                     obj = NULL,
                     dbms = c("access", "sqlite")){

  dbFile <- paste(db.path, db.name, sep = "/")

  if (dbms == "access"){
    dbCon <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
    cs <- paste0(dbCon, dbFile)
    con <- DBI::dbConnect(odbc::odbc(), .connection_string = cs)
  }

  if (dbms == "sqlite"){
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbFile)
  }

  DBI::dbWriteTable(con, tblname, obj, batch_rows = 1, overwrite = TRUE)
  DBI::dbDisconnect(con)
}
