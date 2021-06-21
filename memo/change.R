codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/131/changes?from=2000-01-01&to=2020-02-01"

koGET <- httr::GET(codeUrl)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)

koDT <- koJS[["codeChanges"]]
data.table::setDT(koDT)
koDT

dd <- get_change("kom", 2020, 2000)
dim(dd)
