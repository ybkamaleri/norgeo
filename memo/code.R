codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/131/codesAt?date=2020-01-01"

koGET <- httr::GET(codeUrl)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)

koDT <- koJS[["codes"]]
data.table::setDT(koDT)
koDT


## With query
codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/131/codesAt"
kodeQ <- list(date = "2020-01-01")

koGET <- httr::GET(codeUrl, query = kodeQ)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)

koDT <- koJS[["codes"]]
data.table::setDT(koDT)
koDT

## Codes with from and to arguments
codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/131/codes?from=2014-01-01&to=2015-02-01&csvSeparator=;"

koGET <- httr::GET(codeUrl)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)

koDT <- koJS[["codes"]]
data.table::setDT(koDT)
koDT
