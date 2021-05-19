## NB! for date, 'from' is INclusive and 'to' is EXlusive i.e the date specified in 'to' will be exluded
## Therefore, specify date as 2020-01-02 to include 01.jan.2020
## Ref: https://data.ssb.no/api/klass/v1/api-guide.html#_range

## Bydel til Fylke
codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/131/corresponds?targetClassificationId=103&from=2014-01-01&to=2015-01-02"

koGET <- httr::GET(codeUrl)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)

koDT <- koJS[["correspondenceItems"]]
data.table::setDT(koDT)
koDT

## Kommune (131) til Fylke (104)
codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/104/corresponds"
kodeQ <- list(targetClassificationId = 131, from = "2018-01-01", to = "2020-01-02")

koGET <- httr::GET(codeUrl, query = kodeQ)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)
koDT <- koJS[["correspondenceItems"]]
data.table::setDT(koDT)
koDT


## Grunnkrest (1) til Kommune (131)
## -------------------------
## corresponds
codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/131/corresponds"
kodeQ <- list(targetClassificationId = 1, from = "2018-01-02", to = "2020-01-02")

koGET <- httr::GET(codeUrl, query = kodeQ)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)
koDT <- koJS[["correspondenceItems"]]
data.table::setDT(koDT)
koDT
koDT[, .N, by = validTo]
koDT[, .N, by = validFrom]

## correspondsAt
codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/131/correspondsAt"
kodeQ <- list(targetClassificationId = 1, date = "2020-01-02")

koGET <- httr::GET(codeUrl, query = kodeQ)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)
## koJSF <- jsonlite::fromJSON(koTxt, flatten = TRUE)

koDT <- koJS[["correspondenceItems"]]
data.table::setDT(koDT)
koDT
dim(koDT)


## Grunnkrest (1) til Bydeler (103)
## -------------------------
## corresponds
codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/103/corresponds"
kodeQ <- list(targetClassificationId = 1, from = "2004-01-01", to = "2020-01-01")

koGET <- httr::GET(codeUrl, query = kodeQ)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)
koDT <- koJS[["correspondenceItems"]]
data.table::setDT(koDT)
koDT
koDT[, .N, by = validTo]


## correspondsAt
codeUrl <- "http://data.ssb.no/api/klass/v1/classifications/103/correspondsAt"
kodeQ <- list(targetClassificationId = 1, date = "2020-01-01")

koGET <- httr::GET(codeUrl, query = kodeQ)
koTxt <- httr::content(koGET, as = "text")
koJS <- jsonlite::fromJSON(koTxt)
## koJSF <- jsonlite::fromJSON(koTxt, flatten = TRUE)

koDT <- koJS[["correspondenceItems"]]
data.table::setDT(koDT)
koDT

## With klassR pkg
library(klassR)
head(GetKlass(103, date = "2020-01-01", correspond = 1))
head(GetKlass(131, date = "2020-01-01", correspond = 1))
CorrespondList(1)

## Flaten
library(jsonlite)
options(stringsAsFactors = FALSE)
x <- data.frame(driver = c("Bowser", "Peach"), occupation = c("Koopa", "Princess"))
x$vehicle <- data.frame(model = c("Piranha Prowler", "Royal Racer"))
x$vehicle$stats <- data.frame(speed = c(55, 34), weight = c(67, 24), drift = c(35, 32))
str(x)
str(flatten(x))
x1 <- flatten(x)
str(flatten(x, recursive = FALSE))

data1 <- fromJSON("https://api.github.com/users/hadley/repos")
colnames(data1)
colnames(data1$owner)
colnames(flatten(data1))
