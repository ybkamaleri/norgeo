#' Track all changes for codes from API
#'
#' Track code changes from the downloaded data via API
#'
#' @inheritParams get_code
#'
#' @import data.table
#'
#' @export

track_change <- function(type = c(
  "fylke",
  "kommune",
  "bydel",
  "grunnkrets"
),
from = NULL,
to = NULL) {
  type <- match.arg(type)

  dataApi <<- new.env()
  dataApi$dt <- data_current(type, from, to)
  vecYear <- unique(dataApi$dc$changeOccurred)
  selYear <- sort(vecYear[vecYear != dataApi$yrMax], decreasing = TRUE)

  for (i in seq_along(selYear)) {
    DT <- data_merge(dataApi$dt, dataApi$dc, selYear[i])
    dataApi$dt <- DT
  }

  data.table::setkey(dataApi$dt, newCode, changeOccurred)
  ## When nothing changes
  dataApi$dt[oldCode == newCode, oldCode := NA]

  data.table::setnames(dataApi$dt, "newCode", "currentCode")
  return(dataApi$dt[])
}

## Do testing with:
## dat <- data_current()
## length(unique(dat$changeOccurred))==2
data_current <- function(type, from, to) {
  DT <- get_code(type, from = to)

  data_change(type, from, to)

  yrMax <- max(dataApi$dc$changeOccurred)
  dataApi$yrMax <- yrMax

  dt <- dataApi$dc[changeOccurred == yrMax][DT, on = c(newCode = "code")]

  ## when no code changes have occurred
  dt[is.na(oldCode), oldCode := newCode][
    is.na(changeOccurred), changeOccurred := validTo
  ][
    is.na(newName), newName := name
  ]

  dt[, c("name", "validTo") := NULL][]
}

## data1 : newest dataset with code changes
## data2 : older dataset with code changes
## year  : year for subset
data_merge <- function(data1, data2, year) {

  ## 1 to 1 merge
  dtc <- data1[data2[changeOccurred == year], on = c(oldCode = "newCode")]
  delCols <- c("oldCode", "oldName", "i.newName", "changeOccurred")
  dtc[, (delCols) := NULL]
  setnames(
    dtc,
    c("i.oldCode", "i.oldName", "i.changeOccurred"),
    c("oldCode", "oldName", "changeOccurred")
  )

  ## Code changes that keep the earlier code change eg. Trondheim in 2018 and
  ## with Klæbu joining in 2020
  codeMulti <- dtc[is.na(newCode)]$oldCode

  if (length(codeMulti) > 0) {
    dtp <- data1[data2[oldCode %in% codeMulti], on = "newCode"]
    delCols <- c("oldCode", "oldName", "newName", "changeOccurred")
    dtp[, (delCols) := NULL]
    setnames(
      dtp,
      c("i.oldCode", "i.changeOccurred", "i.oldName", "i.newName"),
      c("oldCode", "changeOccurred", "oldName", "newName")
    )

    dtcNA <- dtc[!is.na(newCode)]

    DTC <- rbindlist(list(dtcNA, dtp), use.names = TRUE)
  } else {
    DTC <- dtc
  }

  dd <- rbindlist(list(data1, DTC), use.names = TRUE)
}

## Avoid downloading changes data multiple times
data_change <- function(type, from, to) {
  dataApi$dc <- get_change(type, from, to, quiet = TRUE)
}
