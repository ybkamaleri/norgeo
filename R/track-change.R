#' Track all changes for codes from API
#'
#' Track code changes from the downloaded data via API
#'
#' @inheritParams get_code
#' @return dataApi environment with main objects ie. dc (data change) and dt (current data)
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

  data_change(type, from, to)

  if (nrow(dataApi$dc) == 0) {
    stop("No code change has occurred for the selected time range")
  }

  ## Prepare the starting dataset with current year vs. last year
  data_current(type, from, to)

  vecYear <- unique(dataApi$dc$changeOccurred)
  selYear <- sort(vecYear[vecYear != dataApi$yrMax], decreasing = TRUE)

  for (i in seq_along(selYear)) {
    dataApi$dt <- data_merge(dataApi$dt, dataApi$dc, selYear[i])
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
## Else there isn't any changes occurred
data_current <- function(type, from, to) {
  if (is.null(to)) {
    to <- as.integer(format(Sys.Date(), "%Y"))
  }

  DT <- get_code(type = type, from = to)

  ## dataApi$dc is created when data_change is called in function track_change
  yrMax <- max(dataApi$dc$changeOccurred)
  dataApi$yrMax <- yrMax

  dt <- dataApi$dc[changeOccurred == yrMax][DT, on = c(newCode = "code")]

  ## when no code changes have occurred except names
  dt[is.na(oldCode), oldCode := newCode][
    is.na(changeOccurred), changeOccurred := validTo
  ][
    is.na(newName), newName := name
  ]

  dt[, c("name", "validTo") := NULL][]
  dataApi$dt <- dt
}

## data1 : newest dataset with code changes
## data2 : older dataset with code changes
## year  : year for subset
data_merge <- function(data1, data2, year) {

  ## 1 to 1 merge
  dtc <- data1[data2[changeOccurred == year], on = c(oldCode = "newCode")]
  delCols <- c("oldCode", "oldName", "i.newName", "changeOccurred")
  dtc[, (delCols) := NULL]
  data.table::setnames(
    dtc,
    c("i.oldCode", "i.oldName", "i.changeOccurred"),
    c("oldCode", "oldName", "changeOccurred")
  )

  ## Code changes that happen multiple time at different years while keeping the newest code
  ## eg. Trondheim in 2018 and Klæbu joining Trondheim in 2020
  codeMulti <- dtc[is.na(newCode)]$oldCode

  if (length(codeMulti) > 0) {
    dtp <- data1[data2[oldCode %in% codeMulti], on = "newCode"]
    delCols <- c("oldCode", "oldName", "newName", "changeOccurred")
    dtp[, (delCols) := NULL]
    data.table::setnames(
      dtp,
      c("i.oldCode", "i.changeOccurred", "i.oldName", "i.newName"),
      c("oldCode", "changeOccurred", "oldName", "newName")
    )

    dtcNA <- dtc[!is.na(newCode)]

    DTC <- data.table::rbindlist(list(dtcNA, dtp), use.names = TRUE)
  } else {
    DTC <- dtc
  }

  dd <- data.table::rbindlist(list(data1, DTC), use.names = TRUE)
}


## Avoid downloading changes data multiple times
data_change <- function(type, from, to) {
  dataApi$dc <- get_change(
    type = type,
    from = from,
    to = to,
    quiet = TRUE
  )
}
