% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track-change.R
\name{track_change}
\alias{track_change}
\title{Track all changes for codes from API}
\usage{
track_change(
  type = c("fylke", "kommune", "bydel", "grunnkrets"),
  from = NULL,
  to = NULL
)
}
\arguments{
\item{type}{Type of regional granularity ie. fylke, kommune etc.}

\item{from}{Specify the starting year for range period. Current year is the default.}

\item{to}{Specify the year to end the range period. Current year is used when not specified.}
}
\value{
dataApi environment with main objects ie. dc (data change) and dt (current data)
}
\description{
Track all code changes until current year or the year specified in \code{to} argument.
The column \code{oldCode} could have several codes if it has changed many times until
it becomes the code in \code{newCode}. When no code change has taken place, \code{NA} will
be used.
}
\examples{
\dontrun{
mydata <- track - change("kommune", from = 2017, to = 2020)
}

}
