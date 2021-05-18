## Make sure the root is at package
rm(list = ls())
getwd()
setwd("../")

library(pacman)
pkgs <- c(
  "usethis", "roxygen2", "devtools", "rmarkdown", "knitr", "pkgdown", "here", "fs",
  "data.table"
)
pacman::p_load(pkgs, character.only = TRUE)


## Looping
devtools::load_all()
devtools::document()

devtools::check()
devtools::test()


# Run to build the website
pkgdown::build_site()
pkgdown::build_news(preview = TRUE)
