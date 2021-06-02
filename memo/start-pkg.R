## Make sure the root is at package
rm(list = ls())
getwd()
setwd("../")

library(pacman)
pkgs <- c(
  "usethis", "roxygen2", "devtools", "rmarkdown", "knitr", "pkgdown", "here", "fs",
  "data.table", "readxl", "openxlsx"
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

# Use CI
usethis::use_git_remote("origin", url = "https://github.com/helseprofil/norgeo.git", overwrite = TRUE)
usethis::use_github_action_check_standard()
usethis::use_git_remote("origin", url = "git@work:helseprofil/norgeo.git", overwrite = TRUE)
