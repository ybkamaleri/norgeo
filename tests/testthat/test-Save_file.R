context("Save object")

test_that("Database name missing", {

  expect_error(save_geo(file.type = "Access"),
               "Database name", ignore.case = TRUE)

})
