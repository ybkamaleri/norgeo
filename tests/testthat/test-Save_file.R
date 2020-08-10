context("Save object")

test_that("Database name missing", {

  expect_error(geo_save(file.type = "Access"),
               "Database name", ignore.case = TRUE)

})
