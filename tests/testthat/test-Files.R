context("Raw files")

test_that("File reference error", {

  expect_error(add_change(filegeo = "file1.csv",
                          filechg = c("file2.xlsx", "file3.xlsx"),
                          raw = TRUE,
                          year = 2019
                          ), "File not found", ignore.case = TRUE)
})


test_that("Year missing", {

  expect_error(add_change(filegeo = "file1.csv",
                          filechg = c("file2.xlsx", "file3.xlsx"),
                          raw = TRUE
                          ), "Year", ignore.case = TRUE)
})


test_that("Only one info needed", {
  expect_error(add_change(grep.file = "jan2018",
                          grep.change = "change",
                          folder = "~/Test",
                          year = 2018,
                          filege = "file2.csv"), "Only one of them need", ignore.case = TRUE)
})
