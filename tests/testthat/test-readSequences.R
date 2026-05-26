library(testthat)
library(checkmate)

test_that("readSequences returns zero rows when no valid sequences are found", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c("this is NOT a properly formatted sequence name", "no sequences here"), tmp)

  result <- readSequences(tmp)

  expect_data_frame(result, nrows = 0)
})
test_that("readSequences returns zero rows when no valid sequences are found but a valid name is", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c(">this is a sequence name", "no sequences here"), tmp)

  result <- readSequences(tmp)

  expect_data_frame(result, nrows = 0)
})
test_that("readSequences returns a valid data frame with correct structure", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c(">seq1 test 1", "ATCG", ">seq2", "GGCC"), tmp)

  result <- readSequences(tmp)

  # correct number of rows
  expect_data_frame(result, nrows = 2, ncols = 2)
  # data didn't change magically
  expect_names(names(result), identical.to = c("name", "sequence"))
  # no entry should be missing from either column
  expect_character(result$name, any.missing = FALSE)
  expect_character(result$sequence, any.missing = FALSE)
})
test_that("readSequences collapses multi-line sequences into a single string", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(c(">seq1 test two", "ATCG", "TTAA", "GCGC"), tmp)

  result <- readSequences(tmp)

  expect_equal(nrow(result), 1)
  expect_string(result$sequence[1], pattern = "^ATCGTTAAGCGC$")
})




