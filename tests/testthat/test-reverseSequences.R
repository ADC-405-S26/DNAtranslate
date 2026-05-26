library(checkmate)
library(testthat)

test_that("reverseSequence reverses the specified row as expected", {
  df <- data.frame(name = c("seq1", "seq2"),
                   sequence = c("ATCG", "GGCC"),
                   stringsAsFactors = FALSE)

  result <- reverseSequence(df, "seq1")

  expect_equal(result$sequence[1], "GCTA")
  expect_equal(result$sequence[2], "GGCC")  # untouched
})
test_that("reverseSequence does not touch rows that were not specified", {
  df <- data.frame(name = c("seq1", "seq2", "seq3"),
                   sequence = c("ATCG", "GGCC", "TTAA"),
                   stringsAsFactors = FALSE)

  result <- reverseSequence(df, "seq2")

  expect_equal(result$sequence[1], "ATCG")
  expect_equal(result$sequence[3], "TTAA")
})
test_that("reverseSequence errors with an informative message when name is missing", {
  df <- data.frame(name = c("seq1"),
                   sequence = c("ATCG"),
                   stringsAsFactors = FALSE)

  expect_error(reverseSequence(df, "doesNotExist"),
               regexp = "Assertion on 'name %in% df\\$name' failed")
})










