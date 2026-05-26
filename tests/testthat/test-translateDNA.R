test_that("translateDNA correctly translates all amino acids and stop codon and keeps translating even after a stop codon, and fails to translate overhangs", {
  df <- data.frame(name = "seq1",
                   sequence = "TTTCTGATTATGGTTTCTCCTACTGCTTATCATCAAAATAAAGATGAATGTTGGCGTTAAGGTA",
                   stringsAsFactors = FALSE)

  result <- translateDNA(df, offset = 0)

  expect_equal(result$protein[1], "FLIMVSPTAYHQNKDECWR*G")
})
test_that("translateDNA does the reading frame offset", {
  df <- data.frame(name = "seq1",
                   sequence = "AATGGTT",  # offset of 1 drops the leading A making ATGGTT which should translate to MV
                   stringsAsFactors = FALSE)

  result <- translateDNA(df, offset = 1)

  expect_equal(result$protein[1], "MV")

  result2 <- translateDNA(df, offset = 2)

  expect_equal(result2$protein[1],"W")
})
test_that("offset greater than 2 are rejected", {
  df <- data.frame(name = "seq1",
                   sequence = "ATGGTT",
                   stringsAsFactors = FALSE)

  expect_error(translateDNA(df, offset = 3), regexp = "Must be element of set ")
})
