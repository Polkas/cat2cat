testthat::test_that("get_freqs", {
  input <- c("a", "a", "a", "c", "c", "b", "d", NA)
  expect_identical(get_freqs(input), as.data.frame(table(input, useNA = "ifany"), stringsAsFactors = FALSE))
  set.seed(1234)
  input_multiplier <- sample(1:10, length(input))
  elem1 <- get_freqs(input, multiplier = input_multiplier)
  input <- rep(input, times = input_multiplier)
  elem2 <- as.data.frame(table(input, useNA = "ifany"), stringsAsFactors = FALSE)
  expect_identical(elem1, elem2)
})

transition_table <- data.frame(old = c(1, 1, 1, 2 , 2, 3, NA, NA), new = c(NA, 1, 2, 2, 3, 2, NA, 3), stringsAsFactors = FALSE)

testthat::test_that("get_mappings", {
  mappings <- get_mappings(transition_table)
  expect_identical(length(mappings$to_new), 4L)
  expect_identical(length(mappings$to_old), 4L)
})

testthat::test_that("cat_apply_freq", {
  mappings <- get_mappings(transition_table)
  mappings_freq <- cat_apply_freq(mappings$to_new, get_freqs(c(1, 1, 1, 1, 2, 3, 3, NA, NA)))
  expect_identical(length(mappings_freq), 4L)
  expect_equal(as.numeric(lengths(mappings_freq)), c(3, 2, 1, 2))
})



