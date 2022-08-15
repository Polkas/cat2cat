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

transition_table <- data.frame(old = c(1, 1, 1, 2, 2, 3, NA, NA), new = c(NA, 1, 2, 2, 3, 2, NA, 3), stringsAsFactors = FALSE)

testthat::test_that("get_mappings", {
  mappings <- get_mappings(transition_table)
  expect_identical(length(mappings$to_new), 4L)
  expect_identical(lengths(mappings$to_new), structure(c(3L, 2L, 1L, 2L), names = c("1", "2", "3", NA)))
  expect_identical(length(mappings$to_old), 4L)
  expect_identical(lengths(mappings$to_old), structure(c(2L, 1L, 3L, 2L), names = c(NA, "1", "2", "3")))
})

testthat::test_that("cat_apply_freq", {
  mappings <- get_mappings(transition_table)
  mappings_freq <- cat_apply_freq(mappings$to_new, get_freqs(c(1, 1, 1, 1, 2, 3, 3, NA, NA)))
  expect_identical(mappings_freq$`1`, c(2/7, 4/7, 1/7))
  expect_identical(length(mappings_freq), 4L)
  expect_equal(lengths(mappings_freq), structure(c(3L, 2L, 1L, 2L), names = c("1", "2", "3", NA)))
  mappings_freq2 <- cat_apply_freq(mappings$to_new, get_freqs(c(1, 1, 1, 1)))
  expect_identical(mappings_freq2$`1`, c(0, 1L, 0))
  expect_identical(mappings_freq2$`2`, c(0.5, 0.5))
})

testthat::test_that("dummy_c2c", {
  expect_identical(
    c(colnames(airquality), "index_c2c", "g_new_c2c", "wei_freq_c2c", "rep_c2c", "wei_naive_c2c"),
    colnames(dummy_c2c(airquality, "Month"))
  )
})

testthat::test_that("dummy_c2c", {
  expect_identical(
    c(colnames(airquality), "index_c2c", "g_new_c2c", "wei_freq_c2c", "rep_c2c", "wei_naive_c2c", "wei_knn_c2c"),
    colnames(dummy_c2c(airquality, "Month", ml = "knn"))
  )
})

testthat::test_that("dummy_c2c backward", {
  expect_identical(
    c(colnames(airquality), "index_c2c", "g_new_c2c", "wei_freq_c2c", "rep_c2c", "wei_naive_c2c", "wei_knn_c2c"),
    colnames(dummy_c2c(airquality, "Month", ml = "wei_knn_c2c"))
  )
})
