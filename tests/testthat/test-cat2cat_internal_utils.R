testthat::test_that("get_freqs", {
  expected_freqs <- function(x) {
    tab <- table(x, useNA = "ifany")
    data.frame(
      input = names(tab),
      Freq = as.integer(tab),
      stringsAsFactors = FALSE
    )
  }

  input <- c("a", "a", "a", "c", "c", "b", "d", NA)
  expect_identical(
    get_freqs(input),
    expected_freqs(input)
  )
  set.seed(1234)
  input_multiplier <- sample(1:10, length(input))
  elem1 <- get_freqs(input, multiplier = input_multiplier)
  input <- rep(input, times = input_multiplier)
  elem2 <- expected_freqs(input)
  expect_identical(elem1, elem2)
})

mapping_table <- data.frame(
  old = c(1, 1, 1, 2, 2, 3, NA, NA),
  new = c(NA, 1, 2, 2, 3, 2, NA, 3),
  stringsAsFactors = FALSE
)

testthat::test_that("get_mappings", {
  mappings <- get_mappings(mapping_table)
  expect_identical(length(mappings$to_new), 4L)
  expect_identical(
    lengths(mappings$to_new),
    structure(c(3L, 2L, 1L, 2L), names = c("1", "2", "3", NA))
  )
  expect_identical(length(mappings$to_old), 4L)
  expect_identical(
    lengths(mappings$to_old),
    structure(c(2L, 1L, 3L, 2L), names = c(NA, "1", "2", "3"))
  )
})

testthat::test_that("cat_apply_freq", {
  mappings <- get_mappings(mapping_table)
  mappings_freq <- cat_apply_freq(
    mappings$to_new,
    get_freqs(c(1, 1, 1, 1, 2, 3, 3, NA, NA))
  )
  expect_identical(mappings_freq$`1`, c(2 / 7, 4 / 7, 1 / 7))
  expect_identical(length(mappings_freq), 4L)
  expect_equal(
    lengths(mappings_freq),
    structure(c(3L, 2L, 1L, 2L), names = c("1", "2", "3", NA))
  )
  mappings_freq2 <- cat_apply_freq(mappings$to_new, get_freqs(c(1, 1, 1, 1)))
  expect_identical(mappings_freq2$`1`, c(0, 1L, 0))
  expect_identical(mappings_freq2$`2`, c(0.5, 0.5))
})

testthat::test_that("resolve_frequencies falls back to get_freqs when weights are absent", {
  cat_base_year <- data.frame(
    code = c("A", "A", "B", "C"),
    mult = c(2, 1, 3, 1),
    stringsAsFactors = FALSE
  )

  out <- resolve_frequencies(
    cat_base_year = cat_base_year,
    cat_var_base = "code",
    freqs_df = NULL,
    multiplier_var = "mult"
  )

  expect_identical(out, get_freqs(cat_base_year$code, multiplier = cat_base_year$mult))
})

testthat::test_that("delayed_package_load throws custom error for missing namespace", {
  expect_error(
    delayed_package_load("definitely_not_installed_pkg_for_cat2cat", "custom missing package message"),
    "custom missing package message"
  )
})
