agg_old <- data.frame(
  vertical = c(
    "Electronics", "Kids1", "Kids2", "Automotive", "Books",
    "Clothes", "Home", "Fashion", "Health", "Sport"
  ),
  sales = rnorm(10, 100, 10),
  counts = rgeom(10, 0.0001),
  v_date = rep("2020-04-01", 10), stringsAsFactors = FALSE
)

agg_new <- data.frame(
  vertical = c(
    "Electronics", "Supermarket", "Kids", "Automotive1",
    "Automotive2", "Books", "Clothes", "Home", "Fashion", "Health", "Sport"
  ),
  sales = rnorm(11, 100, 10),
  counts = rgeom(11, 0.0001),
  v_date = rep("2020-05-01", 11), stringsAsFactors = FALSE
)

testthat::test_that("wrong input", {
  testthat::expect_error(cat2cat_agg(list()))
  testthat::expect_error(cat2cat_agg(list(
    old = agg_old,
    new = agg_new,
    cat_var = "WRONG",
    time_var = "v_date",
    freq_var = "counts"
  )))
  testthat::expect_error(cat2cat_agg(list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "WRONG",
    freq_var = "counts"
  )))
  testthat::expect_error(cat2cat_agg(list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "v_date",
    freq_var = "WRONG"
  )))
})

testthat::test_that("error when - wrong mapping", {
  testthat::expect_error(cat2cat_agg(list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "v_date",
    freq_var = "counts"
  ), ff %>% ee()))
})

testthat::test_that("no mapping is neutral for data", {
  agg_neu <- cat2cat_agg(list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "v_date",
    freq_var = "counts"
  ))
  testthat::expect_identical(agg_neu$old, cbind(agg_old, prop_c2c = 1))
  testthat::expect_identical(agg_neu$new, cbind(agg_new, prop_c2c = 1))
})

agg <- cat2cat_agg(
  data = list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "v_date",
    freq_var = "counts"
  ),
  Automotive %<% c(Automotive1, Automotive2),
  c(Kids1, Kids2) %>% c(Kids),
  Home %>% c(Home, Supermarket)
)

testthat::test_that("cat2cat_agg scenario one", {
  expect_true(sum(agg$old$prop_c2c) == nrow(agg_old))
  expect_true(sum(agg$new$prop_c2c) == nrow(agg_new))
  expect_true(all(agg$new$prop_c2c >= 0 & agg$new$prop_c2c <= 1))
  expect_true(all(agg$old$prop_c2c >= 0 & agg$old$prop_c2c <= 1))
})

agg2 <- cat2cat_agg(
  data = list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "v_date",
    freq_var = "counts"
  ),
  Automotive < c(Automotive1, Automotive2),
  c(Kids1, Kids2) > c(Kids),
  Home > c(Home, Supermarket)
)

testthat::test_that("cat2cat_agg scenario two", {
  expect_true(sum(agg2$old$prop_c2c) == nrow(agg_old))
  expect_true(sum(agg2$new$prop_c2c) == nrow(agg_new))
  expect_true(all(agg2$new$prop_c2c >= 0 & agg2$new$prop_c2c <= 1))
  expect_true(all(agg2$old$prop_c2c >= 0 & agg2$old$prop_c2c <= 1))
})

cols <- colnames(agg_new)
cols[1] <- "vertical2"
colnames(agg_new) <- cols
agg3 <- cat2cat_agg(
  data = list(
    old = agg_old,
    new = agg_new,
    cat_var_old = "vertical",
    cat_var_new = "vertical2",
    time_var = "v_date",
    freq_var = "counts"
  ),
  Automotive < c(Automotive1, Automotive2),
  c(Kids1, Kids2) > c(Kids),
  Home > c(Home, Supermarket)
)

testthat::test_that("cat2cat_agg scenario three", {
  cols <- colnames(agg3$new)
  cols[1] <- "vertical"
  colnames(agg3$new) <- cols
  expect_identical(agg2, agg3)
})
