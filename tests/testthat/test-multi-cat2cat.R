library(dplyr)
set.seed(1234)

data(occup)
data(trans)

occup_2006 <- occup[occup$year == 2006, ]
occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]

ml_setup <- list(
  data = occup_2010,
  cat_var = "code",
  method = c("knn"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10, ntree = 50)
)

occup_back_2008_2010 <- cat2cat(
  data = list(
    old = occup_2008, new = occup_2010, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = ml_setup
)

# the counts could be any of wei_* or their combination
freq_df <- occup_back_2008_2010$old[, c("g_new_c2c", "wei_freq_c2c")] %>%
  cross_c2c() %>%
  group_by(g_new_c2c) %>%
  summarise(counts = round(sum(wei_cross_c2c, na.rm = TRUE)))

occup_back_2006_2008_1 <- cat2cat(
  data = list(
    old = occup_2006,
    new = occup_back_2008_2010$old,
    cat_var_old = "code",
    cat_var_new = "g_new_c2c",
    time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = ml_setup
)

testthat::test_that("multi-period cat2cat neutral for base period", {
  expect_identical(occup_back_2008_2010$old, occup_back_2006_2008_1$new)
})

testthat::test_that("multi-period cat2cat probabilities", {
  expect_true(
    !identical(
      occup_back_2006_2008_1$old$wei_freq_c2c,
      occup_back_2006_2008_1$old$wei_naive_c2c
    )
  )
  expect_true(
    !identical(
      occup_back_2006_2008_1$old$wei_freq_c2c,
      occup_back_2006_2008_1$old$wei_knn_c2c
    )
  )
  expect_true(!identical(
    occup_back_2006_2008_1$old$wei_naive_c2c,
    occup_back_2006_2008_1$old$wei_knn_c2c
  ))

  expect_equal(
    sum(occup_back_2006_2008_1$old$wei_freq_c2c),
    nrow(occup_2006)
  )
  expect_equal(
    sum(occup_back_2006_2008_1$old$wei_knn_c2c),
    nrow(occup_2006)
  )

  expect_true((all(occup_back_2006_2008_1$old$wei_freq_c2c <= 1 &
    occup_back_2006_2008_1$old$wei_freq_c2c >= 0)))
  expect_true((all(occup_back_2006_2008_1$old$wei_knn_c2c <= 1 &
    occup_back_2006_2008_1$old$wei_knn_c2c >= 0)))
})

occup_2006_new <- occup_back_2006_2008_1$old
occup_2008_new <- occup_back_2008_2010$old # or occup_back_2006_2008$new
occup_2010_new <- occup_back_2008_2010$new
occup_2012_new <- dummy_c2c(occup_2012,
  cat_var = "code",
  ml = c("knn")
)

final_data <- do.call(rbind, list(
  occup_2006_new,
  occup_2008_new,
  occup_2010_new,
  occup_2012_new
))

counts_new <- final_data %>%
  cross_c2c() %>%
  group_by(year) %>%
  summarise(
    n = as.integer(round(sum(wei_freq_c2c))),
    n2 = as.integer(round(sum(wei_cross_c2c))),
    .groups = "drop"
  )

counts_old <- occup %>%
  group_by(year) %>%
  summarise(n = n(), n2 = n(), .groups = "drop")

testthat::test_that("multi-period cat2cat persist the number of observations", {
  expect_identical(counts_new, counts_old)
})

data(occup)
data(trans)

occup_2006 <- occup[occup$year == 2006, ]
occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]

trans2 <- rbind(
  trans,
  data.frame(
    old = "no_cat",
    new = setdiff(c(occup_2010$code, occup_2012$code), trans$new)
  )
)

# 2008 -> 2010
occup_for_2008_2010 <- cat2cat(
  data = list(
    old = occup_2008, new = occup_2010, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans2, direction = "forward"),
  ml = ml_setup
)

# optional, give more control
# the counts could be any of wei_* or their combination
freq_df <- occup_for_2008_2010$new[, c("g_new_c2c", "wei_freq_c2c")] %>%
  group_by(g_new_c2c) %>%
  summarise(counts = round(sum(wei_freq_c2c)))

# 2010 -> 2012
occup_for_2010_2012 <- cat2cat(
  data = list(
    old = occup_for_2008_2010$new,
    new = occup_2012,
    cat_var_old = "g_new_c2c",
    cat_var_new = "code",
    time_var = "year",
    freqs_df = freq_df
  ),
  mappings = list(trans = trans2, direction = "forward"),
  ml = ml_setup
)

# 2010 -> 2012
occup_for_2010_2012_2 <- cat2cat(
  data = list(
    old = occup_for_2008_2010$new,
    new = occup_2012,
    cat_var_old = "g_new_c2c",
    cat_var_new = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans2, direction = "forward", freqs_df = freq_df),
  ml = ml_setup
)

expect_identical(occup_for_2010_2012_2, occup_for_2010_2012)

# use ml argument when applied ml models
occup_2006_new <- dummy_c2c(occup_2006, "code", ml = c("knn"))
occup_2008_new <- occup_for_2008_2010$old
occup_2010_new <- occup_for_2008_2010$new # or occup_for_2010_2012$old
occup_2012_new <- occup_for_2010_2012$new

final_data_for <- do.call(
  rbind,
  list(occup_2006_new, occup_2008_new, occup_2010_new, occup_2012_new)
)

# We persist the number of observations
counts_new <- final_data_for %>%
  cross_c2c() %>%
  group_by(year) %>%
  summarise(
    n = as.integer(round(sum(wei_freq_c2c))),
    n2 = as.integer(round(sum(wei_cross_c2c)))
  )

counts_old <- occup %>%
  group_by(year) %>%
  summarise(n = n(), n2 = n(), .groups = "drop")

testthat::test_that("", {
  testthat::expect_identical(counts_new, counts_old)
})
