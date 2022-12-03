additional_cols <- c(
  "index_c2c", "g_new_c2c", "wei_freq_c2c",
  "rep_c2c", "wei_naive_c2c"
)

testthat::test_that("dummy_c2c", {
  expect_identical(
    c(colnames(airquality), additional_cols),
    colnames(dummy_c2c(airquality, "Month"))
  )
})

testthat::test_that("dummy_c2c", {
  expect_identical(
    c(colnames(airquality), additional_cols, "wei_knn_c2c"),
    colnames(dummy_c2c(airquality, "Month", ml = "knn"))
  )
})

testthat::test_that("dummy_c2c backward", {
  expect_identical(
    c(colnames(airquality), additional_cols, "wei_knn_c2c"),
    colnames(dummy_c2c(airquality, "Month", ml = "wei_knn_c2c"))
  )
})

###

# cat2cat utils
library("dplyr")
set.seed(1234)

data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_old <- occup[occup$year == 2008, ]
occup_new <- occup[occup$year == 2010, ]

c2c <- cat2cat(
  data = list(
    old = occup_old,
    new = occup_new,
    cat_var = "code",
    time_var = "year",
    multiplier_var = "multiplier"
  ),
  mappings = list(trans = trans, direction = "backward")
)

expect_equal(sum(c2c$old$wei_freq_c2c), nrow(occup_old))
expect_true(all(c2c$old$wei_freq_c2c <= 1 & c2c$old$wei_freq_c2c >= 0))
expect_identical(
  nrow(occup_old),
  c2c$old %>%
    prune_c2c(method = "highest1") %>%
    nrow()
)

expect_true(
  (c2c$old %>%
    prune_c2c(method = "highest") %>%
    nrow()) >=
    (c2c$old %>%
      prune_c2c(method = "highest1") %>%
      nrow())
)

expect_equal(
  nrow(occup_old),
  c2c$old %>%
    prune_c2c(method = "morethan", percent = 0.2) %>%
    pull("wei_freq_c2c") %>%
    sum()
)
