data("occup_small", package = "cat2cat")

occup_old <- occup_small[occup_small$year == 2008, ]
occup_new <- occup_small[occup_small$year == 2010, ]

occup_2 <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward")
)

testthat::test_that("plot_c2c wrong input", {
  expect_error(plot_c2c(occup_2$new, type = c("both")))
  expect_error(plot_c2c(occup_2$old, type = c("wrong")))
  expect_error(plot_c2c(NULL, type = c("both")))
})

testthat::test_that("plot_c2c correct input", {
  expect_silent(plot_c2c(occup_2$old, type = c("both")))
  expect_silent(plot_c2c(occup_2$old, type = c("hist")))
  expect_silent(plot_c2c(occup_2$old, type = c("bar")))
})
