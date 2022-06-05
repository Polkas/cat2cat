library(dplyr)
set.seed(1234)

data(occup)
data(trans)

occup_2006 <- occup[occup$year == 2006, ]
occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]

occup_back_2008_2010 <- cat2cat(
  data = list(old = occup_2008, new = occup_2010, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = c("knn", "rf", "lda"), features = c("age", "sex", "edu", "exp", "parttime", "salary"),
            args = list(k = 10, ntree = 50))
)

occup_2006[["g_new_c2c"]] <- occup_2006[["code"]]
# the counts could be any of wei_* or their combination
freq_df <- occup_back_2008_2010$old[, c("g_new_c2c", "wei_freq_c2c")] %>%
  cross_c2c() %>%
  group_by(g_new_c2c) %>%
  summarise(counts = round(sum(wei_cross_c2c)))

occup_back_2006_2008 <- cat2cat(
  data = list(old = occup_2006,
              new = occup_back_2008_2010$old,
              cat_var = "g_new_c2c",
              time_var = "year",
              freq_df = freq_df),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = c("knn", "rf", "lda"), features = c("age", "sex", "edu", "exp", "parttime", "salary"),
            args = list(k = 10, ntree = 50))
)

occup_2006_new <- occup_back_2006_2008$old
occup_2008_new <- occup_back_2008_2010$old # or occup_back_2006_2008$new
occup_2010_new <- occup_back_2008_2010$new
occup_2012_new <- dummy_c2c_cols(occup_2012,
                                 cat_var = "code",
                                 ml = c("wei_knn_c2c", "wei_rf_c2c", "wei_lda_c2c"))

final_data <- do.call(rbind, list(occup_2006_new,
                                  occup_2008_new,
                                  occup_2010_new,
                                  occup_2012_new))

# We persist the number of observations
counts_new <- final_data %>%
  cross_c2c() %>%
  group_by(year) %>%
  summarise(n = as.integer(round(sum(wei_freq_c2c))),
            n2 = as.integer(round(sum(wei_cross_c2c))),
            .groups = 'drop')

counts_old <- occup %>%
  group_by(year) %>%
  summarise(n = n(), n2 = n(), .groups = 'drop')

testthat::test_that("multi-period cat2cat", {
  expect_identical(counts_new, counts_old)
})
