library(dplyr)

data(occup)
data(trans)

occup_old <- occup[occup$year == 2008, ]
occup_new <- occup[occup$year == 2010, ]

occup_1a <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

expect_true(all(occup_1a$old$wei_freq_c2c <= 1 & occup_1a$old$wei_freq_c2c >= 0))
expect_true(all(occup_1a$new$wei_freq_c2c <= 1 & occup_1a$new$wei_freq_c2c >= 0))

expect_equal(sum(occup_1a$old$wei_naive_c2c), nrow(occup_old))
expect_equal(sum(occup_1a$old$wei_freq_c2c), nrow(occup_old))

occup_2 <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = c("knn", "rf", "lda"),
            features = c("age", "sex", "edu", "exp", "parttime", "salary"),
            args = list(k = 10, ntree = 30))
)

expect_equal(sum(occup_2$old$wei_freq_c2c), nrow(occup_old))
expect_equal(sum(occup_2$old$wei_knn_c2c), nrow(occup_old))
expect_equal(sum(occup_2$old$wei_rf_c2c), nrow(occup_old))
expect_equal(sum(occup_2$old$wei_lda_c2c), nrow(occup_old))

expect_true((all(occup_2$old$wei_knn_c2c <= 1 & occup_2$old$wei_knn_c2c >= 0)))
expect_true((all(occup_2$old$wei_rf_c2c <= 1 & occup_2$old$wei_rf_c2c >= 0)))
expect_true((all(occup_2$old$wei_lda_c2c <= 1 & occup_2$old$wei_lda_c2c >= 0)))

expect_equal(sum((occup_2$old$wei_knn_c2c + occup_2$old$wei_freq_c2c + occup_2$old$wei_naive_c2c) / 3), nrow(occup_old))

lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_2$old, weights = multipier * wei_freq_c2c)

ss_c2c <- summary_c2c(lms, df_old = nrow(occup_old), df_new = nrow(occup_2$old))

lms2 <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old, weights = multipier)

ss <- summary(lms2)

expect_true(sum((ss$coefficients[, 2] - ss_c2c$std.error_c)**2) < 0.01)

occup_3 <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year", multipier_var = "multipier"),
  mappings = list(trans = trans, direction = "backward")
)

expect_equal(sum(occup_3$old$wei_freq_c2c), nrow(occup_old))

expect_true((all(occup_3$old$wei_freq_c2c <= 1 & occup_3$old$wei_freq_c2c >= 0)))

expect_false(identical(occup_3$old$wei_freq_c2c, occup_1a$old$wei_freq_c2c))

occup_4 <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year",
    freqs_df = as.data.frame(table(occup_new$code))
  ),
  mappings = list(trans = trans, direction = "backward")
)

expect_true(identical(occup_4$old$wei_freq_c2c, occup_1a$old$wei_freq_c2c))

occup_5 <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year",
    freqs_df = as.data.frame(table(occup_new$code))
  ),
  mappings = list(trans = trans, direction = "backward")
)


## the ean variable is a unique identifier
data(verticals2)

vert_old <- verticals2[verticals2$v_date == "2020-04-01", ]
vert_new <- verticals2[verticals2$v_date == "2020-05-01", ]

## get transitions table
trans_v <- vert_old %>%
inner_join(vert_new, by = "ean") %>%
select(vertical.x, vertical.y) %>% distinct()

#
## cat2cat
## it is important to set id_var as then we merging categories 1 to 1
## for this identifier which exists in both periods.
verts = cat2cat(
  data = list(old = vert_old, new = vert_new, id_var = "ean", cat_var = "vertical", time_var = "v_date"),
  mappings = list(trans = trans_v, direction = "backward")
)

expect_true(all(verts$old$wei_freq_c2c <= 1 & verts$old$wei_freq_c2c >= 0))
expect_true(all(verts$new$wei_freq_c2c <= 1 & verts$new$wei_freq_c2c >= 0))

expect_equal(sum(verts$old$wei_naive_c2c), nrow(vert_old))
expect_equal(sum(verts$old$wei_freq_c2c), nrow(vert_old))


verts2 = cat2cat(
  data = list(old = vert_old, new = vert_new, id_var = "ean", cat_var = "vertical", time_var = "v_date"),
  mappings = list(trans = trans_v, direction = "backward"),
  ml = list(method = c("knn", "rf", "lda"),
          features = c("sales"),
          args = list(k = 10, ntree = 30))
)

expect_equal(sum(verts2$old$wei_freq_c2c), nrow(vert_old))
expect_equal(sum(verts2$old$wei_knn_c2c), nrow(vert_old))
expect_equal(sum(verts2$old$wei_rf_c2c), nrow(vert_old))
expect_equal(sum(verts2$old$wei_lda_c2c), nrow(vert_old))

expect_true((all(verts2$old$wei_knn_c2c <= 1 & verts2$old$wei_knn_c2c >= 0)))
expect_true((all(verts2$old$wei_rf_c2c <= 1 & verts2$old$wei_rf_c2c >= 0)))
expect_true((all(verts2$old$wei_lda_c2c <= 1 & verts2$old$wei_lda_c2c >= 0)))
