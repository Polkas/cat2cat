library(dplyr)
set.seed(1234)

data(occup)
data(trans)

occup_old <- occup[occup$year == 2008, ]
occup_new <- occup[occup$year == 2010, ]

occup_1a <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

expect_true((all(occup_1a$old$wei_freq_c2c <= 1 & occup_1a$old$wei_freq_c2c >= 0)))
expect_true((all(occup_1a$old$wei_naive_c2c <= 1 & occup_1a$old$wei_naive_c2c >= 0)))
expect_equal(sum(occup_1a$old$wei_naive_c2c), nrow(occup_old))
expect_equal(sum(occup_1a$old$wei_freq_c2c), nrow(occup_old))

occup_2 <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_new,
    cat_var = "code",
    method = c("knn", "rf", "lda"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 30)
  )
)

expect_true(!identical(occup_2$old$wei_freq_c2c, occup_2$old$wei_rf_c2c))
expect_true(!identical(occup_2$old$wei_freq_c2c, occup_2$old$wei_knn_c2c))
expect_true(!identical(occup_2$old$wei_freq_c2c, occup_2$old$wei_lda_c2c))
expect_true(!identical(occup_2$old$wei_freq_c2c, occup_2$old$wei_naive_c2c))

expect_equal(sum(occup_2$old$wei_freq_c2c), nrow(occup_old))
expect_equal(sum(occup_2$old$wei_knn_c2c), nrow(occup_old))
expect_equal(sum(occup_2$old$wei_rf_c2c), nrow(occup_old))
expect_equal(sum(occup_2$old$wei_lda_c2c), nrow(occup_old))
expect_equal(sum(occup_2$old$wei_naive_c2c), nrow(occup_old))

expect_true((all(occup_2$old$wei_freq_c2c <= 1 & occup_2$old$wei_freq_c2c >= 0)))
expect_true((all(occup_2$old$wei_freq_c2c <= 1 & occup_2$old$wei_freq_c2c >= 0)))
expect_true((all(occup_2$old$wei_knn_c2c <= 1 & occup_2$old$wei_knn_c2c >= 0)))
expect_true((all(occup_2$old$wei_rf_c2c <= 1 & occup_2$old$wei_rf_c2c >= 0)))
expect_true((all(occup_2$old$wei_lda_c2c <= 1 & occup_2$old$wei_lda_c2c >= 0)))

expect_equal(
  occup_2$old %>% cross_c2c(., c("wei_freq_c2c", "wei_knn_c2c"), c(1 / 2, 1 / 2)) %>% pull("wei_cross_c2c"),
  (occup_2$old$wei_knn_c2c + occup_2$old$wei_freq_c2c) / 2
)

###############

lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_2$old, weights = multiplier * wei_freq_c2c)
ss_c2c <- summary_c2c(lms, df_old = nrow(occup_old) - length(lms$assign))
lms$df.residual <- nrow(occup_old) - length(lms$assign)
ss1 <- suppressWarnings(summary(lms))

lms2 <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old, weights = multiplier)
ss2 <- summary(lms2)

expect_true(sum((ss2$coefficients[, 2] - ss1$coefficients[, 2])**2) < 0.01)
expect_true(sum((ss2$coefficients[, 2] - ss_c2c$std.error_c)**2) < 0.01)

#################

occup_3 <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year", multiplier_var = "multiplier"),
  mappings = list(trans = trans, direction = "backward")
)

expect_equal(sum(occup_3$old$wei_freq_c2c), nrow(occup_old))
expect_true((all(occup_3$old$wei_freq_c2c <= 1 & occup_3$old$wei_freq_c2c >= 0)))
expect_false(identical(occup_3$old$wei_freq_c2c, occup_1a$old$wei_freq_c2c))
expect_identical(nrow(occup_old), occup_3$old %>% prune_c2c(method = "highest1") %>% nrow())

expect_equal(
  nrow(occup_old),
  occup_3$old %>% prune_c2c(method = "morethan", percent = 0.2) %>% pull("wei_freq_c2c") %>% sum()
)

############################

na_row <- occup_old[1, ]
na_row$code <- NA
na_row2 <- occup_new[1, ]
na_row2$code <- NA
occup_3b <- cat2cat(
  data = list(old = rbind(occup_old, na_row), new = rbind(occup_new, na_row2), cat_var = "code", time_var = "year", multiplier_var = "multiplier"),
  mappings = list(trans = do.call(rbind, list(trans, c(NA, NA), c(NA, "432190"))), direction = "backward"),
  ml = list(
    data = rbind(occup_new, na_row2),
    cat_var = "code",
    method = c("knn"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

expect_identical(nrow(occup_3b$old) - 2L, nrow(occup_3$old))
expect_true((all(occup_3b$old$wei_freq_c2c <= 1 & occup_3b$old$wei_freq_c2c >= 0)))
expect_true((all(occup_3b$old$wei_knn_c2c <= 1 & occup_3b$old$wei_knn_c2c >= 0)))

na_row <- occup_old[1, ]
na_row$code <- "NA"
occup_3c <- cat2cat(
  data = list(old = rbind(occup_old, na_row), new = occup_new, cat_var = "code", time_var = "year", multiplier_var = "multiplier"),
  mappings = list(trans = rbind(trans, c("NA", "NA")), direction = "backward"),
  ml = list(
    method = c("knn"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

expect_identical(nrow(occup_3b$old), nrow(occup_3c$old) + 1L)
expect_true((all(occup_3c$old$wei_freq_c2c <= 1 & occup_3c$old$wei_freq_c2c >= 0)))
expect_true((all(occup_3c$old$wei_knn_c2c <= 1 & occup_3c$old$wei_knn_c2c >= 0)))

occup_4 <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "forward")
)
# not in trans table
expect_equal(sum(occup_4$new$wei_freq_c2c) + sum(occup_new$code %in% setdiff(occup_new$code, trans$new)), nrow(occup_new))
expect_true((all(occup_4$new$wei_freq_c2c <= 1 & occup_4$new$wei_freq_c2c >= 0)))

##########################################
## the ean variable is an unique identifier
data(verticals2)

vert_old <- verticals2[verticals2$v_date == "2020-04-01", ]
vert_new <- verticals2[verticals2$v_date == "2020-05-01", ]

## get transitions table
trans_v <- vert_old %>%
  inner_join(vert_new, by = "ean") %>%
  select(vertical.x, vertical.y) %>%
  distinct()

## cat2cat
## it is important to set id_var as then we merging categories 1 to 1
## for this identifier which exists in both periods.
verts <- cat2cat(
  data = list(old = vert_old, new = vert_new, id_var = "ean", cat_var = "vertical", time_var = "v_date"),
  mappings = list(trans = trans_v, direction = "backward")
)

expect_true(all(verts$old$wei_freq_c2c <= 1 & verts$old$wei_freq_c2c >= 0))
expect_true(all(verts$new$wei_freq_c2c <= 1 & verts$new$wei_freq_c2c >= 0))
expect_equal(sum(verts$old$wei_naive_c2c), nrow(vert_old))
expect_equal(sum(verts$old$wei_freq_c2c), nrow(vert_old))


verts2 <- cat2cat(
  data = list(old = vert_old, new = vert_new, id_var = "ean", cat_var = "vertical", time_var = "v_date"),
  mappings = list(trans = trans_v, direction = "backward"),
  ml = list(
    data = vert_new,
    cat_var = "vertical",
    method = c("knn", "rf", "lda"),
    features = c("sales"),
    args = list(k = 10, ntree = 30)
  )
)

expect_true(!identical(verts2$old$wei_freq_c2c, verts2$old$wei_naive_c2c))
expect_true(!identical(verts2$old$wei_freq_c2c, verts2$old$wei_rf_c2c))
expect_true(!identical(verts2$old$wei_freq_c2c, verts2$old$wei_knn_c2c))
expect_true(!identical(verts2$old$wei_freq_c2c, verts2$old$wei_lda_c2c))

expect_equal(sum(verts2$old$wei_freq_c2c), nrow(vert_old))
expect_equal(sum(verts2$old$wei_knn_c2c), nrow(vert_old))
expect_equal(sum(verts2$old$wei_rf_c2c), nrow(vert_old))
expect_equal(sum(verts2$old$wei_lda_c2c), nrow(vert_old))

expect_true((all(verts2$old$wei_knn_c2c <= 1 & verts2$old$wei_knn_c2c >= 0)))
expect_true((all(verts2$old$wei_rf_c2c <= 1 & verts2$old$wei_rf_c2c >= 0)))
expect_true((all(verts2$old$wei_lda_c2c <= 1 & verts2$old$wei_lda_c2c >= 0)))

verts3 <- cat2cat(
  data = list(old = vert_old, new = vert_new, id_var = "ean", cat_var = "vertical", time_var = "v_date"),
  mappings = list(trans = trans_v, direction = "forward"),
  ml = list(
    data = vert_old,
    cat_var = "vertical",
    method = c("knn", "rf", "lda"),
    features = c("sales"),
    args = list(k = 10, ntree = 30)
  )
)

expect_true(!identical(verts3$new$wei_freq_c2c, verts3$new$wei_rf_c2c))
expect_true(!identical(verts3$new$wei_freq_c2c, verts3$new$wei_knn_c2c))
expect_true(!identical(verts3$new$wei_freq_c2c, verts3$new$wei_lda_c2c))

expect_equal(sum(verts3$new$wei_freq_c2c), nrow(vert_new))
expect_equal(sum(verts3$new$wei_knn_c2c), nrow(vert_new))
expect_equal(sum(verts3$new$wei_rf_c2c), nrow(vert_new))
expect_equal(sum(verts3$new$wei_lda_c2c), nrow(vert_new))

expect_true((all(verts3$new$wei_knn_c2c <= 1 & verts3$new$wei_knn_c2c >= 0)))
expect_true((all(verts3$new$wei_rf_c2c <= 1 & verts3$new$wei_rf_c2c >= 0)))
expect_true((all(verts3$new$wei_lda_c2c <= 1 & verts3$new$wei_lda_c2c >= 0)))
