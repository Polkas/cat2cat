set.seed(1234)

data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_old <- occup[occup$year == 2008, ]
occup_new <- occup[occup$year == 2010, ]

testthat::test_that("incorect input", {
  testthat::expect_error(cat2cat())
  testthat::expect_error(
    cat2cat(data = list(
      old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
    ))
  )
})

testthat::test_that("no mappings so empty dataset in one direction", {
  data <- list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  )
  mappings <- list(
    trans = data.frame(old = 1, new = 1)[NULL, ],
    direction = "backward"
  )

  testthat::expect_warning(
    cat2cat(data = data, mappings = mappings),
    "trans table does not cover some levels "
  )
  res <- suppressWarnings(cat2cat(data = data, mappings = mappings))
  testthat::expect_identical(nrow(res$old), 0L)
})

data_simple <- list(
  old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
)
mappings_simple_back <- list(trans = trans, direction = "backward")
mappings_simple_for <- list(trans = trans, direction = "forward")
ml <- list(
  data = occup_new,
  cat_var = "code",
  method = c("knn", "rf", "lda"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10, ntree = 30)
)

testthat::test_that("Simple backward cat2cat with 2 periods and no ml", {
  occup_1a <- cat2cat(data = data_simple, mappings = mappings_simple_back)
  expect_true(all(occup_1a$old$wei_freq_c2c <= 1 &
    occup_1a$old$wei_freq_c2c >= 0))
  expect_true(all(occup_1a$old$wei_naive_c2c <= 1 &
    occup_1a$old$wei_naive_c2c >= 0))
  expect_equal(sum(occup_1a$old$wei_naive_c2c), nrow(occup_old))
  expect_equal(sum(occup_1a$old$wei_freq_c2c), nrow(occup_old))
})


testthat::test_that("Simple backward cat2cat with 2 periods and ml", {
  occup_2 <- cat2cat(
    data = data_simple,
    mappings = mappings_simple_back,
    ml = ml
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

  expect_true(
    all(occup_2$old$wei_freq_c2c <= 1 & occup_2$old$wei_freq_c2c >= 0)
  )
  expect_true(
    all(occup_2$old$wei_freq_c2c <= 1 & occup_2$old$wei_freq_c2c >= 0)
  )
  expect_true(
    all(occup_2$old$wei_knn_c2c <= 1 & occup_2$old$wei_knn_c2c >= 0)
  )
  expect_true(
    all(occup_2$old$wei_rf_c2c <= 1 & occup_2$old$wei_rf_c2c >= 0)
  )
  expect_true(
    all(occup_2$old$wei_lda_c2c <= 1 & occup_2$old$wei_lda_c2c >= 0)
  )

  expect_equal(
    occup_2$old %>%
      cross_c2c(., c("wei_freq_c2c", "wei_knn_c2c"), c(1 / 2, 1 / 2)) %>%
      pull("wei_cross_c2c"),
    (occup_2$old$wei_knn_c2c + occup_2$old$wei_freq_c2c) / 2
  )
})

testthat::test_that(
  "Simple backward cat2cat with 2 periods and ml, not all mappings",
  {
    expect_warning(
      cat2cat(
        data = data_simple,
        mappings = list(trans = head(trans, -50), direction = "backward"),
        ml = ml
      ),
      "9321, 9312, 9311, 9331, 9313"
    )
  }
)

# handling NAs
occup_3a <- cat2cat(
  data = list(
    old = occup_old,
    new = occup_new,
    cat_var = "code",
    time_var = "year",
    multiplier_var = "multiplier"
  ),
  mappings = list(
    trans = trans,
    direction = "backward"
  ),
  ml = list(
    data = occup_new,
    cat_var = "code",
    method = c("knn"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

na_row <- occup_old[1, ]
na_row$code <- NA
na_row2 <- occup_new[1, ]
na_row2$code <- NA
occup_3b <- cat2cat(
  data = list(
    old = rbind(occup_old, na_row),
    new = rbind(occup_new, na_row2),
    cat_var = "code",
    time_var = "year",
    multiplier_var = "multiplier"
  ),
  mappings = list(
    trans = do.call(rbind, list(trans, c(NA, NA), c(NA, "432190"))),
    direction = "backward"
  ),
  ml = list(
    data = rbind(occup_new, na_row2),
    cat_var = "code",
    method = c("knn"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

expect_identical(nrow(occup_3b$old) - 2L, nrow(occup_3a$old))
expect_true(all(occup_3b$old$wei_freq_c2c <= 1 &
  occup_3b$old$wei_freq_c2c >= 0))
expect_true(all(occup_3b$old$wei_knn_c2c <= 1 & occup_3b$old$wei_knn_c2c >= 0))

na_row <- occup_old[1, ]
na_row$code <- "NA"
occup_3c <- cat2cat(
  data = list(
    old = rbind(occup_old, na_row),
    new = occup_new,
    cat_var = "code",
    time_var = "year",
    multiplier_var = "multiplier"
  ),
  mappings = list(trans = rbind(trans, c("NA", "NA")), direction = "backward"),
  ml = list(
    method = c("knn"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

expect_identical(nrow(occup_3b$old), nrow(occup_3c$old) + 1L)
expect_true(all(occup_3c$old$wei_freq_c2c <= 1 &
  occup_3c$old$wei_freq_c2c >= 0))
expect_true(all(occup_3c$old$wei_knn_c2c <= 1 & occup_3c$old$wei_knn_c2c >= 0))

# forward and not all mappings

testthat::test_that(
  "Simple forward cat2cat with 2 periods and no ml, not all mappings",
  {
    expect_warning(
      cat2cat(
        data = data_simple,
        mappings = mappings_simple_for
      ),
      paste(
        "741103, 712604, 732201, 818116, 732301,",
        "816003, 741201, 713201, 818115, 815204"
      )
    )
  }
)

testthat::test_that(
  "Simple forward cat2cat with 2 periods and no ml - details",
  {
    occup_4 <- suppressWarnings(cat2cat(
      data = data_simple,
      mappings = mappings_simple_for
    ))

    expect_equal(
      sum(occup_4$new$wei_freq_c2c) +
        sum(occup_new$code %in% setdiff(occup_new$code, trans$new)),
      nrow(occup_new)
    )
    expect_true(all(occup_4$new$wei_freq_c2c <= 1 &
      occup_4$new$wei_freq_c2c >= 0))

    occup_4b <- cat2cat(
      data = list(
        old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
      ),
      mappings = list(
        trans = rbind(
          trans,
          data.frame(old = "no_cat", new = setdiff(occup_new$code, trans$new))
        ),
        direction = "forward"
      )
    )

    expect_identical(nrow(occup_4b$old), nrow(occup_4b$old))
    expect_true(all(occup_4b$old$wei_freq_c2c <= 1 &
      occup_4b$old$wei_freq_c2c >= 0))
  }
)

# automatic mapping table
# the ean variable is an unique identifier
data("verticals2", package = "cat2cat")

vert_old <- verticals2[verticals2$v_date == "2020-04-01", ]
vert_new <- verticals2[verticals2$v_date == "2020-05-01", ]

## get mapping (transition) table
trans_v <- vert_old %>%
  inner_join(vert_new, by = "ean") %>%
  select(vertical.x, vertical.y) %>%
  distinct()

# as then we merging categories 1 to 1 for this identifiers

testthat::test_that(
  "cat2cat - set id_var",
  {
    verts <- cat2cat(
      data = list(
        old = vert_old, new = vert_new, id_var = "ean",
        cat_var = "vertical", time_var = "v_date"
      ),
      mappings = list(trans = trans_v, direction = "backward")
    )

    expect_true(all(verts$old$wei_freq_c2c <= 1 & verts$old$wei_freq_c2c >= 0))
    expect_true(all(verts$new$wei_freq_c2c <= 1 & verts$new$wei_freq_c2c >= 0))
    expect_equal(sum(verts$old$wei_naive_c2c), nrow(vert_old))
    expect_equal(sum(verts$old$wei_freq_c2c), nrow(vert_old))
  }
)

testthat::test_that(
  "set id_var, backward and ml",
  {
    verts2 <- cat2cat(
      data = list(
        old = vert_old, new = vert_new, id_var = "ean",
        cat_var = "vertical", time_var = "v_date"
      ),
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

    expect_true(
      (all(verts2$old$wei_knn_c2c <= 1 & verts2$old$wei_knn_c2c >= 0))
    )
    expect_true(
      (all(verts2$old$wei_rf_c2c <= 1 & verts2$old$wei_rf_c2c >= 0))
    )
    expect_true(
      (all(verts2$old$wei_lda_c2c <= 1 & verts2$old$wei_lda_c2c >= 0))
    )

    expect_true(all(unique(verts2[["old"]][["g_new_c2c"]]) %in% trans_v[[2]]))
  }
)

testthat::test_that(
  "set id_var, forward and ml",
  {
    verts3 <- cat2cat(
      data = list(
        old = vert_old, new = vert_new, id_var = "ean",
        cat_var = "vertical", time_var = "v_date"
      ),
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

    expect_true(
      (all(verts3$new$wei_knn_c2c <= 1 & verts3$new$wei_knn_c2c >= 0))
    )
    expect_true(
      (all(verts3$new$wei_rf_c2c <= 1 & verts3$new$wei_rf_c2c >= 0))
    )
    expect_true(
      (all(verts3$new$wei_lda_c2c <= 1 & verts3$new$wei_lda_c2c >= 0))
    )

    expect_true(all(unique(verts3[["new"]][["g_new_c2c"]]) %in% trans_v[[1]]))
  }
)
