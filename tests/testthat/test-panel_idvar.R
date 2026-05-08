# Tests for id_var functionality with occup_panel dataset
# and other features demonstrated in vignettes

data("occup_panel", package = "cat2cat")
data("trans", package = "cat2cat")

# Split by encoding periods (2009 = old encoding, 2010 = new encoding)
panel_old <- occup_panel[occup_panel$year == 2009, ]
panel_new <- occup_panel[occup_panel$year == 2010, ]

# For quarterly granularity tests
panel_2009Q4 <- occup_panel[occup_panel$quarter == "2009Q4", ]
panel_2010Q1 <- occup_panel[occup_panel$quarter == "2010Q1", ]

# Subjects in both encoding periods
shared_ids <- intersect(panel_old$panel_id, panel_new$panel_id)

# -----------------------------------------------------------------------------
# id_var basic functionality tests
# -----------------------------------------------------------------------------

testthat::test_that("id_var direct matching reduces replication", {
  # Without id_var
  result_no_id <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  # With id_var
  result_with_id <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      id_var = "panel_id",
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  # With id_var should have fewer rows due to direct matching
  expect_lt(nrow(result_with_id$old), nrow(result_no_id$old))

  # But weighted sums should equal original counts
  expect_equal(sum(result_no_id$old$wei_freq_c2c), nrow(panel_2009Q4))
  expect_equal(sum(result_with_id$old$wei_freq_c2c), nrow(panel_2009Q4))
})

testthat::test_that("id_var direct matches have rep_c2c = 1", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      id_var = "panel_id",
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  # Get direct matched subjects
  crossing_ids <- intersect(panel_2009Q4$panel_id, panel_2010Q1$panel_id)
  direct_matches <- result$old[result$old$panel_id %in% crossing_ids, ]

  # Direct matches should have rep_c2c = 1 (no replication)
  expect_true(all(direct_matches$rep_c2c == 1))

  # Direct matches should have wei_freq_c2c = 1
  expect_true(all(direct_matches$wei_freq_c2c == 1))
})

testthat::test_that("id_var non-matched subjects still get replicated", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      id_var = "panel_id",
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  crossing_ids <- intersect(panel_2009Q4$panel_id, panel_2010Q1$panel_id)
  non_matched <- result$old[!result$old$panel_id %in% crossing_ids, ]

  # Non-matched subjects should have some with rep_c2c > 1
  expect_true(any(non_matched$rep_c2c > 1))
})

testthat::test_that("id_var g_new_c2c comes from base period for direct matches", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      id_var = "panel_id",
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  crossing_ids <- intersect(panel_2009Q4$panel_id, panel_2010Q1$panel_id)

  # For each direct match, g_new_c2c should equal their 2010Q1 code
  for (pid in head(crossing_ids, 10)) {
    matched_row <- result$old[result$old$panel_id == pid, ]
    new_code <- panel_2010Q1$code[panel_2010Q1$panel_id == pid]
    expect_equal(matched_row$g_new_c2c, new_code)
  }
})

testthat::test_that("id_var complete matched panel uses only direct matches", {
  complete_ids <- intersect(panel_2009Q4$panel_id, panel_2010Q1$panel_id)
  complete_old <- panel_2009Q4[panel_2009Q4$panel_id %in% complete_ids, ]
  complete_new <- panel_2010Q1[panel_2010Q1$panel_id %in% complete_ids, ]

  result <- cat2cat(
    data = list(
      old = complete_old,
      new = complete_new,
      id_var = "panel_id",
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  # No observations should go through the replication path when every id matches.
  expect_equal(nrow(result$old), nrow(complete_old))
  expect_equal(nrow(result$new), nrow(complete_new))
  expect_true(all(result$old$rep_c2c == 1))
  expect_true(all(result$new$rep_c2c == 1))
  expect_true(all(result$old$wei_freq_c2c == 1))
  expect_true(all(result$old$wei_naive_c2c == 1))

  new_codes <- complete_new$code[match(result$old$panel_id, complete_new$panel_id)]
  expect_equal(result$old$g_new_c2c, new_codes)
})

# -----------------------------------------------------------------------------
# Weight sum preservation tests
# -----------------------------------------------------------------------------

testthat::test_that("weights sum to original n for all weight types", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      id_var = "panel_id",
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  n_original <- nrow(panel_2009Q4)
  expect_equal(sum(result$old$wei_freq_c2c), n_original)
  expect_equal(sum(result$old$wei_naive_c2c), n_original)
})

testthat::test_that("per-subject weights sum to 1", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  # Group by index_c2c (original observation) and sum weights
  per_subject_freq <- tapply(result$old$wei_freq_c2c,
                              result$old$index_c2c, sum)
  per_subject_naive <- tapply(result$old$wei_naive_c2c,
                               result$old$index_c2c, sum)

  # All should sum to 1 (within floating point tolerance)
  expect_true(all(abs(per_subject_freq - 1) < 1e-6))
  expect_true(all(abs(per_subject_naive - 1) < 1e-6))
})

# -----------------------------------------------------------------------------
# prune_c2c tests
# -----------------------------------------------------------------------------

testthat::test_that("prune_c2c highest1 returns one row per original obs", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  pruned <- prune_c2c(result$old, method = "highest1")
  expect_equal(nrow(pruned), nrow(panel_2009Q4))
})

testthat::test_that("prune_c2c nonzero removes zero-weight rows", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  pruned <- prune_c2c(result$old, method = "nonzero")

  # Should have no zero weights
  expect_true(all(pruned$wei_freq_c2c > 0))

  # Fewer or equal rows

  expect_lte(nrow(pruned), nrow(result$old))
})

testthat::test_that("prune_c2c preserves weight sum after renormalization", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  pruned <- prune_c2c(result$old, method = "highest")

  # Weight sum should still equal original n
  expect_equal(sum(pruned$wei_freq_c2c), nrow(panel_2009Q4))
})

# -----------------------------------------------------------------------------
# cross_c2c ensemble tests
# -----------------------------------------------------------------------------

testthat::test_that("cross_c2c creates ensemble weight column", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  crossed <- cross_c2c(result$old,
                        c("wei_freq_c2c", "wei_naive_c2c"),
                        c(0.5, 0.5))

  expect_true("wei_cross_c2c" %in% names(crossed))
})

testthat::test_that("cross_c2c weights sum correctly", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  crossed <- cross_c2c(result$old,
                        c("wei_freq_c2c", "wei_naive_c2c"),
                        c(0.7, 0.3))

  # Ensemble weights should sum to original n
  expect_equal(sum(crossed$wei_cross_c2c), nrow(panel_2009Q4))
})

testthat::test_that("cross_c2c weights are between 0 and 1", {
  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward")
  )

  crossed <- cross_c2c(result$old,
                        c("wei_freq_c2c", "wei_naive_c2c"),
                        c(0.5, 0.5))

  expect_true(all(crossed$wei_cross_c2c >= 0))
  expect_true(all(crossed$wei_cross_c2c <= 1))
})

# -----------------------------------------------------------------------------
# Quarterly panel structure validation
# -----------------------------------------------------------------------------

testthat::test_that("occup_panel has expected BAEL-style structure", {
  # 8 quarters
  expect_equal(length(unique(occup_panel$quarter)), 8)

  # Subjects observed across encoding change
  crossing <- intersect(
    occup_panel$panel_id[occup_panel$quarter == "2009Q4"],
    occup_panel$panel_id[occup_panel$quarter == "2010Q1"]
  )
  expect_gt(length(crossing), 400)  # Should be ~450

  # Most subjects observed 4 quarters
  appearances <- table(table(occup_panel$panel_id))
  expect_true("4" %in% names(appearances))
  expect_gt(appearances["4"], 500)  # Should be ~750
})

testthat::test_that("occup_panel codes have correct length per encoding", {
  old_codes <- occup_panel$code[occup_panel$year == 2009]
  new_codes <- occup_panel$code[occup_panel$year == 2010]

  # Old encoding: 4-digit
  expect_true(all(nchar(old_codes) == 4))

  # New encoding: 6-digit
  expect_true(all(nchar(new_codes) == 6))
})

testthat::test_that("boundary transitions are valid per trans table", {
  p2009Q4 <- occup_panel[occup_panel$quarter == "2009Q4",
                          c("panel_id", "code")]
  p2010Q1 <- occup_panel[occup_panel$quarter == "2010Q1",
                          c("panel_id", "code")]

  # Merge to get transitions
  crossing <- merge(p2009Q4, p2010Q1, by = "panel_id",
                     suffixes = c("_old", "_new"))

  # Check each transition is valid per trans table
  valid <- mapply(function(o, n) n %in% trans$new[trans$old == o],
                  crossing$code_old, crossing$code_new)

  # All transitions should be valid
  expect_true(all(valid))
})

# -----------------------------------------------------------------------------
# Forward direction with id_var
# -----------------------------------------------------------------------------

testthat::test_that("id_var works with forward direction", {
  # Some 2010 codes don't have backward mapping (genuinely new occupations)
  # So we expect a warning about missing coverage
  result <- suppressWarnings(cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      id_var = "panel_id",
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "forward")
  ))

  # Forward maps new to old, so $new is replicated
  # Some observations may be dropped due to missing mapping coverage
  expect_lte(sum(result$new$wei_freq_c2c), nrow(panel_2010Q1))
  expect_gt(sum(result$new$wei_freq_c2c), nrow(panel_2010Q1) - 10)

  # Direct matches should have rep_c2c = 1
  crossing_ids <- intersect(panel_2009Q4$panel_id, panel_2010Q1$panel_id)
  direct_matches <- result$new[result$new$panel_id %in% crossing_ids, ]
  expect_true(all(direct_matches$rep_c2c == 1))
})

# -----------------------------------------------------------------------------
# ML weights with id_var
# -----------------------------------------------------------------------------

testthat::test_that("id_var works with ML weights", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")

  result <- cat2cat(
    data = list(
      old = panel_2009Q4,
      new = panel_2010Q1,
      id_var = "panel_id",
      cat_var = "code",
      time_var = "quarter"
    ),
    mappings = list(trans = trans, direction = "backward"),
    ml = list(
      data = panel_2010Q1,
      cat_var = "code",
      method = "knn",
      features = c("age", "sex", "edu", "exp", "salary"),
      args = list(k = 5)
    )
  )

  # ML weights should be added
  expect_true("wei_knn_c2c" %in% names(result$old))

  # ML weights sum to original n
  expect_equal(sum(result$old$wei_knn_c2c), nrow(panel_2009Q4))

  # Direct matches should have ML weight = 1 (no ML prediction needed)
  crossing_ids <- intersect(panel_2009Q4$panel_id, panel_2010Q1$panel_id)
  direct_matches <- result$old[result$old$panel_id %in% crossing_ids, ]
  expect_true(all(direct_matches$wei_knn_c2c == 1))
})
