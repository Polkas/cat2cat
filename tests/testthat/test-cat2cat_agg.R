agg_old <- data.frame(
  vertical = c(
    "Electronics", "Kids1", "Kids2", "Automotive", "Books",
    "Clothes", "Home", "Fashion", "Health", "Sport"
  ),
  sales = rnorm(10, 100, 10),
  counts = rgeom(10, 0.0001),
  v_date = rep("2020-04-01", 10), stringsAsFactors = F
)

agg_new <- data.frame(
  vertical = c(
    "Electronics", "Supermarket", "Kids", "Automotive1",
    "Automotive2", "Books", "Clothes", "Home", "Fashion", "Health", "Sport"
  ),
  sales = rnorm(11, 100, 10),
  counts = rgeom(11, 0.0001),
  v_date = rep("2020-05-01", 11), stringsAsFactors = F
)

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

expect_true(sum(agg$old$prop_c2c) == nrow(agg_old))
expect_true(sum(agg$new$prop_c2c) == nrow(agg_new))
expect_true(all(agg$new$prop_c2c >= 0 & agg$new$prop_c2c <= 1))
expect_true(all(agg$old$prop_c2c >= 0 & agg$old$prop_c2c <= 1))
