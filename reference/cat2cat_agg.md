# Manual mapping for an aggregated panel dataset

Applies user-defined mapping equations to redistribute aggregated counts
(and other numeric columns) when categorical encodings change between
time periods. Unlike
[`cat2cat`](https://polkas.github.io/cat2cat/reference/cat2cat.md),
which works on micro-data, this function operates on pre-aggregated data
where each row represents a category with associated counts/totals.

## Usage

``` r
cat2cat_agg(
  data = list(old = NULL, new = NULL, cat_var_old = NULL, cat_var_new = NULL, time_var =
    NULL, freq_var = NULL),
  ...
)
```

## Arguments

- data:

  Named list with 5-6 fields describing the datasets:

  `old`

  :   `data.frame` - older time period (one row per category).

  `new`

  :   `data.frame` - newer time period (one row per category).

  `cat_var`

  :   `character(1)` - (deprecated) category variable name if identical
      in both periods. Use `cat_var_old`/`cat_var_new` instead.

  `cat_var_old`

  :   `character(1)` - category variable name in old period.

  `cat_var_new`

  :   `character(1)` - category variable name in new period.

  `time_var`

  :   `character(1)` - time variable name.

  `freq_var`

  :   `character(1)` - frequency/count variable used to compute
      proportions when splitting one category into many.

- ...:

  Mapping equations specifying how categories relate across periods. See
  \*\*Equation syntax\*\* in Details.

## Value

A named list with two elements:

- `$old`:

  `data.frame` - old period with `prop_c2c` column added. Categories may
  be replicated if split by backward equations.

- `$new`:

  `data.frame` - new period with `prop_c2c` column added. Categories may
  be replicated if split by forward equations.

The `prop_c2c` column contains the proportion (0-1) to apply when
aggregating. For rows not affected by any equation, `prop_c2c = 1`. For
split categories, proportions sum to 1 within the original category.

## Details

**Equation syntax**

Each equation has the form:

    OLD_SIDE  DIRECTION  NEW_SIDE

where:

- **OLD_SIDE** - one or more old-period category names (use `c(A, B)`
  for multiple)

- **DIRECTION** - one of:

  - `%>%` or `>` - **forward**: replicates the NEW period,
    renaming/splitting new categories to match old encoding

  - `%<%` or `<` - **backward**: replicates the OLD period,
    renaming/splitting old categories to match new encoding

- **NEW_SIDE** - one or more new-period category names

**How proportions are calculated**

When one category maps to multiple:

- **Backward** (`%<%`): proportions come from `freq_var` in the **new**
  period (the target encoding)

- **Forward** (`%>%`): proportions come from `freq_var` in the **old**
  period (the target encoding)

**Examples of valid equations**

- `Automotive %<% c(Automotive1, Automotive2)`:

  Backward: the old "Automotive" row is split into two rows
  ("Automotive1", "Automotive2") with proportions from new-period
  counts.

- `c(Kids1, Kids2) %>% c(Kids)`:

  Forward: the new "Kids" row is split into two rows ("Kids1", "Kids2")
  with proportions from old-period counts.

- `Home %>% c(Home, Supermarket)`:

  Forward: the new "Home" and "Supermarket" rows are each renamed to
  "Home" (1-to-many from new perspective; after aggregation they merge).

**Typical workflow**

1\. Call `cat2cat_agg()` with all mapping equations. 2. Bind `$old` and
`$new` together. 3. Group by time and the (now unified) category
variable. 4. Summarise numeric columns as `sum(value * prop_c2c)`.

## Note

- All equations must be valid - unknown category names cause an error.

- Each equation must have exactly one category on one side (the "one" in
  one-to-many). Many-to-many within a single equation is not allowed.

- Each category in `old` and `new` must appear exactly once (no
  duplicates allowed before mapping).

## See also

[`vignette("cat2cat_advanced")`](https://polkas.github.io/cat2cat/articles/cat2cat_advanced.md)
for a complete workflow example.

## Examples

``` r
data("verticals", package = "cat2cat")
agg_old <- verticals[verticals$v_date == "2020-04-01", ]
agg_new <- verticals[verticals$v_date == "2020-05-01", ]

# cat2cat_agg - can map in both directions at once
# although usually we want to have the old or the new representation

agg <- cat2cat_agg(
  data = list(
    old = agg_old,
    new = agg_new,
    cat_var_old = "vertical",
    cat_var_new = "vertical",
    time_var = "v_date",
    freq_var = "counts"
  ),
  Automotive %<% c(Automotive1, Automotive2),
  c(Kids1, Kids2) %>% c(Kids),
  Home %>% c(Home, Supermarket)
)

## possible processing
library("dplyr")
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
agg %>%
  bind_rows() %>%
  group_by(v_date, vertical) %>%
  summarise(
    sales = sum(sales * prop_c2c),
    counts = sum(counts * prop_c2c),
    v_date = first(v_date)
  )
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by v_date and vertical.
#> ℹ Output is grouped by v_date.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(v_date, vertical))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
#> # A tibble: 22 × 4
#> # Groups:   v_date [2]
#>    v_date     vertical    sales  counts
#>    <chr>      <chr>       <dbl>   <dbl>
#>  1 2020-04-01 Automotive1  49.4    87.1
#>  2 2020-04-01 Automotive2  27.2    47.9
#>  3 2020-04-01 Books       104.   7489  
#>  4 2020-04-01 Clothes     105.   1078  
#>  5 2020-04-01 Electronics  87.9  9544  
#>  6 2020-04-01 Fashion      94.5  7399  
#>  7 2020-04-01 Health       94.4 16102  
#>  8 2020-04-01 Home         94.3  2414  
#>  9 2020-04-01 Kids1       103.  17686  
#> 10 2020-04-01 Kids2       111.  32349  
#> # ℹ 12 more rows
```
