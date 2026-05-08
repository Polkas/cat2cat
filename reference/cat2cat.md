# Automatic mapping in a panel dataset

Unifies an inconsistently coded categorical variable in a panel dataset
according to a mapping (transition) table. Apply iteratively for each
pair of neighboring periods. Use
[`prune_c2c`](https://polkas.github.io/cat2cat/reference/prune_c2c.md)
to limit growing replications across steps.

## Usage

``` r
cat2cat(
  data = list(old = NULL, new = NULL, time_var = NULL, cat_var = NULL, cat_var_old =
    NULL, cat_var_new = NULL, id_var = NULL, multiplier_var = NULL),
  mappings = list(trans = NULL, direction = NULL, freqs_df = NULL),
  ml = list(data = NULL, cat_var = NULL, method = NULL, features = NULL, args = NULL)
)
```

## Arguments

- data:

  \`named list\` with fields \`old\`, \`new\`, \`cat_var\` (shorthand
  for \`cat_var_old = cat_var_new = cat_var\`), \`time_var\` and
  optional \`id_var\`, \`multiplier_var\`.

- mappings:

  \`named list\` with 3 fields \`trans\`, \`direction\` and optional
  \`freqs_df\`.

- ml:

  \`named list\` (optional) with fields \`data\`, \`cat_var\`,
  \`method\`, \`features\` and optional \`args\`, \`on_fail\`,
  \`fail_warn\`.

## Value

\`named list\` with 2 fields old and new - 2 data.frames. There will be
added additional columns like index_c2c, g_new_c2c, wei_freq_c2c,
rep_c2c, wei\_(ml method name)\_c2c. Additional columns will be
informative only for a one data.frame as we always make the changes to
one direction. The new columns are added instead of the additional
metadata as we are working with new datasets where observations could be
replicated. For the transparency the probability and number of
replications are part of each observation in the \`data.frame\`.

## Details

data args

- "old":

  data.frame older time point in a panel

- "new":

  data.frame more recent time point in a panel

- "time_var":

  character(1) name of the time variable.

- "cat_var":

  character(1) name of the categorical variable. Shorthand: sets both
  `cat_var_old` and `cat_var_new` to the same value.

- "cat_var_old":

  Optional character(1) name of the categorical variable in the older
  time point. Default \`cat_var\`.

- "cat_var_new":

  Optional character(1) name of the categorical variable in the newer
  time point. Default \`cat_var\`.

- "id_var":

  Optional character(1) name of the unique identifier variable. When
  specified, subjects observed in both periods are mapped 1-to-1
  directly (no replication, `wei_freq_c2c = 1, rep_c2c = 1`). Only
  subjects absent from the base period enter the replication path. This
  assumes a subject's true category does not change between adjacent
  waves. See
  [`vignette("cat2cat_advanced")`](https://polkas.github.io/cat2cat/articles/cat2cat_advanced.md)
  for details on when this assumption is and is not satisfied.

- "multiplier_var":

  Optional character(1) name of the multiplier variable - number of
  replication needed to reproduce the population

- "freqs_df":

  Deprecated - use `mappings$freqs_df` instead.

mappings args

- "trans":

  data.frame with 2 columns - mapping (transition) table - all
  categories for cat_var in old and new datasets have to be included.
  First column contains an old encoding and second a new one. The
  mapping (transition) table should include a candidate for each
  category in the period being harmonised.

- "direction":

  character(1) direction - "backward" or "forward"

- "freqs_df":

  Optional - data.frame with 2 columns where first one is category name
  (base period) and second counts. If It is not provided then is
  assessed automatically. Artificial counts for each variable level in
  the base period. It is optional nevertheless will be often needed, as
  gives more control. It will be used to assess the probabilities. The
  multiplier variable is omitted so sb has to apply it in this table.

Optional ml args

- "data":

  data.frame - dataset with features and the \`cat_var\`.

- "cat_var":

  character(1) - the dependent variable name.

- "method":

  character vector - one or a few from "knn", "rf", "lda" and "nb"
  methods - "knn" k-NearestNeighbors, "lda" Linear Discriminant
  Analysis, "rf" Random Forest, "nb" Naive Bayes

- "features":

  character vector of features names where all have to be numeric,
  logical or factor. Factor features are automatically one-hot encoded
  using the union of levels observed in `ml$data` and the target period.

- "args":

  optional - list parameters: knn: k ; rf: ntree

- "on_fail":

  optional character(1) controlling failed ML weights: `"freq"`
  (default) uses `wei_freq_c2c`, `"naive"` uses `wei_naive_c2c`, `"na"`
  leaves failed weights as `NA`, and `"error"` stops when failed weights
  are detected.

- "fail_warn":

  optional logical(1), default `TRUE`; warn when failed ML weights are
  replaced or retained as `NA`.

Without the `ml` argument, only frequency-based weights are computed. If
an ML model fails, `ml$on_fail` controls whether frequency weights,
naive weights, `NA`, or an error are used. The `knn` method is
recommended for smaller datasets.

## Note

`trans` columns and `cat_var` must be of the same type. The mapping
table must include a candidate for every category in the target period.
Observations without a matched candidate are dropped; add a `c(NA, NA)`
row to `trans` to retain them as `NA`.

## See also

- [`cat2cat_ml_run`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md) -
  validate ML performance before use

- [`prune_c2c`](https://polkas.github.io/cat2cat/reference/prune_c2c.md),
  [`cross_c2c`](https://polkas.github.io/cat2cat/reference/cross_c2c.md) -
  post-processing

- [`summary_c2c`](https://polkas.github.io/cat2cat/reference/summary_c2c.md),
  [`dummy_c2c`](https://polkas.github.io/cat2cat/reference/dummy_c2c.md) -
  helpers

- [`cat2cat_agg`](https://polkas.github.io/cat2cat/reference/cat2cat_agg.md) -
  for pre-aggregated data

- [`vignette("cat2cat")`](https://polkas.github.io/cat2cat/articles/cat2cat.md) -
  identification assumptions and worked examples

## Examples

``` r
if (FALSE) { # \dontrun{
data("occup_small", package = "cat2cat")
data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_old <- occup_small[occup_small$year == 2008, ]
occup_new <- occup_small[occup_small$year == 2010, ]

# Adding the dummy level to the mapping table for levels without a candidate
# The best to fill them manually with proper candidates, if possible
# In this case it is only needed for forward mapping, to suppress warnings
trans2 <- rbind(
  trans,
  data.frame(
    old = "no_cat",
    new = setdiff(c(occup_new$code), trans$new)
  )
)

# default only simple frequencies
occup_simple <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans2, direction = "forward")
)

mappings <- list(trans = trans, direction = "backward")

ml_setup <- list(
    data = occup_small[occup_small$year >= 2010, ],
    cat_var = "code",
    method = "knn",
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10),
    # defaults for failed ML weights
    on_fail = "freq",
    fail_warn = TRUE
)

# ml model performance check
print(cat2cat_ml_run(mappings, ml_setup))

# additional probabilities from knn
occup_ml <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = mappings,
  ml = ml_setup
)

# strict mode: stop when any ML weights cannot be produced
ml_setup_strict <- ml_setup
ml_setup_strict$on_fail <- "error"

# diagnostic mode: keep failed ML weights as NA and silence warnings
ml_setup_diag <- ml_setup
ml_setup_diag$on_fail <- "na"
ml_setup_diag$fail_warn <- FALSE
} # }
```
