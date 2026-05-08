# Pruning which could be useful after the mapping process

user could specify one of four methods to prune replications created in
the cat2cat procedure.

## Usage

``` r
prune_c2c(
  df,
  index = "index_c2c",
  column = "wei_freq_c2c",
  method = "nonzero",
  percent = 50
)
```

## Arguments

- df:

  \`data.frame\` like result of the \`cat2cat\` function for a specific
  period.

- index:

  \`character(1)\` a column name with the \`cat2cat\` identifier. Should
  not be updated in most cases. Default \`index_c2c\`.

- column:

  \`character(1)\` a column name with weights, default \`wei_freq_c2c\`.

- method:

  \`character(1)\` one of four available methods: "nonzero" (default),
  "highest", "highest1" or "morethan".

- percent:

  \`integer(1)\` from 0 to 99

## Value

\`data.frame\` with the same structure and possibly reduced number of
rows

## Details

method - specify a method to reduce number of replications

- "nonzero":

  remove nonzero probabilities

- "highest":

  leave only highest probabilities for each subject- accepting ties

- "highest1":

  leave only highest probabilities for each subject - not accepting ties
  so always one is returned

- "morethan":

  leave rows where a probability is higher than value specify by percent
  argument

## Examples

``` r
if (FALSE) { # \dontrun{
data("occup_small", package = "cat2cat")
data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_old <- occup_small[occup_small$year == 2008, ]
occup_new <- occup_small[occup_small$year == 2010, ]

occup_ml <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_new,
    cat_var = "code",
    method = "knn",
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

prune_c2c(occup_ml$old, method = "nonzero")
prune_c2c(occup_ml$old, method = "highest")
prune_c2c(occup_ml$old, method = "highest1")
prune_c2c(occup_ml$old, method = "morethan", percent = 90)

prune_c2c(occup_ml$old, column = "wei_knn_c2c", method = "nonzero")
} # }
```
