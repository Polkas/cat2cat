# Add default cat2cat columns to a \`data.frame\`

a utils function to add default cat2cat columns to a \`data.frame\`. It
will be useful e.g. for a boarder periods which will not have additional
\`cat2cat\` columns.

## Usage

``` r
dummy_c2c(df, cat_var, ml = NULL)
```

## Arguments

- df:

  \`data.frame\`.

- cat_var:

  \`character(1)\` a categorical variable name.

- ml:

  \`character\` vector of ml models applied, any of \`c("knn", "rf",
  "lda", "nb")\`.

## Value

the provided \`data.frame\` with additional \`cat2cat\` like columns.

## Examples

``` r
if (FALSE) { # \dontrun{
dummy_c2c(airquality, "Month")

data("occup_small", package = "cat2cat")
occup_old <- occup_small[occup_small$year == 2008, ]
dummy_c2c(occup_old, "code")
dummy_c2c(occup_old, "code", "knn")
} # }
```
