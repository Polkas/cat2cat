# Transforming a mapping (transition) table to two associative lists

to rearrange the one classification encoding into another, an
associative list that maps keys to values is used. More precisely, an
association list is used which is a linked list in which each list
element consists of a key and value or values. An association list where
unique categories codes are keys and matching categories from next or
previous time point are values. A mapping (transition) table is used to
build such associative lists.

## Usage

``` r
get_mappings(x = data.frame())
```

## Arguments

- x:

  \`data.frame\` or \`matrix\` - mapping (transition) table with 2
  columns where first column is assumed to be the older encoding.

## Value

a list with 2 named lists \`to_old\` and \`to_new\`.

## Examples

``` r
data("trans", package = "cat2cat")

mappings <- get_mappings(trans)
mappings$to_old[1:4]
#> $`111101`
#> [1] "1111"
#> 
#> $`111102`
#> [1] "1111"
#> 
#> $`111103`
#> [1] "1111"
#> 
#> $`111201`
#> [1] "1112"
#> 
mappings$to_new[1:4]
#> $`1111`
#> [1] "111101" "111102" "111103"
#> 
#> $`1112`
#> [1] "111201" "111202" "111301"
#> 
#> $`1121`
#> [1] "111402"
#> 
#> $`1122`
#> [1] "111401" "111403" "111404"
#> 
```
