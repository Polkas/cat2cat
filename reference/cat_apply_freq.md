# Applying frequencies to the object returned by the \`get_mappings\` function

applying frequencies to the object returned by the \`get_mappings\`
function. We will get a symmetric object to the one returned by the
\`get_mappings\` function, nevertheless categories are replaced with
frequencies. Frequencies for each category/key are sum to 1, so could be
interpreted as probabilities.

## Usage

``` r
cat_apply_freq(to_x, freqs)
```

## Arguments

- to_x:

  \`list\` object returned by \`get_mappings\`.

- freqs:

  \`data.frame\` object like the one returned by the \`get_freqs\`
  function.

## Value

a \`list\` with a structure like \`to_x\` object but with probabilities
for each category.

## Note

\`freqs\` arg first column (keys) and the to_x arg values have to be of
the same type. The uniform distribution (outcomes are equally likely) is
assumed for no match for all possible categories.

## Examples

``` r
data("trans", package = "cat2cat")
data("occup", package = "cat2cat")

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

mapp_p <- cat_apply_freq(
  mappings$to_old,
  get_freqs(
    occup$code[occup$year == "2008"],
    occup$multiplier[occup$year == "2008"]
  )
)
head(data.frame(I(mappings$to_old), I(mapp_p)))
#>        mappings.to_old mapp_p
#> 111101            1111      1
#> 111102            1111      1
#> 111103            1111      1
#> 111201            1112      1
#> 111202            1112      1
#> 111301            1112      1
mapp_p <- cat_apply_freq(
  mappings$to_new,
  get_freqs(
    occup$code[occup$year == "2010"],
    occup$multiplier[occup$year == "2010"]
  )
)
head(data.frame(I(mappings$to_new), I(mapp_p)))
#>      mappings.to_new       mapp_p
#> 1111    111101, .... 0, 0.100....
#> 1112    111201, .... 0.079357....
#> 1121          111402            1
#> 1122    111401, .... 0.333333....
#> 1123          111405            1
#> 1211    112007, .... 0.370800....
```
