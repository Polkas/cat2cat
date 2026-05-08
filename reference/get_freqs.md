# Getting frequencies from a vector with an optional multiplier

getting frequencies for a vector with an optional multiplier.

## Usage

``` r
get_freqs(x, multiplier = NULL)
```

## Arguments

- x:

  \`vector\` categorical variable to summarize.

- multiplier:

  \`numeric\` vector how many times to repeat certain value, additional
  weights.

## Value

\`data.frame\` with two columns \`input\` \`Freq\`

## Note

without multiplier variable it is a basic \`table\` function wrapped
with the \`as.data.frame\` function. The \`table\` function is used with
the \`useNA = "ifany"\` argument.

## Examples

``` r
data("occup", package = "cat2cat")

head(get_freqs(occup$code[occup$year == "2008"]))
#>   input Freq
#> 1  1112   35
#> 2  1123    2
#> 3  1211  202
#> 4  1212   91
#> 5  1221    9
#> 6  1222   18
head(get_freqs(occup$code[occup$year == "2010"]))
#>    input Freq
#> 1 111102    1
#> 2 111103   10
#> 3 111201    3
#> 4 111301   34
#> 5 111405    5
#> 6 112001   13

head(
  get_freqs(
    occup$code[occup$year == "2008"],
    occup$multiplier[occup$year == "2008"]
  )
)
#>   input   Freq
#> 1  1112  15599
#> 2  1123    959
#> 3  1211 102790
#> 4  1212  49196
#> 5  1221   4181
#> 6  1222   9193
head(
  get_freqs(
    occup$code[occup$year == "2010"],
    occup$multiplier[occup$year == "2010"]
  )
)
#>    input  Freq
#> 1 111102   422
#> 2 111103  3784
#> 3 111201  1399
#> 4 111301 16230
#> 5 111405  1896
#> 6 112001  6694
```
