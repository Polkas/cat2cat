# Occupational panel dataset with BAEL-style quarterly rotation

Occupational panel dataset with BAEL-style quarterly rotation

## Usage

``` r
occup_panel
```

## Format

A data frame with approximately 3900 observations and 15 variables.

- id:

  integer original row identifier from `occup`

- panel_id:

  integer consistent person identifier across quarters

- cohort:

  character entry quarter (e.g., "2009Q1")

- age:

  numeric age of the worker

- sex:

  numeric sex of the worker

- edu:

  integer education level (1 = highest)

- exp:

  numeric years of experience

- district:

  integer district code

- parttime:

  numeric contract type (1 = full-time)

- salary:

  numeric annual salary

- code:

  character occupational code (4-digit pre-2010, 6-digit post-2010)

- multiplier:

  numeric survey weight

- quarter:

  character survey quarter (e.g., "2009Q1", "2010Q2")

- code4:

  character first 4 digits of occupational code

- year:

  integer year extracted from quarter

- quarter_num:

  integer quarter number (1-4)

## Details

A simulated rotational panel derived from `occup`, inspired by the
Polish BAEL (Badanie Aktywnosci Ekonomicznej Ludnosci - Labour Force
Survey). Unlike the main `occup` dataset (repeated cross-sections), this
dataset includes workers observed for 4 consecutive quarters, enabling
demonstration of the `id_var` feature in
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md).

Panel design:

- 8 quarters: 2009Q1 through 2010Q4

- Encoding change between 2009Q4 and 2010Q1

- Each cohort enters quarterly and stays for 4 consecutive quarters

- ~150 new subjects enter each quarter (1/4 rotation)

- ~450 subjects observed across the encoding change

For subjects across quarters:

- `panel_id` is consistent across quarters

- Occupation code is preserved (or mapped via `trans` at encoding
  change)

- Age and experience increase annually (every 4 quarters)

- Salary varies slightly between quarters (-1% to +2%)

## See also

[`occup`](https://polkas.github.io/cat2cat/reference/occup.md) for the
full repeated cross-section dataset,
[`trans`](https://polkas.github.io/cat2cat/reference/trans.md) for the
transition table

## Examples

``` r
data("occup_panel", package = "cat2cat")

# Check panel structure
table(occup_panel$quarter)
#> 
#> 2009Q1 2009Q2 2009Q3 2009Q4 2010Q1 2010Q2 2010Q3 2010Q4 
#>    150    300    450    600    600    600    600    600 
table(table(occup_panel$panel_id))  # appearances per subject (target: 4)
#> 
#>   1   2   3   4 
#> 150 150 150 750 

# Subjects observed across the encoding change (2009Q4 -> 2010Q1)
panel_2009Q4 <- occup_panel[occup_panel$quarter == "2009Q4", ]
panel_2010Q1 <- occup_panel[occup_panel$quarter == "2010Q1", ]
length(intersect(panel_2009Q4$panel_id, panel_2010Q1$panel_id))
#> [1] 450
```
