# Occupational dataset - small one

Occupational dataset - small one

## Usage

``` r
occup_small
```

## Format

A data frame with around 8000 observations and 12 variables.

- id:

  integer id

- age:

  numeric age of a subject

- sex:

  numeric sex of a subject

- edu:

  integer edu level of education of a subject where lower means higher -
  1 for at least master degree

- exp:

  numeric exp number of experience years for a subject

- district:

  integer district

- parttime:

  numeric contract type regards time where 1 mean full-time (work a
  whole week)

- salary:

  numeric salary per year

- code:

  character code - occupational code

- multiplier:

  numeric multiplier for the subject to reproduce a population - how
  many of such subjects in population

- year:

  integer year

- code4:

  character code - occupational code - first 4 digits

## Details

occup dataset is an example of unbalance panel dataset. This is a
simulated data although there are applied a real world characteristics
from national statistical office survey. The original survey is
anonymous and take place every two years. It is presenting a
characteristics from randomly selected company and then using k step
procedure employees are chosen.

occupational dataset

## Examples

``` r
set.seed(1234)
data("occup", package = "cat2cat")
occup_small <- occup[sort(sample(nrow(occup), 8000)), ]
```
