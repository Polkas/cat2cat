# Package index

## main functions

- [`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md) :
  Automatic mapping in a panel dataset
- [`cat2cat_agg()`](https://polkas.github.io/cat2cat/reference/cat2cat_agg.md)
  : Manual mapping for an aggregated panel dataset

## cat2cat ml functions

- [`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md)
  [`print(`*`<cat2cat_ml_run>`*`)`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md)
  : Cross-validation diagnostics for cat2cat ML models

## Datasets

- [`occup`](https://polkas.github.io/cat2cat/reference/occup.md) :
  Occupational dataset
- [`occup_small`](https://polkas.github.io/cat2cat/reference/occup_small.md)
  : Occupational dataset - small one
- [`occup_panel`](https://polkas.github.io/cat2cat/reference/occup_panel.md)
  : Occupational panel dataset with BAEL-style quarterly rotation
- [`verticals`](https://polkas.github.io/cat2cat/reference/verticals.md)
  : verticals dataset
- [`verticals2`](https://polkas.github.io/cat2cat/reference/verticals2.md)
  : verticals2 dataset
- [`trans`](https://polkas.github.io/cat2cat/reference/trans.md) : trans
  dataset containing mappings (transitions) between old (2008) and
  new (2010) occupational codes. This table could be used to map
  encodings in both directions.

## cat2cat utils

- [`dummy_c2c()`](https://polkas.github.io/cat2cat/reference/dummy_c2c.md)
  : Add default cat2cat columns to a \`data.frame\`
- [`prune_c2c()`](https://polkas.github.io/cat2cat/reference/prune_c2c.md)
  : Pruning which could be useful after the mapping process
- [`cross_c2c()`](https://polkas.github.io/cat2cat/reference/cross_c2c.md)
  : Make a combination of weights from different methods
- [`plot_c2c()`](https://polkas.github.io/cat2cat/reference/plot_c2c.md)
  : Summary plots for cat2cat results
- [`summary_c2c()`](https://polkas.github.io/cat2cat/reference/summary_c2c.md)
  : Adjusted summary for regressions on replicated datasets

## raw - replications and frequencies utils

- [`get_mappings()`](https://polkas.github.io/cat2cat/reference/get_mappings.md)
  : Transforming a mapping (transition) table to two associative lists
- [`get_freqs()`](https://polkas.github.io/cat2cat/reference/get_freqs.md)
  : Getting frequencies from a vector with an optional multiplier
- [`cat_apply_freq()`](https://polkas.github.io/cat2cat/reference/cat_apply_freq.md)
  : Applying frequencies to the object returned by the \`get_mappings\`
  function
