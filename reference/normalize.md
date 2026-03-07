# Normalize all word vectors to the unit length 1.

L2-normalization (scaling to unit euclidean length): the *norm* of each
vector in the vector space will be normalized to 1. It is necessary for
any linear operation of word vectors.

## Usage

``` r
normalize(x)
```

## Arguments

- x:

  A
  [`wordvec`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (data.table) or
  [`embed`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (matrix), see
  [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md).

## Value

A `wordvec` (data.table) or `embed` (matrix) with *normalized* word
vectors.

## Details

R code inside:

- Vector: `vec / sqrt(sum(vec^2))`

- Matrix: `mat / sqrt(rowSums(mat^2))`

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## See also

[`as_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
/
[`as_embed()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)

[`load_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
/
[`load_embed()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)

[`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md)

[`data_wordvec_subset()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md)

## Examples

``` r
d = normalize(demodata)
# the same: d = as_wordvec(demodata, normalize=TRUE)
```
