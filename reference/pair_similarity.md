# Compute a matrix of cosine similarity/distance of word pairs.

Compute a matrix of cosine similarity/distance of word pairs.

## Usage

``` r
pair_similarity(
  data,
  words = NULL,
  pattern = NULL,
  words1 = NULL,
  words2 = NULL,
  distance = FALSE
)
```

## Arguments

- data:

  A
  [`wordvec`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (data.table) or
  [`embed`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (matrix), see
  [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md).

- words:

  \[Option 1\] Character string(s).

- pattern:

  \[Option 2\] [Regular
  expression](https://stringr.tidyverse.org/reference/str_subset.html).
  If `words` and `pattern` are not specified, all words in the data will
  be extracted.

- words1, words2:

  \[Option 3\] Two sets of words for only n1 \* n2 word pairs. See
  examples.

- distance:

  Compute cosine distance instead? Defaults to `FALSE` (cosine
  similarity).

## Value

A matrix of pairwise cosine similarity/distance.

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## See also

[`cosine_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md)

[`plot_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/plot_similarity.md)

[`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md)

[`most_similar()`](https://psychbruce.github.io/PsychWordVec/reference/most_similar.md)

## Examples

``` r
pair_similarity(demodata, c("China", "Chinese"))
#>             China   Chinese
#> China   1.0000000 0.7678081
#> Chinese 0.7678081 1.0000000

pair_similarity(demodata, pattern="^Chi")
#> 4 words matched...
#>             China    Chicago    Chinese      Chile
#> China   1.0000000 0.13040186 0.76780811 0.38012317
#> Chicago 0.1304019 1.00000000 0.09174141 0.08685822
#> Chinese 0.7678081 0.09174141 1.00000000 0.21538189
#> Chile   0.3801232 0.08685822 0.21538189 1.00000000

pair_similarity(demodata,
                words1=c("China", "Chinese"),
                words2=c("Japan", "Japanese"))
#>             Japan Japanese
#> China   0.5967756 0.413391
#> Chinese 0.4226447 0.642242
```
