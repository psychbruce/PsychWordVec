# Tabulate cosine similarity/distance of word pairs.

Tabulate cosine similarity/distance of word pairs.

## Usage

``` r
tab_similarity(
  data,
  words = NULL,
  pattern = NULL,
  words1 = NULL,
  words2 = NULL,
  unique = FALSE,
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

- unique:

  Return unique word pairs (`TRUE`) or all pairs with duplicates
  (`FALSE`; default).

- distance:

  Compute cosine distance instead? Defaults to `FALSE` (cosine
  similarity).

## Value

A `data.table` of words, word pairs, and their cosine similarity
(`cos_sim`) or cosine distance (`cos_dist`).

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## See also

[`cosine_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md)

[`pair_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/pair_similarity.md)

[`plot_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/plot_similarity.md)

[`most_similar()`](https://psychbruce.github.io/PsychWordVec/reference/most_similar.md)

[`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md)

[`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md)

## Examples

``` r
tab_similarity(demodata, cc("king, queen, man, woman"))
#>      word1  word2    wordpair   cos_sim
#>     <char> <char>      <char>     <num>
#>  1:   king   king   king-king 1.0000000
#>  2:   king  queen  king-queen 0.6510958
#>  3:   king    man    king-man 0.2294268
#>  4:   king  woman  king-woman 0.1284797
#>  5:  queen   king  queen-king 0.6510958
#>  6:  queen  queen queen-queen 1.0000000
#>  7:  queen    man   queen-man 0.1665822
#>  8:  queen  woman queen-woman 0.3161813
#>  9:    man   king    man-king 0.2294268
#> 10:    man  queen   man-queen 0.1665822
#> 11:    man    man     man-man 1.0000000
#> 12:    man  woman   man-woman 0.7664012
#> 13:  woman   king  woman-king 0.1284797
#> 14:  woman  queen woman-queen 0.3161813
#> 15:  woman    man   woman-man 0.7664012
#> 16:  woman  woman woman-woman 1.0000000
tab_similarity(demodata, cc("king, queen, man, woman"),
               unique=TRUE)
#>     word1  word2    wordpair   cos_sim
#>    <char> <char>      <char>     <num>
#> 1:   king  queen  king-queen 0.6510958
#> 2:   king    man    king-man 0.2294268
#> 3:   king  woman  king-woman 0.1284797
#> 4:  queen    man   queen-man 0.1665822
#> 5:  queen  woman queen-woman 0.3161813
#> 6:    man  woman   man-woman 0.7664012

tab_similarity(demodata, cc("Beijing, China, Tokyo, Japan"))
#>       word1   word2        wordpair   cos_sim
#>      <char>  <char>          <char>     <num>
#>  1: Beijing Beijing Beijing-Beijing 1.0000000
#>  2: Beijing   China   Beijing-China 0.7648461
#>  3: Beijing   Tokyo   Beijing-Tokyo 0.5229628
#>  4: Beijing   Japan   Beijing-Japan 0.3995245
#>  5:   China Beijing   China-Beijing 0.7648461
#>  6:   China   China     China-China 1.0000000
#>  7:   China   Tokyo     China-Tokyo 0.3814305
#>  8:   China   Japan     China-Japan 0.5967756
#>  9:   Tokyo Beijing   Tokyo-Beijing 0.5229628
#> 10:   Tokyo   China     Tokyo-China 0.3814305
#> 11:   Tokyo   Tokyo     Tokyo-Tokyo 1.0000000
#> 12:   Tokyo   Japan     Tokyo-Japan 0.7002254
#> 13:   Japan Beijing   Japan-Beijing 0.3995245
#> 14:   Japan   China     Japan-China 0.5967756
#> 15:   Japan   Tokyo     Japan-Tokyo 0.7002254
#> 16:   Japan   Japan     Japan-Japan 1.0000000
tab_similarity(demodata, cc("Beijing, China, Tokyo, Japan"),
               unique=TRUE)
#>      word1  word2      wordpair   cos_sim
#>     <char> <char>        <char>     <num>
#> 1: Beijing  China Beijing-China 0.7648461
#> 2: Beijing  Tokyo Beijing-Tokyo 0.5229628
#> 3: Beijing  Japan Beijing-Japan 0.3995245
#> 4:   China  Tokyo   China-Tokyo 0.3814305
#> 5:   China  Japan   China-Japan 0.5967756
#> 6:   Tokyo  Japan   Tokyo-Japan 0.7002254

## only n1 * n2 word pairs across two sets of words
tab_similarity(demodata,
               words1=cc("king, queen, King, Queen"),
               words2=cc("man, woman"))
#>     word1  word2    wordpair    cos_sim
#>    <char> <char>      <char>      <num>
#> 1:   king    man    king-man 0.22942676
#> 2:   king  woman  king-woman 0.12847968
#> 3:  queen    man   queen-man 0.16658216
#> 4:  queen  woman queen-woman 0.31618132
#> 5:   King    man    King-man 0.15777646
#> 6:   King  woman  King-woman 0.06369529
#> 7:  Queen    man   Queen-man 0.09365463
#> 8:  Queen  woman Queen-woman 0.20171619
```
