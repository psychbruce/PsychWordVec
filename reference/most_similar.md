# Find the Top-N most similar words.

Find the Top-N most similar words, which replicates the results produced
by the Python `gensim` module `most_similar()` function. (Exact
replication of `gensim` requires the same word vectors data, not the
`demodata` used here in examples.)

## Usage

``` r
most_similar(
  data,
  x = NULL,
  topn = 10,
  above = NULL,
  keep = FALSE,
  row.id = TRUE,
  verbose = TRUE
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

- x:

  Can be:

  - `NULL`: use the sum of all word vectors in `data`

  - a single word:

    - `"China"`

  - a vector of words:

    - `c("king", "queen")`

    - `cc(" king , queen ; man | woman")`

  - an R formula (`~ xxx`) specifying words that positively and
    negatively contribute to the similarity (for word analogy):

    - `~ boy - he + she`

    - `~ king - man + woman`

    - `~ Beijing - China + Japan`

- topn:

  Top-N most similar words. Defaults to `10`.

- above:

  Defaults to `NULL`. Can be:

  - a threshold value to find all words with cosine similarities higher
    than this value

  - a critical word to find all words with cosine similarities higher
    than that with this critical word

  If both `topn` and `above` are specified, `above` wins.

- keep:

  Keep words specified in `x` in results? Defaults to `FALSE`.

- row.id:

  Return the row number of each word? Defaults to `TRUE`, which may help
  determine the relative word frequency in some cases.

- verbose:

  Print information to the console? Defaults to `TRUE`.

## Value

A `data.table` with the most similar words and their cosine
similarities.

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## See also

[`sum_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/sum_wordvec.md)

[`dict_expand()`](https://psychbruce.github.io/PsychWordVec/reference/dict_expand.md)

[`dict_reliability()`](https://psychbruce.github.io/PsychWordVec/reference/dict_reliability.md)

[`cosine_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md)

[`pair_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/pair_similarity.md)

[`plot_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/plot_similarity.md)

[`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md)

## Examples

``` r
d = as_embed(demodata, normalize=TRUE)

most_similar(d)
#> [Word Vector] =~ <all>
#> (normalized to unit length)
#>       word   cos_sim row_id
#>     <char>     <num>  <int>
#>  1:   just 0.5165516      3
#>  2:     do 0.5067575      9
#>  3:   that 0.4886930     11
#>  4:    the 0.4882641     13
#>  5: anyway 0.4879882     51
#>  6:   even 0.4823844     67
#>  7:    not 0.4755315     74
#>  8:     it 0.4727806    136
#>  9:     so 0.4703010    196
#> 10: really 0.4693804   3598
most_similar(d, "China")
#> [Word Vector] =~ China
#> (normalized to unit length)
#>          word   cos_sim row_id
#>        <char>     <num>  <int>
#>  1:   Chinese 0.7678081    485
#>  2:   Beijing 0.7648461    821
#>  3:    Taiwan 0.7081156    924
#>  4:  Shanghai 0.6727434   2086
#>  5:  Shenzhen 0.6239033   3011
#>  6: Guangzhou 0.6223897   3932
#>  7:      yuan 0.6005429   4619
#>  8:     India 0.6004212   7558
#>  9:     Japan 0.5967756   7984
#> 10:        Li 0.5882897   7986
most_similar(d, c("king", "queen"))
#> [Word Vector] =~ king + queen
#> (normalized to unit length)
#>          word   cos_sim row_id
#>        <char>     <num>  <int>
#>  1:     royal 0.5985594   1449
#>  2:     Queen 0.5487501   1509
#>  3:      King 0.4662653   4267
#>  4:    legend 0.3730853   4631
#>  5:     crown 0.3673733   5174
#>  6: superstar 0.3652350   5428
#>  7:  champion 0.3587134   6388
#>  8:      lady 0.3581903   6754
#>  9:     lover 0.3425792   7218
#> 10:      pope 0.3420032   7947
most_similar(d, cc(" king , queen ; man | woman "))
#> [Word Vector] =~ king + queen + man + woman
#> (normalized to unit length)
#>            word   cos_sim row_id
#>          <char>     <num>  <int>
#>  1:        girl 0.6387542    754
#>  2:         boy 0.5966774   1257
#>  3:    teenager 0.5941694   1379
#>  4:        lady 0.5923386   1537
#>  5: grandmother 0.5081522   3922
#>  6:      mother 0.5000261   4345
#>  7:       lover 0.4826535   4631
#>  8:      victim 0.4806608   4811
#>  9:   boyfriend 0.4717627   5364
#> 10:  girlfriend 0.4709186   7947

# the same as above:
most_similar(d, ~ China)
#> [Word Vector] =~ China
#> (normalized to unit length)
#>          word   cos_sim row_id
#>        <char>     <num>  <int>
#>  1:   Chinese 0.7678081    485
#>  2:   Beijing 0.7648461    821
#>  3:    Taiwan 0.7081156    924
#>  4:  Shanghai 0.6727434   2086
#>  5:  Shenzhen 0.6239033   3011
#>  6: Guangzhou 0.6223897   3932
#>  7:      yuan 0.6005429   4619
#>  8:     India 0.6004212   7558
#>  9:     Japan 0.5967756   7984
#> 10:        Li 0.5882897   7986
most_similar(d, ~ king + queen)
#> [Word Vector] =~ king + queen
#> (normalized to unit length)
#>          word   cos_sim row_id
#>        <char>     <num>  <int>
#>  1:     royal 0.5985594   1449
#>  2:     Queen 0.5487501   1509
#>  3:      King 0.4662653   4267
#>  4:    legend 0.3730853   4631
#>  5:     crown 0.3673733   5174
#>  6: superstar 0.3652350   5428
#>  7:  champion 0.3587134   6388
#>  8:      lady 0.3581903   6754
#>  9:     lover 0.3425792   7218
#> 10:      pope 0.3420032   7947
most_similar(d, ~ king + queen + man + woman)
#> [Word Vector] =~ king + queen + man + woman
#> (normalized to unit length)
#>            word   cos_sim row_id
#>          <char>     <num>  <int>
#>  1:        girl 0.6387542    754
#>  2:         boy 0.5966774   1257
#>  3:    teenager 0.5941694   1379
#>  4:        lady 0.5923386   1537
#>  5: grandmother 0.5081522   3922
#>  6:      mother 0.5000261   4345
#>  7:       lover 0.4826535   4631
#>  8:      victim 0.4806608   4811
#>  9:   boyfriend 0.4717627   5364
#> 10:  girlfriend 0.4709186   7947

most_similar(d, ~ boy - he + she)
#> [Word Vector] =~ boy - he + she
#> (normalized to unit length)
#>            word   cos_sim row_id
#>          <char>     <num>  <int>
#>  1:        girl 0.8635271    558
#>  2:       woman 0.6822032    702
#>  3:      mother 0.6530156    754
#>  4:    daughter 0.6431009   1078
#>  5:    teenager 0.6310561   1195
#>  6:       child 0.5933921   1257
#>  7: grandmother 0.5800967   1838
#>  8:        teen 0.5755065   3542
#>  9:        baby 0.5672891   4345
#> 10:       girls 0.5599151   5364
most_similar(d, ~ Jack - he + she)
#> [Word Vector] =~ Jack - he + she
#> (normalized to unit length)
#>        word   cos_sim row_id
#>      <char>     <num>  <int>
#>  1:    Jane 0.6338590   3717
#>  2: Rebecca 0.6054167   4141
#>  3:   Julie 0.5988102   4521
#>  4:   Sarah 0.5867900   4899
#>  5:     Amy 0.5831094   4955
#>  6:   Susan 0.5812214   5502
#>  7:    Lisa 0.5766206   5721
#>  8:     Ann 0.5765654   5861
#>  9:   Alice 0.5649283   6896
#> 10:   Carol 0.5647291   7340
most_similar(d, ~ Rose - she + he)
#> [Word Vector] =~ Rose - she + he
#> (normalized to unit length)
#>         word   cos_sim row_id
#>       <char>     <num>  <int>
#>  1:  Leonard 0.4443190    761
#>  2:   Martin 0.4206247   1214
#>  3:   Thomas 0.4078561   1425
#>  4:  Wallace 0.3833057   2257
#>  5:  Francis 0.3793813   2698
#>  6:  Johnson 0.3757961   3379
#>  7:    Allen 0.3736620   4350
#>  8: Robinson 0.3725750   4738
#>  9:    Evans 0.3639734   5220
#> 10:   Duncan 0.3635448   6245

most_similar(d, ~ king - man + woman)
#> [Word Vector] =~ king - man + woman
#> (normalized to unit length)
#>         word   cos_sim row_id
#>       <char>     <num>  <int>
#>  1:    queen 0.7118192     65
#>  2:    royal 0.4938203    754
#>  3:    Queen 0.4346379   1078
#>  4:     King 0.3749903   1449
#>  5:      she 0.3341126   4267
#>  6:     lady 0.3282869   4631
#>  7:   mother 0.3241257   5174
#>  8:    crown 0.3164823   6754
#>  9:     hers 0.3073009   7852
#> 10: daughter 0.3021213   7981
most_similar(d, ~ Tokyo - Japan + China)
#> [Word Vector] =~ Tokyo - Japan + China
#> (normalized to unit length)
#>          word   cos_sim row_id
#>        <char>     <num>  <int>
#>  1:   Beijing 0.8216199    924
#>  2:  Shanghai 0.7951419   2086
#>  3: Guangzhou 0.6529652   3163
#>  4:   Chinese 0.6439487   3932
#>  5:  Shenzhen 0.6439113   4619
#>  6:     Seoul 0.5868835   5180
#>  7:      yuan 0.5821307   7083
#>  8:        Li 0.5712056   7558
#>  9:      Wang 0.5192575   7984
#> 10:    Moscow 0.5082187   7986
most_similar(d, ~ Beijing - China + Japan)
#> [Word Vector] =~ Beijing - China + Japan
#> (normalized to unit length)
#>          word   cos_sim row_id
#>        <char>     <num>  <int>
#>  1:     Tokyo 0.8115592   1562
#>  2:     Seoul 0.6568831   2695
#>  3:  Japanese 0.6475989   3011
#>  4: Pyongyang 0.5348969   3017
#>  5:   Bangkok 0.4677356   3163
#>  6:     Korea 0.4660699   3768
#>  7:       yen 0.4631333   5180
#>  8:    Taiwan 0.4330458   6322
#>  9:    Moscow 0.4217667   6510
#> 10: Guangzhou 0.4154183   7986

most_similar(d, "China", above=0.7)
#> [Word Vector] =~ China
#> (normalized to unit length)
#>       word   cos_sim row_id
#>     <char>     <num>  <int>
#> 1: Chinese 0.7678081    924
#> 2: Beijing 0.7648461   2086
#> 3:  Taiwan 0.7081156   3011
most_similar(d, "China", above="Shanghai")
#> [Word Vector] =~ China
#> (normalized to unit length)
#>        word   cos_sim row_id
#>      <char>     <num>  <int>
#> 1:  Chinese 0.7678081    924
#> 2:  Beijing 0.7648461   2086
#> 3:   Taiwan 0.7081156   3011
#> 4: Shanghai 0.6727434   3932

# automatically normalized for more accurate results
ms = most_similar(demodata, ~ king - man + woman)
#> ! Results may be inaccurate if word vectors are not normalized.
#> ✔ All word vectors now have been automatically normalized.
#> [Word Vector] =~ king - man + woman
#> (normalized to unit length)
ms
#>         word   cos_sim row_id
#>       <char>     <num>  <int>
#>  1:    queen 0.7118192     65
#>  2:    royal 0.4938203    754
#>  3:    Queen 0.4346379   1078
#>  4:     King 0.3749903   1449
#>  5:      she 0.3341126   4267
#>  6:     lady 0.3282869   4631
#>  7:   mother 0.3241257   5174
#>  8:    crown 0.3164823   6754
#>  9:     hers 0.3073009   7852
#> 10: daughter 0.3021213   7981
str(ms)
#> Classes ‘data.table’ and 'data.frame':   10 obs. of  3 variables:
#>  $ word   : chr  "queen" "royal" "Queen" "King" ...
#>  $ cos_sim: num  0.712 0.494 0.435 0.375 0.334 ...
#>  $ row_id : int  65 754 1078 1449 4267 4631 5174 6754 7852 7981
#>  - attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "formula")= chr "king - man + woman"
```
