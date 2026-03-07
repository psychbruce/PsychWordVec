# Demo data (pre-trained using word2vec on Google News; 8000 vocab, 300 dims).

This demo data contains a sample of 8000 English words with
300-dimension word vectors pre-trained using the "word2vec" algorithm
based on the Google News corpus. Most of these words are from the Top
8000 frequent wordlist, whereas a few are selected from less frequent
words and appended.

## Usage

``` r
data(demodata)
```

## Format

A `data.table` (class `wordvec`) with two variables `word` and `vec`,
transformed from the raw data (see the URL in Source) into `.RData`
using
[`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md).

## Source

Google Code - word2vec (<https://code.google.com/archive/p/word2vec/>)

## Examples

``` r
class(demodata)
#> [1] "wordvec"    "data.table" "data.frame"
demodata
#> # wordvec (data.table): [8000 × 2] (NOT normalized)
#>                  word                      vec
#>    1:              in [ 0.0703, ...<300 dims>]
#>    2:             for [-0.0118, ...<300 dims>]
#>    3:            that [-0.0157, ...<300 dims>]
#>    4:              is [ 0.0070, ...<300 dims>]
#>    5:              on [ 0.0267, ...<300 dims>]
#> -----                                         
#> 7996:     salesperson [ 0.1245, ...<300 dims>]
#> 7997:     computation [ 0.0791, ...<300 dims>]
#> 7998:   psychotherapy [ 0.1445, ...<300 dims>]
#> 7999:       equations [ 0.3242, ...<300 dims>]
#> 8000: psychotherapist [ 0.1357, ...<300 dims>]

embed = as_embed(demodata, normalize=TRUE)
class(embed)
#> [1] "embed"  "matrix" "array" 
embed
#> # embed (matrix): [8000 × 300] (normalized)
#>                          dim1 ...     dim300
#>    1: in               0.0530 ... <300 dims>
#>    2: for             -0.0085 ... <300 dims>
#>    3: that            -0.0124 ... <300 dims>
#>    4: is               0.0037 ... <300 dims>
#>    5: on               0.0167 ... <300 dims>
#> -----                                       
#> 7996: salesperson      0.0381 ... <300 dims>
#> 7997: computation      0.0227 ... <300 dims>
#> 7998: psychotherapy    0.0405 ... <300 dims>
#> 7999: equations        0.0957 ... <300 dims>
#> 8000: psychotherapist  0.0364 ... <300 dims>
```
