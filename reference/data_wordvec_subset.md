# \[S3 method\] Extract a subset of word vectors data.

Extract a subset of word vectors data. You may specify either a
`wordvec` or `embed` loaded by
[`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
or an .RData file transformed by
[`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md)).

## Usage

``` r
data_wordvec_subset(
  x,
  words = NULL,
  pattern = NULL,
  as = c("wordvec", "embed"),
  file.save,
  compress = "bzip2",
  compress.level = 9,
  verbose = TRUE
)

# S3 method for class 'wordvec'
subset(x, ...)

# S3 method for class 'embed'
subset(x, ...)
```

## Arguments

- x:

  Can be:

  - a `wordvec` or `embed` loaded by
    [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)

  - an .RData file transformed by
    [`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md)

- words:

  \[Option 1\] Character string(s).

- pattern:

  \[Option 2\] [Regular
  expression](https://stringr.tidyverse.org/reference/str_subset.html).
  If `words` and `pattern` are not specified, all words in the data will
  be extracted.

- as:

  Reshape to
  [`wordvec`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (data.table) or
  [`embed`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (matrix). Defaults to the original class of `x`.

- file.save:

  File name of to-be-saved R data (must be .RData).

- compress:

  Compression method for the saved file. Defaults to `"bzip2"`.

  - `1` or `"gzip"`: modest file size (fastest)

  - `2` or `"bzip2"`: small file size (fast)

  - `3` or `"xz"`: minimized file size (slow)

- compress.level:

  Compression level from `0` (none) to `9` (maximal compression for
  minimal file size). Defaults to `9`.

- verbose:

  Print information to the console? Defaults to `TRUE`.

- ...:

  Arguments passed on to `data_wordvec_subset()` when using the S3
  method `subset`.

## Value

A subset of `wordvec` or `embed` of valid (available) words.

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

[`get_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/get_wordvec.md)

[`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md)

## Examples

``` r
## directly use `embed[i, j]` (3x faster than `wordvec`):
d = as_embed(demodata)
d[1:5]
#> # embed (matrix): [5 × 300] (NOT normalized)
#>            dim1 ...     dim300
#> 1: in    0.0703 ... <300 dims>
#> 2: for  -0.0118 ... <300 dims>
#> 3: that -0.0157 ... <300 dims>
#> 4: is    0.0070 ... <300 dims>
#> 5: on    0.0267 ... <300 dims>
d["people"]
#> # embed (matrix): [1 × 300] (NOT normalized)
#>              dim1 ...     dim300
#> 1: people  0.2637 ... <300 dims>
d[c("China", "Japan", "Korea")]
#> # embed (matrix): [3 × 300] (NOT normalized)
#>             dim1 ...     dim300
#> 1: China -0.0732 ... <300 dims>
#> 2: Japan  0.0508 ... <300 dims>
#> 3: Korea  0.1089 ... <300 dims>

## specify `x` as a `wordvec` or `embed` object:
subset(demodata, c("China", "Japan", "Korea"))
#> # wordvec (data.table): [3 × 2] (NOT normalized)
#>     word                      vec
#> 1: China [-0.0732, ...<300 dims>]
#> 2: Japan [ 0.0508, ...<300 dims>]
#> 3: Korea [ 0.1089, ...<300 dims>]
subset(d, pattern="^Chi")
#> 4 words matched...
#> # embed (matrix): [4 × 300] (NOT normalized)
#>               dim1 ...     dim300
#> 1: China   -0.0732 ... <300 dims>
#> 2: Chicago -0.0786 ... <300 dims>
#> 3: Chinese -0.1367 ... <300 dims>
#> 4: Chile   -0.2754 ... <300 dims>

## specify `x` and `pattern`, and save with `file.save`:
subset(demodata, pattern="Chin[ae]|Japan|Korea",
       file.save="subset.RData")
#> 6 words matched...
#> 
#> Compressing and saving...
#> ✔ Saved to subset.RData (time cost = 0.003 secs)
#> # wordvec (data.table): [6 × 2] (NOT normalized)
#>        word                      vec
#> 1:    China [-0.0732, ...<300 dims>]
#> 2:    Japan [ 0.0508, ...<300 dims>]
#> 3:  Chinese [-0.1367, ...<300 dims>]
#> 4: Japanese [ 0.0105, ...<300 dims>]
#> 5:   Korean [ 0.0898, ...<300 dims>]
#> 6:    Korea [ 0.1089, ...<300 dims>]

## load the subset:
d.subset = load_wordvec("subset.RData")
#> Loading...
#> ✔ Word vectors data: 6 vocab, 300 dims (time cost = 0.002 secs)
#> ✔ All word vectors have been normalized to unit length 1.
d.subset
#> # wordvec (data.table): [6 × 2] (normalized)
#>        word                      vec
#> 1:    China [-0.0270, ...<300 dims>]
#> 2:    Japan [ 0.0183, ...<300 dims>]
#> 3:  Chinese [-0.0499, ...<300 dims>]
#> 4: Japanese [ 0.0037, ...<300 dims>]
#> 5:   Korean [ 0.0264, ...<300 dims>]
#> 6:    Korea [ 0.0338, ...<300 dims>]

## specify `x` as an .RData file and save with `file.save`:
data_wordvec_subset("subset.RData",
                    words=c("China", "Chinese"),
                    file.save="new.subset.RData")
#> Loading...
#> ✔ Word vectors data: 6 vocab, 300 dims (time cost = 0.001 secs)
#> 
#> Compressing and saving...
#> ✔ Saved to new.subset.RData (time cost = 0.002 secs)
#> # wordvec (data.table): [2 × 2] (NOT normalized)
#>       word                      vec
#> 1:   China [-0.0732, ...<300 dims>]
#> 2: Chinese [-0.1367, ...<300 dims>]
d.new.subset = load_embed("new.subset.RData")
#> Loading...
#> ✔ Word vectors data: 2 vocab, 300 dims (time cost = 0.001 secs)
#> ✔ All word vectors have been normalized to unit length 1.
d.new.subset
#> # embed (matrix): [2 × 300] (normalized)
#>               dim1 ...     dim300
#> 1: China   -0.0270 ... <300 dims>
#> 2: Chinese -0.0499 ... <300 dims>

unlink("subset.RData")  # delete file for code check
unlink("new.subset.RData")  # delete file for code check
```
