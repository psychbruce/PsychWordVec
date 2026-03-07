# Load word vectors data (`wordvec` or `embed`) from ".RData" file.

Load word vectors data (`wordvec` or `embed`) from ".RData" file.

## Usage

``` r
data_wordvec_load(
  file,
  as = c("wordvec", "embed"),
  normalize = FALSE,
  verbose = TRUE
)

load_wordvec(file, normalize = TRUE)

load_embed(file, normalize = TRUE)
```

## Arguments

- file:

  File name of .RData transformed by
  [`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md).
  Can also be an .RData file containing an embedding matrix with words
  as row names.

- as:

  Load as
  [`wordvec`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (data.table) or
  [`embed`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (matrix). Defaults to the original class of the R object in `file`.
  The two wrapper functions `load_wordvec()` and `load_embed()`
  automatically reshape the data to the corresponding class and
  normalize all word vectors (for faster future use).

- normalize:

  Normalize all word vectors to unit length? Defaults to `FALSE`. See
  [`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md).

- verbose:

  Print information to the console? Defaults to `TRUE`.

## Value

A `wordvec` (data.table) or `embed` (matrix).

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## See also

[`as_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
/
[`as_embed()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)

[`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md)

[`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md)

[`data_wordvec_subset()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md)

## Examples

``` r
d = demodata[1:200]
save(d, file="demo.RData")
d = load_wordvec("demo.RData")
#> Loading...
#> ✔ Word vectors data: 200 vocab, 300 dims (time cost = 0.004 secs)
#> ✔ All word vectors have been normalized to unit length 1.
d
#> # wordvec (data.table): [200 × 2] (normalized)
#>        word                      vec
#>   1:     in [ 0.0530, ...<300 dims>]
#>   2:    for [-0.0085, ...<300 dims>]
#>   3:   that [-0.0124, ...<300 dims>]
#>   4:     is [ 0.0037, ...<300 dims>]
#>   5:     on [ 0.0167, ...<300 dims>]
#> ----                                
#> 196: really [ 0.0440, ...<300 dims>]
#> 197:  found [-0.0144, ...<300 dims>]
#> 198:   used [ 0.0964, ...<300 dims>]
#> 199:    lot [ 0.0678, ...<300 dims>]
#> 200:  money [ 0.0644, ...<300 dims>]
d = load_embed("demo.RData")
#> Loading...
#> ✔ Word vectors data: 200 vocab, 300 dims (time cost = 0.004 secs)
#> ✔ All word vectors have been normalized to unit length 1.
d
#> # embed (matrix): [200 × 300] (normalized)
#>                dim1 ...     dim300
#>   1: in      0.0530 ... <300 dims>
#>   2: for    -0.0085 ... <300 dims>
#>   3: that   -0.0124 ... <300 dims>
#>   4: is      0.0037 ... <300 dims>
#>   5: on      0.0167 ... <300 dims>
#> ----                              
#> 196: really  0.0440 ... <300 dims>
#> 197: found  -0.0144 ... <300 dims>
#> 198: used    0.0964 ... <300 dims>
#> 199: lot     0.0678 ... <300 dims>
#> 200: money   0.0644 ... <300 dims>
unlink("demo.RData")  # delete file for code check

if (FALSE) { # \dontrun{
# please first manually download the .RData file
# (see https://psychbruce.github.io/WordVector_RData.pdf)
# or transform plain text data by using `data_transform()`

# the RData file must be on your disk
# the following code cannot run unless you have the file
library(bruceR)
set.wd()
d = load_embed("../data-raw/GloVe/glove_wiki_50d.RData")
d
} # }
```
