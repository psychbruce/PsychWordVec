# Word vectors data class: `wordvec` and `embed`.

`PsychWordVec` uses two types of word vectors data: `wordvec`
(data.table, with two variables `word` and `vec`) and `embed` (matrix,
with dimensions as columns and words as row names). Note that matrix
operation makes `embed` much faster than `wordvec`. Users are suggested
to reshape data to `embed` before using the other functions.

## Usage

``` r
as_embed(x, normalize = FALSE)

as_wordvec(x, normalize = FALSE)

# S3 method for class 'embed'
x[i, j]

pattern(pattern)
```

## Arguments

- x:

  Object to be reshaped. See examples.

- normalize:

  Normalize all word vectors to unit length? Defaults to `FALSE`. See
  [`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md).

- i, j:

  Row (`i`) and column (`j`) filter to be used in `embed[i, j]`.

- pattern:

  Regular expression to be used in `embed[pattern("...")]`.

## Value

A `wordvec` (data.table) or `embed` (matrix).

## Functions

- `as_embed()`: From `wordvec` (data.table) to `embed` (matrix).

- `as_wordvec()`: From `embed` (matrix) to `wordvec` (data.table).

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## See also

[`load_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
/
[`load_embed()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)

[`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md)

[`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md)

[`data_wordvec_subset()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md)

## Examples

``` r
dt = head(demodata, 10)
str(dt)
#> Class "wordvec" [10 × 2] (inherits: "data.table")
#> $ word : "in" "for" "that" "is" "on" ...
#> $ vec : list of 10
#> - attr(*, "dims") = 300
#> - attr(*, "normalized") = FALSE

embed = as_embed(dt, normalize=TRUE)
embed
#> # embed (matrix): [10 × 300] (normalized)
#>             dim1 ...     dim300
#>  1: in    0.0530 ... <300 dims>
#>  2: for  -0.0085 ... <300 dims>
#>  3: that -0.0124 ... <300 dims>
#>  4: is    0.0037 ... <300 dims>
#>  5: on    0.0167 ... <300 dims>
#>  6: with -0.0160 ... <300 dims>
#>  7: said -0.0045 ... <300 dims>
#>  8: was   0.0126 ... <300 dims>
#>  9: the   0.0747 ... <300 dims>
#> 10: at   -0.0354 ... <300 dims>
str(embed)
#> Class "embed" [10 × 300] (inherits: "matrix")
#> - rownames(*) : "in" "for" "that" "is" "on" ...
#> - colnames(*) : "dim1" "dim2" "dim3" "dim4" "dim5" ...
#> - attr(*, "dims") = 300
#> - attr(*, "normalized") = TRUE

wordvec = as_wordvec(embed, normalize=TRUE)
wordvec
#> # wordvec (data.table): [10 × 2] (normalized)
#>     word                      vec
#>  1:   in [ 0.0530, ...<300 dims>]
#>  2:  for [-0.0085, ...<300 dims>]
#>  3: that [-0.0124, ...<300 dims>]
#>  4:   is [ 0.0037, ...<300 dims>]
#>  5:   on [ 0.0167, ...<300 dims>]
#>  6: with [-0.0160, ...<300 dims>]
#>  7: said [-0.0045, ...<300 dims>]
#>  8:  was [ 0.0126, ...<300 dims>]
#>  9:  the [ 0.0747, ...<300 dims>]
#> 10:   at [-0.0354, ...<300 dims>]
str(wordvec)
#> Class "wordvec" [10 × 2] (inherits: "data.table")
#> $ word : "in" "for" "that" "is" "on" ...
#> $ vec : list of 10
#> - attr(*, "dims") = 300
#> - attr(*, "normalized") = TRUE

df = data.frame(token=LETTERS, D1=1:26/10000, D2=26:1/10000)
as_embed(df)
#> # embed (matrix): [26 × 2] (NOT normalized)
#>          dim1 ...     dim2
#>  1: A  0.0001 ... <2 dims>
#>  2: B  0.0002 ... <2 dims>
#>  3: C  0.0003 ... <2 dims>
#>  4: D  0.0004 ... <2 dims>
#>  5: E  0.0005 ... <2 dims>
#>  6: F  0.0006 ... <2 dims>
#>  7: G  0.0007 ... <2 dims>
#>  8: H  0.0008 ... <2 dims>
#>  9: I  0.0009 ... <2 dims>
#> 10: J  0.0010 ... <2 dims>
#> 11: K  0.0011 ... <2 dims>
#> 12: L  0.0012 ... <2 dims>
#> 13: M  0.0013 ... <2 dims>
#> 14: N  0.0014 ... <2 dims>
#> 15: O  0.0015 ... <2 dims>
#> 16: P  0.0016 ... <2 dims>
#> 17: Q  0.0017 ... <2 dims>
#> 18: R  0.0018 ... <2 dims>
#> 19: S  0.0019 ... <2 dims>
#> 20: T  0.0020 ... <2 dims>
#> 21: U  0.0021 ... <2 dims>
#> 22: V  0.0022 ... <2 dims>
#> 23: W  0.0023 ... <2 dims>
#> 24: X  0.0024 ... <2 dims>
#> 25: Y  0.0025 ... <2 dims>
#> 26: Z  0.0026 ... <2 dims>
as_wordvec(df)
#> # wordvec (data.table): [26 × 2] (NOT normalized)
#>     word                    vec
#>  1:    A [ 0.0001, ...<2 dims>]
#>  2:    B [ 0.0002, ...<2 dims>]
#>  3:    C [ 0.0003, ...<2 dims>]
#>  4:    D [ 0.0004, ...<2 dims>]
#>  5:    E [ 0.0005, ...<2 dims>]
#>  6:    F [ 0.0006, ...<2 dims>]
#>  7:    G [ 0.0007, ...<2 dims>]
#>  8:    H [ 0.0008, ...<2 dims>]
#>  9:    I [ 0.0009, ...<2 dims>]
#> 10:    J [ 0.0010, ...<2 dims>]
#> 11:    K [ 0.0011, ...<2 dims>]
#> 12:    L [ 0.0012, ...<2 dims>]
#> 13:    M [ 0.0013, ...<2 dims>]
#> 14:    N [ 0.0014, ...<2 dims>]
#> 15:    O [ 0.0015, ...<2 dims>]
#> 16:    P [ 0.0016, ...<2 dims>]
#> 17:    Q [ 0.0017, ...<2 dims>]
#> 18:    R [ 0.0018, ...<2 dims>]
#> 19:    S [ 0.0019, ...<2 dims>]
#> 20:    T [ 0.0020, ...<2 dims>]
#> 21:    U [ 0.0021, ...<2 dims>]
#> 22:    V [ 0.0022, ...<2 dims>]
#> 23:    W [ 0.0023, ...<2 dims>]
#> 24:    X [ 0.0024, ...<2 dims>]
#> 25:    Y [ 0.0025, ...<2 dims>]
#> 26:    Z [ 0.0026, ...<2 dims>]

dd = rbind(dt[1:5], dt[1:5])
dd  # duplicate words
#> # wordvec (data.table): [10 × 2] (NOT normalized)
#>     word                      vec
#>  1:   in [ 0.0703, ...<300 dims>]
#>  2:  for [-0.0118, ...<300 dims>]
#>  3: that [-0.0157, ...<300 dims>]
#>  4:   is [ 0.0070, ...<300 dims>]
#>  5:   on [ 0.0267, ...<300 dims>]
#>  6:   in [ 0.0703, ...<300 dims>]
#>  7:  for [-0.0118, ...<300 dims>]
#>  8: that [-0.0157, ...<300 dims>]
#>  9:   is [ 0.0070, ...<300 dims>]
#> 10:   on [ 0.0267, ...<300 dims>]
#> ! 5 duplicate words: use `unique()` to delete duplicates
unique(dd)
#> # wordvec (data.table): [5 × 2] (NOT normalized)
#>    word                      vec
#> 1:   in [ 0.0703, ...<300 dims>]
#> 2:  for [-0.0118, ...<300 dims>]
#> 3: that [-0.0157, ...<300 dims>]
#> 4:   is [ 0.0070, ...<300 dims>]
#> 5:   on [ 0.0267, ...<300 dims>]

dm = as_embed(dd)
dm  # duplicate words
#> # embed (matrix): [10 × 300] (NOT normalized)
#>             dim1 ...     dim300
#>  1: in    0.0703 ... <300 dims>
#>  2: for  -0.0118 ... <300 dims>
#>  3: that -0.0157 ... <300 dims>
#>  4: is    0.0070 ... <300 dims>
#>  5: on    0.0267 ... <300 dims>
#>  6: in    0.0703 ... <300 dims>
#>  7: for  -0.0118 ... <300 dims>
#>  8: that -0.0157 ... <300 dims>
#>  9: is    0.0070 ... <300 dims>
#> 10: on    0.0267 ... <300 dims>
#> ! 5 duplicate words: use `unique()` to delete duplicates
unique(dm)
#> # embed (matrix): [5 × 300] (NOT normalized)
#>            dim1 ...     dim300
#> 1: in    0.0703 ... <300 dims>
#> 2: for  -0.0118 ... <300 dims>
#> 3: that -0.0157 ... <300 dims>
#> 4: is    0.0070 ... <300 dims>
#> 5: on    0.0267 ... <300 dims>

# more examples for extracting a subset using `x[i, j]`
# (3x faster than `wordvec`)
embed = as_embed(demodata)
embed[1]
#> # embed (matrix): [1 × 300] (NOT normalized)
#>          dim1 ...     dim300
#> 1: in  0.0703 ... <300 dims>
embed[1:5]
#> # embed (matrix): [5 × 300] (NOT normalized)
#>            dim1 ...     dim300
#> 1: in    0.0703 ... <300 dims>
#> 2: for  -0.0118 ... <300 dims>
#> 3: that -0.0157 ... <300 dims>
#> 4: is    0.0070 ... <300 dims>
#> 5: on    0.0267 ... <300 dims>
embed["for"]
#> # embed (matrix): [1 × 300] (NOT normalized)
#>           dim1 ...     dim300
#> 1: for -0.0118 ... <300 dims>
embed[pattern("^for.{0,2}$")]
#> # embed (matrix): [6 × 300] (NOT normalized)
#>             dim1 ...     dim300
#> 1: for   -0.0118 ... <300 dims>
#> 2: form   0.0835 ... <300 dims>
#> 3: force  0.1641 ... <300 dims>
#> 4: forms -0.0206 ... <300 dims>
#> 5: forum -0.1309 ... <300 dims>
#> 6: forth -0.1074 ... <300 dims>
embed[cc("for, in, on, xxx")]
#> ✖ 1 word not found: "xxx"
#> # embed (matrix): [3 × 300] (NOT normalized)
#>           dim1 ...     dim300
#> 1: for -0.0118 ... <300 dims>
#> 2: in   0.0703 ... <300 dims>
#> 3: on   0.0267 ... <300 dims>
embed[cc("for, in, on, xxx"), 5:10]
#> ✖ 1 word not found: "xxx"
#> # embed (matrix): [3 × 6] (NOT normalized)
#>           dim5 ...    dim10
#> 1: for -0.0182 ... <6 dims>
#> 2: in   0.0693 ... <6 dims>
#> 3: on   0.0062 ... <6 dims>
embed[1:5, 5:10]
#> # embed (matrix): [5 × 6] (NOT normalized)
#>            dim5 ...    dim10
#> 1: in    0.0693 ... <6 dims>
#> 2: for  -0.0182 ... <6 dims>
#> 3: that -0.1104 ... <6 dims>
#> 4: is   -0.1328 ... <6 dims>
#> 5: on    0.0062 ... <6 dims>
embed[, 5:10]
#> # embed (matrix): [8000 × 6] (NOT normalized)
#>                          dim5 ...    dim10
#>    1: in               0.0693 ... <6 dims>
#>    2: for             -0.0182 ... <6 dims>
#>    3: that            -0.1104 ... <6 dims>
#>    4: is              -0.1328 ... <6 dims>
#>    5: on               0.0062 ... <6 dims>
#> -----                                     
#> 7996: salesperson      0.0172 ... <6 dims>
#> 7997: computation      0.1465 ... <6 dims>
#> 7998: psychotherapy    0.2539 ... <6 dims>
#> 7999: equations       -0.4082 ... <6 dims>
#> 8000: psychotherapist  0.0198 ... <6 dims>
embed[3, 4]
#> that [dim4] 
#>    0.050293 
embed["that", 4]
#> that [dim4] 
#>    0.050293 
```
