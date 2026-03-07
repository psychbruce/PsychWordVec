# Cosine similarity/distance between two vectors.

Cosine similarity/distance between two vectors.

## Usage

``` r
cosine_similarity(v1, v2, distance = FALSE)

cos_sim(v1, v2)

cos_dist(v1, v2)
```

## Arguments

- v1, v2:

  Numeric vector (of the same length).

- distance:

  Compute cosine distance instead? Defaults to `FALSE` (cosine
  similarity).

## Value

A value of cosine similarity/distance.

## Details

Cosine similarity =

`sum(v1 * v2) / ( sqrt(sum(v1^2)) * sqrt(sum(v2^2)) )`

Cosine distance =

`1 - cosine_similarity(v1, v2)`

## See also

[`pair_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/pair_similarity.md)

[`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md)

[`most_similar()`](https://psychbruce.github.io/PsychWordVec/reference/most_similar.md)

## Examples

``` r
cos_sim(v1=c(1,1,1), v2=c(2,2,2))  # 1
#> [1] 1
cos_sim(v1=c(1,4,1), v2=c(4,1,1))  # 0.5
#> [1] 0.5
cos_sim(v1=c(1,1,0), v2=c(0,0,1))  # 0
#> [1] 0

cos_dist(v1=c(1,1,1), v2=c(2,2,2))  # 0
#> [1] -2.220446e-16
cos_dist(v1=c(1,4,1), v2=c(4,1,1))  # 0.5
#> [1] 0.5
cos_dist(v1=c(1,1,0), v2=c(0,0,1))  # 1
#> [1] 1
```
