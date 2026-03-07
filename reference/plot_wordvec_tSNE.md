# Visualize word vectors with dimensionality reduced using t-SNE.

Visualize word vectors with dimensionality reduced using the
t-Distributed Stochastic Neighbor Embedding (t-SNE) method (i.e.,
projecting high-dimensional vectors into a low-dimensional vector
space), implemented by
[`Rtsne::Rtsne()`](https://rdrr.io/pkg/Rtsne/man/Rtsne.html). You should
specify a random seed if you expect reproducible results.

## Usage

``` r
plot_wordvec_tSNE(
  x,
  dims = 2,
  perplexity,
  theta = 0.5,
  colors = NULL,
  seed = NULL,
  custom.Rtsne = NULL
)
```

## Arguments

- x:

  Can be:

  - a `data.table` returned from
    [`get_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/get_wordvec.md)

  - a
    [`wordvec`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
    (data.table) or
    [`embed`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
    (matrix) returned from
    [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)

- dims:

  Output dimensionality: `2` (default, the most common choice) or `3`.

- perplexity:

  Perplexity parameter, should not be larger than (number of words - 1)
  / 3. Defaults to `floor((length(dt)-1)/3)` (where columns of `dt` are
  words). See
  [`Rtsne::Rtsne()`](https://rdrr.io/pkg/Rtsne/man/Rtsne.html) for
  details.

- theta:

  Speed/accuracy trade-off (increase for less accuracy), set to 0 for
  exact t-SNE. Defaults to `0.5`.

- colors:

  A character vector specifying (1) the categories of words (for 2-D
  plot only) or (2) the exact colors of words (for 2-D and 3-D plot).
  See examples for its usage.

- seed:

  Random seed for reproducible results. Defaults to `NULL`.

- custom.Rtsne:

  User-defined [`Rtsne`](https://rdrr.io/pkg/Rtsne/man/Rtsne.html)
  object using the same `dt`.

## Value

2-D: A `ggplot` object. You may extract the data from this object using
`$data`.

3-D: Nothing but only the data was invisibly returned, because
[`rgl::plot3d()`](https://dmurdoch.github.io/rgl/dev/reference/plot3d.html)
is "called for the side effect of drawing the plot" and thus cannot
return any 3-D plot object.

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## References

Hinton, G. E., & Salakhutdinov, R. R. (2006). Reducing the
dimensionality of data with neural networks. *Science, 313*(5786),
504–507.

van der Maaten, L., & Hinton, G. (2008). Visualizing data using t-SNE.
*Journal of Machine Learning Research, 9*, 2579–2605.

## See also

[`plot_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/plot_wordvec.md)

[`plot_network()`](https://psychbruce.github.io/PsychWordVec/reference/plot_network.md)

## Examples

``` r
d = as_embed(demodata, normalize=TRUE)

dt = get_wordvec(d, cc("
  man, woman,
  king, queen,
  China, Beijing,
  Japan, Tokyo"))

## 2-D (default):
plot_wordvec_tSNE(dt, seed=1234)


plot_wordvec_tSNE(dt, seed=1234)$data
#>      word        V1        V2
#> 1     man -209.0042 -38.98062
#> 2   woman -177.8955 -44.15574
#> 3    king -127.9884  73.09223
#> 4   queen -120.7618  42.35201
#> 5   China  130.2159  16.86578
#> 6 Beijing  152.3808  41.24428
#> 7   Japan  159.8935 -46.85079
#> 8   Tokyo  193.1596 -43.56715

colors = c(rep("#2B579A", 4), rep("#B7472A", 4))
plot_wordvec_tSNE(dt, colors=colors, seed=1234)


category = c(rep("gender", 4), rep("country", 4))
plot_wordvec_tSNE(dt, colors=category, seed=1234) +
  scale_x_continuous(limits=c(-200, 200),
                     labels=function(x) x/100) +
  scale_y_continuous(limits=c(-200, 200),
                     labels=function(x) x/100) +
  scale_color_manual(values=c("#B7472A", "#2B579A"))
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text_repel()`).


## 3-D:
colors = c(rep("#2B579A", 4), rep("#B7472A", 4))
plot_wordvec_tSNE(dt, dims=3, colors=colors, seed=1)
#> Warning: RGL: unable to open X11 display
#> Warning: 'rgl.init' failed, will use the null device.
#> See '?rgl.useNULL' for ways to avoid this warning.
```
