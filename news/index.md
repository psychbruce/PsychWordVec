# Changelog

## PsychWordVec 2025.11

CRAN release: 2025-11-30

- Moved CRAN packages `text2vec`, `word2vec`, `rsparse`, `fastTextR`
  from Imports to Suggests.

## PsychWordVec 2025.8

CRAN release: 2025-08-19

- Refined help pages in the style of Roxygen markdown.

## PsychWordVec 2025.3

CRAN release: 2025-03-30

- Deprecated all `text_*()` functions.
- Other tiny changes.

## PsychWordVec 2023.9

CRAN release: 2023-09-27

### Minor Changes

- Use `\donttest{}` in more examples to avoid unnecessary errors.
- Improved `text_unmask()`, though it has been deprecated.

## PsychWordVec 2023.8

CRAN release: 2023-08-08

### Minor Changes

- Now use “YYYY.M” as package version number.
- Deprecated `text_unmask()` since I have developed a new package
  [FMAT](https://psychbruce.github.io/FMAT/) as an integrative toolbox
  of the *Fill-Mask Association Test* (FMAT).

## PsychWordVec 0.3.2 (Mar 2023)

CRAN release: 2023-03-04

### Minor Changes

- Changed welcome messages by using
  [`packageStartupMessage()`](https://rdrr.io/r/base/message.html) so
  that the messages can be suppressed.
- Improved `text_unmask()`, but a new package (currently *not* publicly
  available) has been developed for a more general purpose of using
  masked language models to measure conceptual associations. Please wait
  for the release of this new package and the publication of a related
  methodological article.

### Bug Fixes

- Fixed problematic `normalized` attribute when using
  [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md).

## PsychWordVec 0.3.0 (Dec 2022)

CRAN release: 2022-12-15

### New Features

- New S3 `[` method for `embed`, see new examples in
  [`as_embed()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md).
- New S3 [`unique()`](https://rdrr.io/r/base/unique.html) method to
  delete duplicate words.
- New S3 [`str()`](https://rdrr.io/r/utils/str.html) method to print the
  data structure and attributes.
- New
  [`pattern()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  function designed for S3 `[` method of `embed`: Users can directly use
  regular expression like `embed[pattern("^for")]` to extract a subset
  of embedding matrix.
- New
  [`plot_network()`](https://psychbruce.github.io/PsychWordVec/reference/plot_network.md)
  function: Visualize a (partial correlation) network graph of words.
  Very useful for identifying potential semantic clusters from a list of
  words and even useful for disentangling antonyms from synonyms.
- New `targets` argument of `text_unmask()`: Return specific fill-mask
  results for certain target words (rather than the top *n* results).

### Major Changes

- Most functions now have been substantially enhanced for a faster
  speed, especially
  [`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md),
  [`most_similar()`](https://psychbruce.github.io/PsychWordVec/reference/most_similar.md),
  [`dict_expand()`](https://psychbruce.github.io/PsychWordVec/reference/dict_expand.md),
  [`dict_reliability()`](https://psychbruce.github.io/PsychWordVec/reference/dict_reliability.md),
  [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md),
  [`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md).
- Improved S3 [`print()`](https://rdrr.io/r/base/print.html) method for
  `embed` and `wordvec`.
- [`pair_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/pair_similarity.md)
  has been improved by using matrix operation `tcrossprod(embed, embed)`
  to compute cosine similarity, with `embed` normalized.
- [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
  has got two wrapper functions
  [`load_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
  and
  [`load_embed()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
  for faster use.
- `data_wordvec_normalize()` (deprecated) has been renamed to
  [`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md).
- `get_wordvecs()` (deprecated) has been integrated into
  [`get_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/get_wordvec.md).
- `tab_similarity_cross()` (deprecated) has been integrated into
  [`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md).
- [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md)
  and
  [`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md):
  Warning if `T1` and `T2` or `A1` and `A2` have duplicate values.

### Bug Fixes

- Fixed the issue of unexpected long loading and processing time in
  0.2.0, which was related to duplicate words in .RData, too many words
  in `embed` or `wordvec`, and too many words to be printed to console.
  Now all related functions have been substantially improved so that
  they would not take unnecessarily long time.

## PsychWordVec 0.2.0 (Dec 2022)

CRAN release: 2022-12-01

### Breaking News

- Most functions now internally use `embed` (an extended class of
  matrix) rather than `wordvec` in order to enhance the speed!
- New series of `text_*` functions for contextualized word embeddings!
  Based on the R package [`text`](https://www.r-text.org/) (and using
  the R package `reticulate` to call functions from the Python module
  `transformers`), a series of new functions have been developed to (1)
  download [HuggingFace](https://huggingface.co/models) Transformers
  *pre-trained language models* (PLM; thousands of options such as GPT,
  BERT, RoBERTa, DeBERTa, DistilBERT, etc.), (2) extract contextualized
  token (roughly word) embeddings and text embeddings, and (3) fill in
  the blank mask(s) in a query (e.g., “Beijing is the \[MASK\] of
  China.”).
  - `text_init()`: set up a Python environment for PLM
  - `text_model_download()`: download PLMs from
    [HuggingFace](https://huggingface.co/models) to local “.cache”
    folder
  - `text_model_remove()`: remove PLMs from local “.cache” folder
  - `text_to_vec()`: extract contextualized token and text embeddings
  - `text_unmask()`: fill in the blank mask(s) in a query
- New
  [`orth_procrustes()`](https://psychbruce.github.io/PsychWordVec/reference/orth_procrustes.md)
  function: Orthogonal Procrustes matrix alignment. Users can input
  either two matrices of word embeddings or two `wordvec` objects as
  loaded by
  [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
  or transformed from matrices by
  [`as_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md).
- New
  [`dict_expand()`](https://psychbruce.github.io/PsychWordVec/reference/dict_expand.md)
  function: Expand a dictionary from the most similar words, based on
  [`most_similar()`](https://psychbruce.github.io/PsychWordVec/reference/most_similar.md).
- New
  [`dict_reliability()`](https://psychbruce.github.io/PsychWordVec/reference/dict_reliability.md)
  function: Reliability analysis (Cronbach’s α) and Principal Component
  Analysis (PCA) of a dictionary. Note that Cronbach’s α may be
  misleading when the number of items/words is large.

### New Features

- New
  [`sum_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/sum_wordvec.md)
  function: Calculate the sum vector of multiple words.
- New
  [`plot_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/plot_similarity.md)
  function: Visualize cosine similarities between word pairs in a style
  of correlation matrix plot.
- New `tab_similarity_cross()` function: A wrapper of
  [`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md)
  to tabulate cosine similarities for only n1 \* n2 word pairs from two
  sets of words (arguments: `words1`, `words2`).
- New S3 methods: `print.wordvec()`, `print.embed()`, `rbind.wordvec()`,
  `rbind.embed()`,
  [`subset.wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md),
  [`subset.embed()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md)

### Major Changes

- `as_matrix()` has been renamed to
  [`as_embed()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md):
  Now `PsychWordVec` supports two classes of data objects – `wordvec`
  (data.table) and `embed` (matrix). Most functions now use `embed` (or
  transform `wordvec` to `embed`) internally so as to enhance the speed.
  Matrix is much faster!
- Deprecated `data_wordvec_reshape()`: Now use
  [`as_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  and
  [`as_embed()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md).

### Minor Changes

- Defaults changed in
  [`data_wordvec_subset()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md),
  `get_wordvecs()`,
  [`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md),
  and
  [`plot_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/plot_similarity.md):
  If neither `words` nor `pattern` are specified (`NULL`), then all
  words in `data` will be extracted.
- Improved S3 methods `print.weat()` and `print.rnd()`.

## PsychWordVec 0.1.2 (Nov 2022)

CRAN release: 2022-11-03

### New Features

- Added permutation test of significance for both
  [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md)
  and
  [`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md):
  Users can specify the number of permutation samples and choose to
  calculate either one-sided or two-sided *p* value. It can well
  reproduce the results in Caliskan et al.’s (2017) article.
- Added the `pooled.sd` argument for
  [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md):
  Users can choose the method used to calculate the pooled *SD* for
  effect size estimate in WEAT. However, the original approach proposed
  by Caliskan et al. (2017) is the default and highly suggested.
- Wrapper functions `as_matrix()` and
  [`as_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  for `data_wordvec_reshape()`, which can make it easier to reshape word
  embeddings data from `matrix` to “wordvec” `data.table` or vice versa.

### Major Changes

- Both
  [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md)
  and
  [`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md)
  now have changed the element names and S3 print method of their
  returned objects (of new class `weat` and `rnd`, respectively): The
  elements `$eff.raw`, `$eff.size`, and `$eff.sum` are now deprecated
  and replaced by `$eff`, which is a `data.table` containing the overall
  raw/standardized effects and permutation *p* value. The new S3 print
  methods `print.weat()` and `print.rnd()` can make a tidy report of the
  test results when you directly type and print the returned object (see
  code examples).
- Improved command line interfaces using the `cli` package.
- Improved welcome messages when
  [`library(PsychWordVec)`](https://psychbruce.github.io/PsychWordVec/).

## PsychWordVec 0.1.0 (Aug 2022)

CRAN release: 2022-08-22

- CRAN initial release.
- Fixed all issues in the CRAN manual inspection.

## PsychWordVec 0.0.8 (Aug 2022)

### New Features

- Added `wordvec` as the primary class of word vectors data: Now the
  data classes contain `wordvec`, `data.table`, and `data.frame`, which
  actually perform as a `data.table`.
- New
  [`train_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/train_wordvec.md)
  function: Train word vectors using the *Word2Vec*, *GloVe*, or
  *FastText* algorithm with multi-threading.
- New
  [`tokenize()`](https://psychbruce.github.io/PsychWordVec/reference/tokenize.md)
  function: Tokenize raw texts for training word vectors.
- New `data_wordvec_reshape()` function: Reshape word vectors data from
  dense (a `data.table` of new classs `wordvec` with two variables
  `word` and `vec`) to plain (a `matrix` of word vectors) or vice versa.
- New
  [`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md)
  function, and `tab_WEAT()` is renamed to
  [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md):
  These two functions serve as convenient tools of word semantic
  similarity analysis and conceptual association test.
- New
  [`plot_wordvec_tSNE()`](https://psychbruce.github.io/PsychWordVec/reference/plot_wordvec_tSNE.md)
  function: Visualize 2-D or 3-D word vectors with dimensionality
  reduced using the t-Distributed Stochastic Neighbor Embedding (t-SNE)
  method.

## PsychWordVec 0.0.6 (Jul 2022)

### New Features

- Enhanced all functions.
- New
  [`data_wordvec_subset()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md)
  function.
- Added the `unique` argument for
  [`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md).
- Added support to use regular expression pattern in
  [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md).

## PsychWordVec 0.0.4 (Apr 2022)

- Initial public release on
  [GitHub](https://github.com/psychbruce/PsychWordVec) with more
  functions.

## PsychWordVec 0.0.1 (Mar 2022)

- Basic functions and the
  [WordVector_RData.pdf](https://psychbruce.github.io/WordVector_RData.pdf)
  file.
