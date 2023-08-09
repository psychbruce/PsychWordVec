**Please check the [latest news (change log)](https://psychbruce.github.io/PsychWordVec/news/index.html) and keep this package updated.**

⚠️ *All users should update the package to version ≥ 0.3.2. Old versions may have slow processing speed and other problems.*

# PsychWordVec 2023.8

## Minor Changes

-   Now use "YYYY.M" as package version number.
-   Deprecated `text_unmask()` since I have developed a new package [FMAT](https://psychbruce.github.io/FMAT/) as an integrative toolbox of the *Fill-Mask Association Test* (FMAT).

# PsychWordVec 0.3.2 (Mar 2023)

## Minor Changes

-   Changed welcome messages by using `packageStartupMessage()` so that the messages can be suppressed.
-   Improved `text_unmask()`, but a new package (currently *not* publicly available) has been developed for a more general purpose of using masked language models to measure conceptual associations. Please wait for the release of this new package and the publication of a related methodological article.

## Bug Fixes

-   Fixed problematic `normalized` attribute when using `data_wordvec_load()`.

# PsychWordVec 0.3.0 (Dec 2022)

## New Features

-   New S3 `[` method for `embed`, see new examples in `as_embed()`.
-   New S3 `unique()` method to delete duplicate words.
-   New S3 `str()` method to print the data structure and attributes.
-   New `pattern()` function designed for S3 `[` method of `embed`: Users can directly use regular expression like `embed[pattern("^for")]` to extract a subset of embedding matrix.
-   New `plot_network()` function: Visualize a (partial correlation) network graph of words. Very useful for identifying potential semantic clusters from a list of words and even useful for disentangling antonyms from synonyms.
-   New `targets` argument of `text_unmask()`: Return specific fill-mask results for certain target words (rather than the top *n* results).

## Major Changes

-   Most functions now have been substantially enhanced for a faster speed, especially `tab_similarity()`, `most_similar()`, `dict_expand()`, `dict_reliability()`, `test_WEAT()`, `test_RND()`.
-   Improved S3 `print()` method for `embed` and `wordvec`.
-   `pair_similarity()` has been improved by using matrix operation `tcrossprod(embed, embed)` to compute cosine similarity, with `embed` normalized.
-   `data_wordvec_load()` has got two wrapper functions `load_wordvec()` and `load_embed()` for faster use.
-   `data_wordvec_normalize()` (deprecated) has been renamed to `normalize()`.
-   `get_wordvecs()` (deprecated) has been integrated into `get_wordvec()`.
-   `tab_similarity_cross()` (deprecated) has been integrated into `tab_similarity()`.
-   `test_WEAT()` and `test_RND()`: Warning if `T1` and `T2` or `A1` and `A2` have duplicate values.

## Bug Fixes

-   Fixed the issue of unexpected long loading and processing time in 0.2.0, which was related to duplicate words in .RData, too many words in `embed` or `wordvec`, and too many words to be printed to console. Now all related functions have been substantially improved so that they would not take unnecessarily long time.

# PsychWordVec 0.2.0 (Dec 2022)

## Breaking News

-   Most functions now internally use `embed` (an extended class of matrix) rather than `wordvec` in order to enhance the speed!
-   New series of `text_*` functions for contextualized word embeddings! Based on the R package [`text`](https://www.r-text.org/) (and using the R package `reticulate` to call functions from the Python module `transformers`), a series of new functions have been developed to (1) download [HuggingFace](https://huggingface.co/models) Transformers *pre-trained language models* (PLM; thousands of options such as GPT, BERT, RoBERTa, DeBERTa, DistilBERT, etc.), (2) extract contextualized token (roughly word) embeddings and text embeddings, and (3) fill in the blank mask(s) in a query (e.g., "Beijing is the [MASK] of China.").
    -   `text_init()`: set up a Python environment for PLM
    -   `text_model_download()`: download PLMs from [HuggingFace](https://huggingface.co/models) to local ".cache" folder
    -   `text_model_remove()`: remove PLMs from local ".cache" folder
    -   `text_to_vec()`: extract contextualized token and text embeddings
    -   `text_unmask()`: fill in the blank mask(s) in a query
-   New `orth_procrustes()` function: Orthogonal Procrustes matrix alignment. Users can input either two matrices of word embeddings or two `wordvec` objects as loaded by `data_wordvec_load()` or transformed from matrices by `as_wordvec()`.
-   New `dict_expand()` function: Expand a dictionary from the most similar words, based on `most_similar()`.
-   New `dict_reliability()` function: Reliability analysis (Cronbach's α) and Principal Component Analysis (PCA) of a dictionary. Note that Cronbach's α may be misleading when the number of items/words is large.

## New Features

-   New `sum_wordvec()` function: Calculate the sum vector of multiple words.
-   New `plot_similarity()` function: Visualize cosine similarities between word pairs in a style of correlation matrix plot.
-   New `tab_similarity_cross()` function: A wrapper of `tab_similarity()` to tabulate cosine similarities for only n1 \* n2 word pairs from two sets of words (arguments: `words1`, `words2`).
-   New S3 methods: `print.wordvec()`, `print.embed()`, `rbind.wordvec()`, `rbind.embed()`, `subset.wordvec()`, `subset.embed()`

## Major Changes

-   `as_matrix()` has been renamed to `as_embed()`: Now `PsychWordVec` supports two classes of data objects -- `wordvec` (data.table) and `embed` (matrix). Most functions now use `embed` (or transform `wordvec` to `embed`) internally so as to enhance the speed. Matrix is much faster!
-   Deprecated `data_wordvec_reshape()`: Now use `as_wordvec()` and `as_embed()`.

## Minor Changes

-   Defaults changed in `data_wordvec_subset()`, `get_wordvecs()`, `tab_similarity()`, and `plot_similarity()`: If neither `words` nor `pattern` are specified (`NULL`), then all words in `data` will be extracted.
-   Improved S3 methods `print.weat()` and `print.rnd()`.

# PsychWordVec 0.1.2 (Nov 2022)

## New Features

-   Added permutation test of significance for both `test_WEAT()` and `test_RND()`: Users can specify the number of permutation samples and choose to calculate either one-sided or two-sided *p* value. It can well reproduce the results in Caliskan et al.'s (2017) article.
-   Added the `pooled.sd` argument for `test_WEAT()`: Users can choose the method used to calculate the pooled *SD* for effect size estimate in WEAT. However, the original approach proposed by Caliskan et al. (2017) is the default and highly suggested.
-   Wrapper functions `as_matrix()` and `as_wordvec()` for `data_wordvec_reshape()`, which can make it easier to reshape word embeddings data from `matrix` to "wordvec" `data.table` or vice versa.

## Major Changes

-   Both `test_WEAT()` and `test_RND()` now have changed the element names and S3 print method of their returned objects (of new class `weat` and `rnd`, respectively): The elements `$eff.raw`, `$eff.size`, and `$eff.sum` are now deprecated and replaced by `$eff`, which is a `data.table` containing the overall raw/standardized effects and permutation *p* value. The new S3 print methods `print.weat()` and `print.rnd()` can make a tidy report of the test results when you directly type and print the returned object (see code examples).
-   Improved command line interfaces using the `cli` package.
-   Improved welcome messages when `library(PsychWordVec)`.

# PsychWordVec 0.1.0 (Aug 2022)

-   CRAN initial release.
-   Fixed all issues in the CRAN manual inspection.

# PsychWordVec 0.0.8 (Aug 2022)

## New Features

-   Added `wordvec` as the primary class of word vectors data: Now the data classes contain `wordvec`, `data.table`, and `data.frame`, which actually perform as a `data.table`.
-   New `train_wordvec()` function: Train word vectors using the *Word2Vec*, *GloVe*, or *FastText* algorithm with multi-threading.
-   New `tokenize()` function: Tokenize raw texts for training word vectors.
-   New `data_wordvec_reshape()` function: Reshape word vectors data from dense (a `data.table` of new classs `wordvec` with two variables `word` and `vec`) to plain (a `matrix` of word vectors) or vice versa.
-   New `test_RND()` function, and `tab_WEAT()` is renamed to `test_WEAT()`: These two functions serve as convenient tools of word semantic similarity analysis and conceptual association test.
-   New `plot_wordvec_tSNE()` function: Visualize 2-D or 3-D word vectors with dimensionality reduced using the t-Distributed Stochastic Neighbor Embedding (t-SNE) method.

# PsychWordVec 0.0.6 (Jul 2022)

## New Features

-   Enhanced all functions.
-   New `data_wordvec_subset()` function.
-   Added the `unique` argument for `tab_similarity()`.
-   Added support to use regular expression pattern in `test_WEAT()`.

# PsychWordVec 0.0.4 (Apr 2022)

-   Initial public release on [GitHub](https://github.com/psychbruce/PsychWordVec) with more functions.

# PsychWordVec 0.0.1 (Mar 2022)

-   Basic functions and the [WordVector_RData.pdf](https://psychbruce.github.io/WordVector_RData.pdf) file.
