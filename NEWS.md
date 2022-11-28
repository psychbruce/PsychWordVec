**Please check the [latest news (change log)](https://psychbruce.github.io/PsychWordVec/news/index.html) and keep this package updated.**

# To-Do List

-   [x] Function to expand a dictionary based on cosine similarity
-   [x] Function to evaluate the internal consistency reliability of a dictionary
-   [x] Function to perform the orthogonal Procrustes matrix alignment
-   [ ] Function to extract contextualized word vectors from language models

# PsychWordVec 0.1.4 (ongoing...)

## New Features

-   New `sum_wordvec()` function: Calculate the sum vector of multiple words.
-   New `dict_expand()` function: Expand a dictionary from the most similar words, based on `most_similar()`.
-   New `dict_reliability()` function: Reliability analysis (Cronbach's α) and Principal Component Analysis (PCA) of a dictionary. Note that Cronbach's α may be misleading when the number of items/words is large.
-   New `plot_similarity()` function: Visualize cosine similarities between word pairs in a style of correlation matrix plot.
-   New `tab_similarity_cross()` function: A wrapper of `tab_similarity()` to tabulate cosine similarities for only n1 \* n2 word pairs from two sets of words (arguments: `words1`, `words2`).
-   New `orth_procrustes()` function: Orthogonal Procrustes matrix alignment. Users can input either two matrices of word embeddings or two `wordvec` objects as loaded by `data_wordvec_load()` or transformed from matrices by `as_wordvec()`.

## Minor Changes

-   Improved `print.weat()` and `print.rnd()`.

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
