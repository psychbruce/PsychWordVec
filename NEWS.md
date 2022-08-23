**Please check the [latest news (change log)](https://psychbruce.github.io/PsychWordVec/news/index.html) and keep this package updated.**

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
