# PsychWordVec <a href="https://psychbruce.github.io/PsychWordVec"><img src="man/figures/logo.png" align="right" height="138"/></a>

Word Embedding Research Framework for Psychological Science.

An integrated toolbox of word embedding research that provides:

1.  A collection of [pre-trained word vectors](https://psychbruce.github.io/WordVector_RData.pdf) in the `.RData` compressed format;
2.  A variety of functions to process, analyze, and visualize word vectors;
3.  A range of tests to examine conceptual associations, including the *Word Embedding Association Test* (Caliskan et al., 2017) and the *Relative Norm Distance* (Garg et al., 2018), with permutation test of significance;
4.  A set of training methods to locally train word vectors from text corpora, including *Word2Vec* (Mikolov et al., 2013), *GloVe* (Pennington et al., 2014), and *FastText* (Bojanowski et al., 2017).

<!-- badges: start -->

[![CRAN-Version](https://www.r-pkg.org/badges/version/PsychWordVec?color=red)](https://CRAN.R-project.org/package=PsychWordVec) [![GitHub-Version](https://img.shields.io/github/r-package/v/psychbruce/PsychWordVec?label=GitHub&color=orange)](https://github.com/psychbruce/PsychWordVec) [![R-CMD-check](https://github.com/psychbruce/PsychWordVec/workflows/R-CMD-check/badge.svg)](https://github.com/psychbruce/PsychWordVec/actions) [![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/PsychWordVec)](https://CRAN.R-project.org/package=PsychWordVec) [![GitHub-Stars](https://img.shields.io/github/stars/psychbruce/PsychWordVec?style=social)](https://github.com/psychbruce/PsychWordVec/stargazers)

<!-- badges: end -->

<img src="https://s1.ax1x.com/2020/07/28/aAjUJg.jpg" width="120px" height="42px"/>

## Author

Han-Wu-Shuang (Bruce) Bao 包寒吴霜

Email: [baohws\@foxmail.com](mailto:baohws@foxmail.com)

Homepage: [psychbruce.github.io](https://psychbruce.github.io)

## Citation

-   Bao, H.-W.-S. (2022). PsychWordVec: Word embedding research framework for psychological science. R package version 0.1.x. <https://CRAN.R-project.org/package=PsychWordVec>

## Installation

``` r
## Method 1: Install from CRAN
install.packages("PsychWordVec")

## Method 2: Install from GitHub
install.packages("devtools")
devtools::install_github("psychbruce/PsychWordVec", force=TRUE)
```

## Functions

-   Word Embeddings Data Management and Transformation
    -   `data_transform()`
    -   `data_wordvec_load()`
    -   `data_wordvec_normalize()`
    -   `data_wordvec_reshape()`
        -   `as_matrix()`: from `wordvec` to `matrix`
        -   `as_wordvec()`: from `matrix` to `wordvec`
    -   `data_wordvec_subset()`
    -   `orth_procrustes()` (Orthogonal Procrustes matrix alignment)
-   Word Vectors Extraction, Linear Operation, and Visualization
    -   `get_wordvec()`
    -   `get_wordvecs()`
    -   `sum_wordvec()`
    -   `plot_wordvec()`
    -   `plot_wordvec_tSNE()`
-   Word Semantic Similarity Analysis and Conceptual Association Test
    -   `cosine_similarity()`
    -   `pair_similarity()`
    -   `plot_similarity()`
    -   `tab_similarity()`
    -   `most_similar()` (find the Top-N most similar words)
    -   `dict_expand()` (expand a dictionary from the most similar words)
    -   `dict_reliability()` (reliability analysis and PCA of a dictionary)
    -   `test_WEAT()` (with permutation test of significance)
    -   `test_RND()` (with permutation test of significance)
-   Word Vectors Local Training (Word2Vec, GloVe, and FastText)
    -   `tokenize()`
    -   `train_wordvec()`

See the documentation (help pages) for their usage and details.
