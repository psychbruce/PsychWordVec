# PsychWordVec <a href="https://psychbruce.github.io/PsychWordVec/"><img src="man/figures/logo.png" align="right" height="138"/></a>

Word Embedding Research Framework for Psychological Science.

An integrated toolbox of word embedding research that provides:

1.  A collection of [pre-trained static word vectors](https://psychbruce.github.io/WordVector_RData.pdf) in the .RData compressed format;
2.  A series of functions to process, analyze, and visualize word vectors;
3.  A range of tests to examine conceptual associations, including the *Word Embedding Association Test* (Caliskan et al., 2017) and the *Relative Norm Distance* (Garg et al., 2018), with permutation test of significance;
4.  A set of training methods to locally train (*static*) word vectors from text corpora, including *Word2Vec* (Mikolov et al., 2013), *GloVe* (Pennington et al., 2014), and *FastText* (Bojanowski et al., 2017);
5.  A group of functions to download pre-trained language models (e.g., *GPT*, *BERT*), extract contextualized (*dynamic*) word vectors (based on the R package [text](https://www.r-text.org/)), and perform language analysis tasks (e.g., fill in the blank masks).

⚠️ *All users should update the package to version ≥ 0.3.2. Old versions may run slowly and have some problems.*

<!-- badges: start -->

[![CRAN-Version](https://www.r-pkg.org/badges/version/PsychWordVec?color=red)](https://CRAN.R-project.org/package=PsychWordVec) [![GitHub-Version](https://img.shields.io/github/r-package/v/psychbruce/PsychWordVec?label=GitHub&color=orange)](https://github.com/psychbruce/PsychWordVec) [![R-CMD-check](https://github.com/psychbruce/PsychWordVec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psychbruce/PsychWordVec/actions/workflows/R-CMD-check.yaml) [![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/PsychWordVec)](https://psychbruce.github.io/PsychWordVec/) [![GitHub-Stars](https://img.shields.io/github/stars/psychbruce/PsychWordVec?style=social)](https://github.com/psychbruce/PsychWordVec/stargazers)

<!-- badges: end -->

<img src="https://s1.ax1x.com/2020/07/28/aAjUJg.jpg" width="120px" height="42px"/>

## Author

Han-Wu-Shuang (Bruce) Bao 包寒吴霜

Email: [baohws\@foxmail.com](mailto:baohws@foxmail.com)

Homepage: [psychbruce.github.io](https://psychbruce.github.io)

## Citation

-   Bao, H.-W.-S. (2023). PsychWordVec: Word embedding research framework for psychological science. R package version 0.3.x. <https://CRAN.R-project.org/package=PsychWordVec>

## Installation

``` r
## Method 1: Install from CRAN
install.packages("PsychWordVec")

## Method 2: Install from GitHub
install.packages("devtools")
devtools::install_github("psychbruce/PsychWordVec", force=TRUE)
```

## Types of Data for `PsychWordVec`

|                  | `embed`                        | `wordvec`                    |
|------------------|----------------------------|--------------------------|
| Basic class      | matrix                         | data.table                   |
| Row size         | vocabulary size                | vocabulary size              |
| Column size      | dimension size                 | 2 (variables: `word`, `vec`) |
| Advantage        | faster (with matrix operation) | easier to inspect and manage |
| Function to get  | `as_embed()`                   | `as_wordvec()`               |
| Function to load | `load_embed()`                 | `load_wordvec()`             |

: Note: Word embedding refers to a natural language processing technique that embeds word semantics into a low-dimensional **embedding matrix**, with each word (actually token) quantified as a **numeric vector** representing its (uninterpretable) semantic features. Users are suggested to import [word vectors data](https://psychbruce.github.io/WordVector_RData.pdf) as the `embed` class using the function `load_embed()`, which would automatically normalize all word vectors to the unit length 1 (see the `normalize()` function) and accelerate the running of most functions in `PsychWordVec`.

## Functions in `PsychWordVec`

-   Word Embeddings Data Management and Transformation
    -   `as_embed()`: from `wordvec` (data.table) to `embed` (matrix)
    -   `as_wordvec()`: from `embed` (matrix) to `wordvec` (data.table)
    -   `load_embed()`: load word embeddings data as `embed` (matrix)
    -   `load_wordvec()`: load word embeddings data as `wordvec` (data.table)
    -   `data_transform()`: transform plain text word vectors to `wordvec` or `embed`
-   Word Vectors Extraction, Linear Operation, and Visualization
    -   `subset()`: extract a subset of `wordvec` and `embed`
    -   `normalize()`: normalize all word vectors to the unit length 1
    -   `get_wordvec()`: extract word vectors
    -   `sum_wordvec()`: calculate the sum vector of multiple words
    -   `plot_wordvec()`: visualize word vectors
    -   `plot_wordvec_tSNE()`: 2D or 3D visualization with t-SNE
    -   `orth_procrustes()`: Orthogonal Procrustes matrix alignment
-   Word Semantic Similarity Analysis, Network Analysis, and Association Test
    -   `cosine_similarity()`: `cos_sim()` or `cos_dist()`
    -   `pair_similarity()`: compute a similarity matrix of word pairs
    -   `plot_similarity()`: visualize similarities of word pairs
    -   `tab_similarity()`: tabulate similarities of word pairs
    -   `most_similar()`: find the Top-N most similar words
    -   `plot_network()`: visualize a (partial correlation) network graph of words
    -   `test_WEAT()`: WEAT and SC-WEAT with permutation test of significance
    -   `test_RND()`: RND with permutation test of significance
-   Dictionary Automatic Expansion and Reliability Analysis
    -   `dict_expand()`: expand a dictionary from the most similar words
    -   `dict_reliability()`: reliability analysis and PCA of a dictionary
-   Local Training of Static Word Embeddings (Word2Vec, GloVe, and FastText)
    -   `tokenize()`: tokenize raw text
    -   `train_wordvec()`: train static word embeddings
-   Pre-trained Language Models (PLM) and Contextualized Word Embeddings
    -   `text_init()`: set up a Python environment for PLM
    -   `text_model_download()`: download PLMs from [HuggingFace](https://huggingface.co/models) to local ".cache" folder
    -   `text_model_remove()`: remove PLMs from local ".cache" folder
    -   `text_to_vec()`: extract contextualized token and text embeddings
    -   `text_unmask()`: fill in the blank mask(s) in a query

See the documentation (help pages) for their usage and details.
