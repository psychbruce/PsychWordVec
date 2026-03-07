# PsychWordVec

Word Embedding Research Framework for Psychological Science.

An integrative toolbox of word embedding research that provides:

1.  A collection of [pre-trained static word
    vectors](https://psychbruce.github.io/WordVector_RData.pdf) in the
    .RData compressed format.
2.  A group of functions to process, analyze, and visualize word
    vectors.
3.  A range of tests to examine conceptual associations, including the
    *Word Embedding Association Test* (Caliskan et al., 2017) and the
    *Relative Norm Distance* (Garg et al., 2018), with permutation test
    of significance.
4.  A set of training methods to locally train (*static*) word vectors
    from text corpora, including *Word2Vec* (Mikolov et al., 2013),
    *GloVe* (Pennington et al., 2014), and *FastText* (Bojanowski et
    al., 2017).

![](https://psychbruce.github.io/img/CC-BY-NC-SA.jpg)

## Author

Bruce H. W. S. Bao 包寒吴霜

📬 <baohws@foxmail.com>

📋 [psychbruce.github.io](https://psychbruce.github.io)

## Citation

- Bao, H. W. S. (2022). *PsychWordVec: Word embedding research framework
  for psychological science*.
  <https://doi.org/10.32614/CRAN.package.PsychWordVec>
- Bao, H. W. S., Wang, Z., Cheng, X., Su, Z., Yang, Y., Zhang, G., Wang,
  B., & Cai, H. (2023). Using word embeddings to investigate human
  psychology: Methods and applications. *Advances in Psychological
  Science, 31*(6), 887–904.
  <https://doi.org/10.3724/SP.J.1042.2023.00887>  
  \[包寒吴霜, 王梓西, 程曦, 苏展, 杨盈, 张光耀, 王博, 蔡华俭. (2023).
  基于词嵌入技术的心理学研究：方法及应用. *心理科学进展, 31*(6),
  887–904.\]

## Installation

``` r
## Method 1: Install from CRAN
install.packages("PsychWordVec")

## Method 2: Install from GitHub
install.packages("devtools")
devtools::install_github("psychbruce/PsychWordVec", force=TRUE)
```

## Types of Data for `PsychWordVec`

|                  | `embed`                                                                                    | `wordvec`                                                                                    |
|------------------|--------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------|
| Basic class      | matrix                                                                                     | data.table                                                                                   |
| Row size         | vocabulary size                                                                            | vocabulary size                                                                              |
| Column size      | dimension size                                                                             | 2 (variables: `word`, `vec`)                                                                 |
| Advantage        | faster (with matrix operation)                                                             | easier to inspect and manage                                                                 |
| Function to get  | [`as_embed()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)            | [`as_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)            |
| Function to load | [`load_embed()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md) | [`load_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md) |

Note: Word embedding refers to a natural language processing technique
that embeds word semantics into a low-dimensional **embedding matrix**,
with each word (actually token) quantified as a **numeric vector**
representing its (uninterpretable) semantic features. Users are
suggested to import [word vectors
data](https://psychbruce.github.io/WordVector_RData.pdf) as the `embed`
class using the function
[`load_embed()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md),
which would automatically normalize all word vectors to the unit length
1 (see the
[`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md)
function) and accelerate the running of most functions in
`PsychWordVec`.

## Functions in `PsychWordVec`

- Word Embeddings Data Management and Transformation
  - [`as_embed()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md):
    from `wordvec` (data.table) to `embed` (matrix)
  - [`as_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md):
    from `embed` (matrix) to `wordvec` (data.table)
  - [`load_embed()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md):
    load word embeddings data as `embed` (matrix)
  - [`load_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md):
    load word embeddings data as `wordvec` (data.table)
  - [`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md):
    transform plain text word vectors to `wordvec` or `embed`
- Word Vectors Extraction, Linear Operation, and Visualization
  - [`subset()`](https://rdrr.io/r/base/subset.html): extract a subset
    of `wordvec` and `embed`
  - [`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md):
    normalize all word vectors to the unit length 1
  - [`get_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/get_wordvec.md):
    extract word vectors
  - [`sum_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/sum_wordvec.md):
    calculate the sum vector of multiple words
  - [`plot_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/plot_wordvec.md):
    visualize word vectors
  - [`plot_wordvec_tSNE()`](https://psychbruce.github.io/PsychWordVec/reference/plot_wordvec_tSNE.md):
    2D or 3D visualization with t-SNE
  - [`orth_procrustes()`](https://psychbruce.github.io/PsychWordVec/reference/orth_procrustes.md):
    Orthogonal Procrustes matrix alignment
- Word Semantic Similarity Analysis, Network Analysis, and Association
  Test
  - [`cosine_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md):
    [`cos_sim()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md)
    or
    [`cos_dist()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md)
  - [`pair_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/pair_similarity.md):
    compute a similarity matrix of word pairs
  - [`plot_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/plot_similarity.md):
    visualize similarities of word pairs
  - [`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md):
    tabulate similarities of word pairs
  - [`most_similar()`](https://psychbruce.github.io/PsychWordVec/reference/most_similar.md):
    find the Top-N most similar words
  - [`plot_network()`](https://psychbruce.github.io/PsychWordVec/reference/plot_network.md):
    visualize a (partial correlation) network graph of words
  - [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md):
    WEAT and SC-WEAT with permutation test of significance
  - [`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md):
    RND with permutation test of significance
- Dictionary Automatic Expansion and Reliability Analysis
  - [`dict_expand()`](https://psychbruce.github.io/PsychWordVec/reference/dict_expand.md):
    expand a dictionary from the most similar words
  - [`dict_reliability()`](https://psychbruce.github.io/PsychWordVec/reference/dict_reliability.md):
    reliability analysis and PCA of a dictionary
- Local Training of Static Word Embeddings (Word2Vec, GloVe, and
  FastText)
  - [`tokenize()`](https://psychbruce.github.io/PsychWordVec/reference/tokenize.md):
    tokenize raw text
  - [`train_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/train_wordvec.md):
    train static word embeddings

See the documentation (help pages) for their usage and details.
