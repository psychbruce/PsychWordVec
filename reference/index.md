# Package index

## All functions

- [`as_embed()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  [`as_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  [`` `[`( ``*`<embed>`*`)`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  [`pattern()`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  :

  Word vectors data class: `wordvec` and `embed`.

- [`cosine_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md)
  [`cos_sim()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md)
  [`cos_dist()`](https://psychbruce.github.io/PsychWordVec/reference/cosine_similarity.md)
  : Cosine similarity/distance between two vectors.

- [`data_transform()`](https://psychbruce.github.io/PsychWordVec/reference/data_transform.md)
  :

  Transform plain text of word vectors into `wordvec` (data.table) or
  `embed` (matrix), saved in a compressed ".RData" file.

- [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
  [`load_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
  [`load_embed()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md)
  :

  Load word vectors data (`wordvec` or `embed`) from ".RData" file.

- [`data_wordvec_subset()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md)
  [`subset(`*`<wordvec>`*`)`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md)
  [`subset(`*`<embed>`*`)`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_subset.md)
  : \[S3 method\] Extract a subset of word vectors data.

- [`demodata`](https://psychbruce.github.io/PsychWordVec/reference/demodata.md)
  : Demo data (pre-trained using word2vec on Google News; 8000 vocab,
  300 dims).

- [`dict_expand()`](https://psychbruce.github.io/PsychWordVec/reference/dict_expand.md)
  : Expand a dictionary from the most similar words.

- [`dict_reliability()`](https://psychbruce.github.io/PsychWordVec/reference/dict_reliability.md)
  : Reliability analysis and PCA of a dictionary.

- [`get_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/get_wordvec.md)
  : Extract word vector(s).

- [`most_similar()`](https://psychbruce.github.io/PsychWordVec/reference/most_similar.md)
  : Find the Top-N most similar words.

- [`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md)
  : Normalize all word vectors to the unit length 1.

- [`orth_procrustes()`](https://psychbruce.github.io/PsychWordVec/reference/orth_procrustes.md)
  : Orthogonal Procrustes rotation for matrix alignment.

- [`pair_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/pair_similarity.md)
  : Compute a matrix of cosine similarity/distance of word pairs.

- [`plot_network()`](https://psychbruce.github.io/PsychWordVec/reference/plot_network.md)
  : Visualize a (partial correlation) network graph of words.

- [`plot_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/plot_similarity.md)
  : Visualize cosine similarity of word pairs.

- [`plot_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/plot_wordvec.md)
  : Visualize word vectors.

- [`plot_wordvec_tSNE()`](https://psychbruce.github.io/PsychWordVec/reference/plot_wordvec_tSNE.md)
  : Visualize word vectors with dimensionality reduced using t-SNE.

- [`sum_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/sum_wordvec.md)
  : Calculate the sum vector of multiple words.

- [`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md)
  : Tabulate cosine similarity/distance of word pairs.

- [`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md)
  : Relative Norm Distance (RND) analysis.

- [`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md)
  : Word Embedding Association Test (WEAT) and Single-Category WEAT.

- [`tokenize()`](https://psychbruce.github.io/PsychWordVec/reference/tokenize.md)
  : Tokenize raw text for training word embeddings.

- [`train_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/train_wordvec.md)
  : Train static word embeddings using the Word2Vec, GloVe, or FastText
  algorithm.
