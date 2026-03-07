# Train static word embeddings using the Word2Vec, GloVe, or FastText algorithm.

Train static word embeddings using the
[Word2Vec](https://rdrr.io/pkg/word2vec/man/word2vec.html),
[GloVe](https://rdrr.io/pkg/rsparse/man/GloVe.html), or
[FastText](https://rdrr.io/pkg/fastTextR/man/ft_train.html) algorithm
with multi-threading.

## Usage

``` r
train_wordvec(
  text,
  method = c("word2vec", "glove", "fasttext"),
  dims = 300,
  window = 5,
  min.freq = 5,
  threads = 8,
  model = c("skip-gram", "cbow"),
  loss = c("ns", "hs"),
  negative = 5,
  subsample = 1e-04,
  learning = 0.05,
  ngrams = c(3, 6),
  x.max = 10,
  convergence = -1,
  stopwords = character(0),
  encoding = "UTF-8",
  tolower = FALSE,
  normalize = FALSE,
  iteration,
  tokenizer,
  remove,
  file.save,
  compress = "bzip2",
  verbose = TRUE
)
```

## Arguments

- text:

  A character vector of text, or a file path on disk containing text.

- method:

  Training algorithm:

  - `"word2vec"` (default): using
    [`word2vec::word2vec()`](https://rdrr.io/pkg/word2vec/man/word2vec.html)

  - `"glove"`: using
    [`rsparse::GloVe()`](https://rdrr.io/pkg/rsparse/man/GloVe.html) and
    [`text2vec::text2vec()`](https://rdrr.io/pkg/text2vec/man/text2vec.html)

  - `"fasttext"`: using
    [`fastTextR::ft_train()`](https://rdrr.io/pkg/fastTextR/man/ft_train.html)

- dims:

  Number of dimensions of word vectors to be trained. Common choices
  include 50, 100, 200, 300, and 500. Defaults to `300`.

- window:

  Window size (number of nearby words behind/ahead the current word). It
  defines how many surrounding words to be included in training:
  \[window\] words behind and \[window\] words ahead (\[window\]\*2 in
  total). Defaults to `5`.

- min.freq:

  Minimum frequency of words to be included in training. Words that
  appear less than this value of times will be excluded from vocabulary.
  Defaults to `5` (take words that appear at least five times).

- threads:

  Number of CPU threads used for training. A modest value produces the
  fastest training. Too many threads are not always helpful. Defaults to
  `8`.

- model:

  **\[Only for Word2Vec / FastText\]** Learning model architecture:

  - `"skip-gram"` (default): Skip-Gram, which predicts surrounding words
    given the current word

  - `"cbow"`: Continuous Bag-of-Words, which predicts the current word
    based on the context

- loss:

  **\[Only for Word2Vec / FastText\]** Loss function (computationally
  efficient approximation):

  - `"ns"` (default): Negative Sampling

  - `"hs"`: Hierarchical Softmax

- negative:

  **\[Only for Negative Sampling in Word2Vec / FastText\]** Number of
  negative examples. Values in the range 5~20 are useful for small
  training datasets, while for large datasets the value can be as small
  as 2~5. Defaults to `5`.

- subsample:

  **\[Only for Word2Vec / FastText\]** Subsampling of frequent words
  (threshold for occurrence of words). Those that appear with higher
  frequency in the training data will be randomly down-sampled. Defaults
  to `0.0001` (`1e-04`).

- learning:

  **\[Only for Word2Vec / FastText\]** Initial (starting) learning rate,
  also known as alpha. Defaults to `0.05`.

- ngrams:

  **\[Only for FastText\]** Minimal and maximal ngram length. Defaults
  to `c(3, 6)`.

- x.max:

  **\[Only for GloVe\]** Maximum number of co-occurrences to use in the
  weighting function. Defaults to `10`.

- convergence:

  **\[Only for GloVe\]** Convergence tolerance for SGD iterations.
  Defaults to `-1`.

- stopwords:

  **\[Only for Word2Vec / GloVe\]** A character vector of stopwords to
  be excluded from training.

- encoding:

  Text encoding. Defaults to `"UTF-8"`.

- tolower:

  Convert all upper-case characters to lower-case? Defaults to `FALSE`.

- normalize:

  Normalize all word vectors to unit length? Defaults to `FALSE`. See
  [`normalize()`](https://psychbruce.github.io/PsychWordVec/reference/normalize.md).

- iteration:

  Number of training iterations. More iterations makes a more precise
  model, but computational cost is linearly proportional to iterations.
  Defaults to `5` for Word2Vec and FastText while `10` for GloVe.

- tokenizer:

  Function used to tokenize the text. Defaults to
  [`text2vec::word_tokenizer()`](https://rdrr.io/pkg/text2vec/man/tokenizers.html).

- remove:

  Strings (in regular expression) to be removed from the text. Defaults
  to `"_|'|<br/>|<br />|e\\\\.g\\\\.|i\\\\.e\\\\."`. You may turn off
  this by specifying `remove=NULL`.

- file.save:

  File name of to-be-saved R data (must be .RData).

- compress:

  Compression method for the saved file. Defaults to `"bzip2"`.

  - `1` or `"gzip"`: modest file size (fastest)

  - `2` or `"bzip2"`: small file size (fast)

  - `3` or `"xz"`: minimized file size (slow)

- verbose:

  Print information to the console? Defaults to `TRUE`.

## Value

A `wordvec` (data.table) with three variables: `word`, `vec`, `freq`.

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## References

All-in-one package:

- <https://CRAN.R-project.org/package=wordsalad>

Word2Vec:

- <https://code.google.com/archive/p/word2vec/>

- <https://CRAN.R-project.org/package=word2vec>

- <https://github.com/maxoodf/word2vec>

GloVe:

- <https://nlp.stanford.edu/projects/glove/>

- <https://text2vec.org/glove.html>

- <https://CRAN.R-project.org/package=text2vec>

- <https://CRAN.R-project.org/package=rsparse>

FastText:

- <https://fasttext.cc/>

- <https://CRAN.R-project.org/package=fastTextR>

## See also

[`tokenize()`](https://psychbruce.github.io/PsychWordVec/reference/tokenize.md)

## Examples

``` r
review = text2vec::movie_review  # a data.frame'
text = review$review

## Note: All the examples train 50 dims for faster code check.

## Word2Vec (SGNS)
dt1 = train_wordvec(
  text,
  method="word2vec",
  model="skip-gram",
  dims=50, window=5,
  normalize=TRUE)
#> ✔ Tokenized: 70105 sentences (time cost = 2 secs)
#> ✔ Text corpus: 5242249 characters, 1185427 tokens (roughly words)
#> 
#> ── Training model information ──────────────────────────────────────────────────
#> - Method:      Word2Vec (Skip-Gram with Negative Sampling)
#> - Dimensions:  50
#> - Window size: 5 (5 words behind and 5 words ahead the current word)
#> - Subsampling: 1e-04
#> - Min. freq.:  5 occurrences in text
#> - Iterations:  5 training iterations
#> - CPU threads: 8
#> 
#> ── Training... 
#> ✔ Word vectors trained: 14205 unique tokens (time cost = 6 secs)

dt1
#> # wordvec (data.table): [14205 × 3] (normalized)
#>          word                     vec  freq
#>     1:    the [ 0.1336, ...<50 dims>] 58797
#>     2:    and [-0.0418, ...<50 dims>] 32193
#>     3:      a [-0.0537, ...<50 dims>] 31783
#>     4:     of [ 0.1400, ...<50 dims>] 29142
#>     5:     to [-0.0408, ...<50 dims>] 27218
#> ------                                     
#> 14201: drunks [ 0.1589, ...<50 dims>]     5
#> 14202:   flea [ 0.1164, ...<50 dims>]     5
#> 14203: liquid [ 0.0601, ...<50 dims>]     5
#> 14204:   LOTR [ 0.1651, ...<50 dims>]     5
#> 14205: morose [ 0.1029, ...<50 dims>]     5
most_similar(dt1, "Ive")  # evaluate performance
#> [Word Vector] =~ Ive
#> (normalized to unit length)
#>         word   cos_sim row_id
#>       <char>     <num>  <int>
#>  1:      ive 0.9024187    110
#>  2:    Youve 0.8256251    124
#>  3:   lately 0.8117039    766
#>  4:   dozens 0.7907409   1542
#>  5: funniest 0.7896039   3487
#>  6:   havent 0.7848171   4782
#>  7:     ever 0.7755008   5131
#>  8:     seen 0.7726179   5989
#>  9: Possibly 0.7725835   6331
#> 10: scariest 0.7724189   8503
most_similar(dt1, ~ man - he + she, topn=5)  # evaluate performance
#> [Word Vector] =~ man - he + she
#> (normalized to unit length)
#>      word   cos_sim row_id
#>    <char>     <num>  <int>
#> 1:  woman 0.8375342    260
#> 2:   lady 0.7875039    299
#> 3:   girl 0.7617716    478
#> 4:  child 0.7384513    523
#> 5:    boy 0.7077159   1140
most_similar(dt1, ~ boy - he + she, topn=5)  # evaluate performance
#> [Word Vector] =~ boy - he + she
#> (normalized to unit length)
#>      word   cos_sim row_id
#>    <char>     <num>  <int>
#> 1:   girl 0.8120676    260
#> 2:   lady 0.7852260    299
#> 3:  woman 0.7801553    674
#> 4:    kid 0.7194529   1140
#> 5:   baby 0.6977160   1150

## GloVe
dt2 = train_wordvec(
  text,
  method="glove",
  dims=50, window=5,
  normalize=TRUE)
#> ✔ Tokenized: 70105 sentences (time cost = 2 secs)
#> ✔ Text corpus: 5242249 characters, 1185427 tokens (roughly words)
#> 
#> ── Training model information ──────────────────────────────────────────────────
#> - Method:      GloVe
#> - Dimensions:  50
#> - Window size: 5 (5 words behind and 5 words ahead the current word)
#> - Subsampling: N/A
#> - Min. freq.:  5 occurrences in text
#> - Iterations:  10 training iterations
#> - CPU threads: 8
#> 
#> ── Training... 
#> ✔ Word vectors trained: 14207 unique tokens (time cost = 9 secs)

dt2
#> # wordvec (data.table): [14207 × 3] (normalized)
#>            word                     vec  freq
#>     1:      the [ 0.0245, ...<50 dims>] 58797
#>     2:      and [ 0.0410, ...<50 dims>] 32193
#>     3:        a [ 0.0021, ...<50 dims>] 31783
#>     4:       of [ 0.0020, ...<50 dims>] 29142
#>     5:       to [-0.0595, ...<50 dims>] 27218
#> ------                                       
#> 14203:      yea [ 0.1832, ...<50 dims>]     5
#> 14204:   yearly [-0.0499, ...<50 dims>]     5
#> 14205: yearning [ 0.0065, ...<50 dims>]     5
#> 14206:   yelled [ 0.3892, ...<50 dims>]     5
#> 14207:      yer [-0.0892, ...<50 dims>]     5
most_similar(dt2, "Ive")  # evaluate performance
#> [Word Vector] =~ Ive
#> (normalized to unit length)
#>        word   cos_sim row_id
#>      <char>     <num>  <int>
#>  1:    seen 0.9290392    110
#>  2:    ever 0.8840161    124
#>  3:   worst 0.7779253    127
#>  4:   heard 0.7452084    261
#>  5:   since 0.7208421    262
#>  6:  havent 0.7030606    305
#>  7: watched 0.6915888    468
#>  8:   youve 0.6824924    515
#>  9: already 0.6785463    767
#> 10:    best 0.6745803    950
most_similar(dt2, ~ man - he + she, topn=5)  # evaluate performance
#> [Word Vector] =~ man - he + she
#> (normalized to unit length)
#>      word   cos_sim row_id
#>    <char>     <num>  <int>
#> 1:  woman 0.8741511    198
#> 2:  young 0.7545400    244
#> 3:   girl 0.7540540    260
#> 4:  child 0.7499048    299
#> 5:    guy 0.7301461    523
most_similar(dt2, ~ boy - he + she, topn=5)  # evaluate performance
#> [Word Vector] =~ boy - he + she
#> (normalized to unit length)
#>      word   cos_sim row_id
#>    <char>     <num>  <int>
#> 1:   girl 0.8237547    153
#> 2:  young 0.7521802    198
#> 3:  woman 0.7501856    260
#> 4:  named 0.7344799    299
#> 5:    man 0.6913053    867

## FastText
dt3 = train_wordvec(
  text,
  method="fasttext",
  model="skip-gram",
  dims=50, window=5,
  normalize=TRUE)
#> ✔ Tokenized: 70105 sentences (time cost = 2 secs)
#> ✔ Text corpus: 5242249 characters, 1185427 tokens (roughly words)
#> 
#> ── Training model information ──────────────────────────────────────────────────
#> - Method:      FastText (Skip-Gram with Negative Sampling)
#> - Dimensions:  50
#> - Window size: 5 (5 words behind and 5 words ahead the current word)
#> - Subsampling: 1e-04
#> - Min. freq.:  5 occurrences in text
#> - Iterations:  5 training iterations
#> - CPU threads: 8
#> 
#> ── Training... 
#> ✔ Word vectors trained: 14207 unique tokens (time cost = 15 secs)

dt3
#> # wordvec (data.table): [14207 × 3] (normalized)
#>                word                     vec  freq
#>     1:          the [ 0.0151, ...<50 dims>] 58797
#>     2:          and [ 0.1275, ...<50 dims>] 32193
#>     3:            a [ 0.0605, ...<50 dims>] 31783
#>     4:           of [ 0.0976, ...<50 dims>] 29142
#>     5:           to [ 0.0547, ...<50 dims>] 27218
#> ------                                           
#> 14203:        spray [ 0.0176, ...<50 dims>]     5
#> 14204: disabilities [ 0.1069, ...<50 dims>]     5
#> 14205:        crook [ 0.0289, ...<50 dims>]     5
#> 14206:     Syndrome [-0.0086, ...<50 dims>]     5
#> 14207:      snipers [-0.0370, ...<50 dims>]     5
most_similar(dt3, "Ive")  # evaluate performance
#> [Word Vector] =~ Ive
#> (normalized to unit length)
#>           word   cos_sim row_id
#>         <char>     <num>  <int>
#>  1:      Youve 0.8351248    110
#>  2:       Weve 0.8273311    945
#>  3:       seen 0.7772487   3105
#>  4:      youve 0.7769450   3494
#>  5:         ve 0.7761806   5898
#>  6:      WORST 0.7738867   6816
#>  7:        ive 0.7708102   6913
#>  8:   Vonnegut 0.7454293   7108
#>  9: beforehand 0.7437324   9171
#> 10:    funnier 0.7416327  12894
most_similar(dt3, ~ man - he + she, topn=5)  # evaluate performance
#> [Word Vector] =~ man - he + she
#> (normalized to unit length)
#>        word   cos_sim row_id
#>      <char>     <num>  <int>
#> 1:    woman 0.8852883    261
#> 2:     girl 0.7864475    299
#> 3: salesman 0.7765916   5594
#> 4:  caveman 0.7429952  12263
#> 5: henchman 0.7390146  12884
most_similar(dt3, ~ boy - he + she, topn=5)  # evaluate performance
#> [Word Vector] =~ boy - he + she
#> (normalized to unit length)
#>        word   cos_sim row_id
#>      <char>     <num>  <int>
#> 1:     girl 0.8381224    261
#> 2:    woman 0.7644894    299
#> 3: teenager 0.6950984   1949
#> 4:    widow 0.6718249   2364
#> 5:   blonde 0.6705159   5504
```
