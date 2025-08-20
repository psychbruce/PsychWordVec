#### Train Static Word Vectors ####


## Default UTF-8 separators used to split words and sentences.
utf8_split_default = function() {
  c(paste0(" \n,.-?!:;/\"#$%&'()*+<=>@[]\\^_`{|}~\t\v\f\r",
           "\u3001\u3002\uff01\uff02\uff03\uff04\uff05\uff06\uff07\uff08\uff09\uff0a\uff0b\uff0c\uff0d\uff0e\uff0f",
           "\uff1a\uff1b\uff1c\uff1d\uff1e\uff1f\u2014\u2018\u2019\u201c\u201d\u3010\u3011\u300a\u300b"),
    "\n.?!\u3002\uff1f\uff01\u2026")
}


#' Tokenize raw text for training word embeddings.
#'
#' @inheritParams data_transform
#' @param text A character vector of text, or a file path on disk containing text.
#' @param tokenizer Function used to tokenize the text. Defaults to [text2vec::word_tokenizer()].
#' @param split Separator between tokens, only used when `simplify=TRUE`. Defaults to `" "`.
#' @param remove Strings (in regular expression) to be removed from the text. Defaults to `"_|'|<br/>|<br />|e\\\\.g\\\\.|i\\\\.e\\\\."`. You may turn off this by specifying `remove=NULL`.
#' @param encoding Text encoding (only used if `text` is a file). Defaults to `"UTF-8"`.
#' @param simplify Return a character vector (`TRUE`) or a list of character vectors (`FALSE`). Defaults to `TRUE`.
#'
#' @return
#' - `simplify=TRUE`: A tokenized character vector, with each element as a sentence.
#' - `simplify=FALSE`: A list of tokenized character vectors, with each element as a vector of tokens in a sentence.
#'
#' @seealso
#' [train_wordvec()]
#'
#' @examples
#' \donttest{txt1 = c(
#'   "I love natural language processing (NLP)!",
#'   "I've been in this city for 10 years. I really like here!",
#'   "However, my computer is not among the \"Top 10\" list."
#' )
#' tokenize(txt1, simplify=FALSE)
#' tokenize(txt1) %>% cat(sep="\n----\n")
#'
#' txt2 = text2vec::movie_review$review[1:5]
#' texts = tokenize(txt2)
#'
#' txt2[1]
#' texts[1:20]  # all sentences in txt2[1]
#' }
#' @export
tokenize = function(
    text,
    tokenizer=text2vec::word_tokenizer,
    split=" ",
    remove="_|'|<br/>|<br />|e\\.g\\.|i\\.e\\.",
    # '\\w+
    encoding="UTF-8",
    simplify=TRUE,
    verbose=TRUE
) {
  ## Import text if necesssary
  t0 = Sys.time()
  if(length(text) == 1) {
    if(file.exists(text)) {
      if(verbose) Print("Reading text from file...")
      text = readLines(text, encoding=encoding, warn=FALSE)
      if(verbose) cli::cli_alert_success("Raw text corpus has been loaded (time cost = {dtime(t0, 'auto')})")
    }
  }

  t0 = Sys.time()
  split.sentence = "\\n|\\.|:|;|\\?|\\!|\u3002|\uff1f|\uff01|\u2026"
  if(!is.null(remove)) text = str_remove_all(text, remove)
  text = str_trim(unlist(strsplit(text, split.sentence)))
  tokens = tokenizer(text)
  texts = vapply(tokens, paste, collapse=split, FUN.VALUE=character(1))
  texts = texts[texts!=""]
  gc()
  if(verbose) cli::cli_alert_success("Tokenized: {length(texts)} sentences (time cost = {dtime(t0, 'auto')})")
  if(simplify)
    return(texts)
  else
    return(tokens)
}


#' Train static word embeddings using the Word2Vec, GloVe, or FastText algorithm.
#'
#' Train static word embeddings using the [Word2Vec][word2vec::word2vec], [GloVe][rsparse::GloVe], or [FastText][fastTextR::ft_train] algorithm with multi-threading.
#'
#' @inheritParams data_transform
#' @inheritParams data_wordvec_load
#' @inheritParams tokenize
#' @param method Training algorithm:
#' - `"word2vec"` (default): using [word2vec::word2vec()]
#' - `"glove"`: using [rsparse::GloVe()] and [text2vec::text2vec()]
#' - `"fasttext"`: using [fastTextR::ft_train()]
#' @param dims Number of dimensions of word vectors to be trained. Common choices include 50, 100, 200, 300, and 500. Defaults to `300`.
#' @param window Window size (number of nearby words behind/ahead the current word). It defines how many surrounding words to be included in training: \[window\] words behind and \[window\] words ahead (\[window\]*2 in total). Defaults to `5`.
#' @param min.freq Minimum frequency of words to be included in training. Words that appear less than this value of times will be excluded from vocabulary. Defaults to `5` (take words that appear at least five times).
#' @param threads Number of CPU threads used for training. A modest value produces the fastest training. Too many threads are not always helpful. Defaults to `8`.
#' @param model **\[Only for Word2Vec / FastText\]** Learning model architecture:
#' - `"skip-gram"` (default): Skip-Gram, which predicts surrounding words given the current word
#' - `"cbow"`: Continuous Bag-of-Words, which predicts the current word based on the context
#' @param loss **\[Only for Word2Vec / FastText\]** Loss function (computationally efficient approximation):
#' - `"ns"` (default): Negative Sampling
#' - `"hs"`: Hierarchical Softmax
#' @param negative **\[Only for Negative Sampling in Word2Vec / FastText\]** Number of negative examples. Values in the range 5~20 are useful for small training datasets, while for large datasets the value can be as small as 2~5. Defaults to `5`.
#' @param subsample **\[Only for Word2Vec / FastText\]** Subsampling of frequent words (threshold for occurrence of words). Those that appear with higher frequency in the training data will be randomly down-sampled. Defaults to `0.0001` (`1e-04`).
#' @param learning **\[Only for Word2Vec / FastText\]** Initial (starting) learning rate, also known as alpha. Defaults to `0.05`.
#' @param ngrams **\[Only for FastText\]** Minimal and maximal ngram length. Defaults to `c(3, 6)`.
#' @param x.max **\[Only for GloVe\]** Maximum number of co-occurrences to use in the weighting function. Defaults to `10`.
#' @param convergence **\[Only for GloVe\]** Convergence tolerance for SGD iterations. Defaults to `-1`.
#' @param stopwords **\[Only for Word2Vec / GloVe\]** A character vector of stopwords to be excluded from training.
#' @param encoding Text encoding. Defaults to `"UTF-8"`.
#' @param tolower Convert all upper-case characters to lower-case? Defaults to `FALSE`.
#' @param iteration Number of training iterations. More iterations makes a more precise model, but computational cost is linearly proportional to iterations. Defaults to `5` for Word2Vec and FastText while `10` for GloVe.
#'
#' @return
#' A `wordvec` (data.table) with three variables: `word`, `vec`, `freq`.
#'
#' @inheritSection as_embed Download
#'
#' @seealso
#' [tokenize()]
#'
#' @references
#' All-in-one package:
#' - <https://CRAN.R-project.org/package=wordsalad>
#'
#' Word2Vec:
#' - <https://code.google.com/archive/p/word2vec/>
#' - <https://CRAN.R-project.org/package=word2vec>
#' - <https://github.com/maxoodf/word2vec>
#'
#' GloVe:
#' - <https://nlp.stanford.edu/projects/glove/>
#' - <https://text2vec.org/glove.html>
#' - <https://CRAN.R-project.org/package=text2vec>
#' - <https://CRAN.R-project.org/package=rsparse>
#'
#' FastText:
#' - <https://fasttext.cc/>
#' - <https://CRAN.R-project.org/package=fastTextR>
#'
#'
#' @examples
#' \donttest{review = text2vec::movie_review  # a data.frame'
#' text = review$review
#'
#' ## Note: All the examples train 50 dims for faster code check.
#'
#' ## Word2Vec (SGNS)
#' dt1 = train_wordvec(
#'   text,
#'   method="word2vec",
#'   model="skip-gram",
#'   dims=50, window=5,
#'   normalize=TRUE)
#'
#' dt1
#' most_similar(dt1, "Ive")  # evaluate performance
#' most_similar(dt1, ~ man - he + she, topn=5)  # evaluate performance
#' most_similar(dt1, ~ boy - he + she, topn=5)  # evaluate performance
#'
#' ## GloVe
#' dt2 = train_wordvec(
#'   text,
#'   method="glove",
#'   dims=50, window=5,
#'   normalize=TRUE)
#'
#' dt2
#' most_similar(dt2, "Ive")  # evaluate performance
#' most_similar(dt2, ~ man - he + she, topn=5)  # evaluate performance
#' most_similar(dt2, ~ boy - he + she, topn=5)  # evaluate performance
#'
#' ## FastText
#' dt3 = train_wordvec(
#'   text,
#'   method="fasttext",
#'   model="skip-gram",
#'   dims=50, window=5,
#'   normalize=TRUE)
#'
#' dt3
#' most_similar(dt3, "Ive")  # evaluate performance
#' most_similar(dt3, ~ man - he + she, topn=5)  # evaluate performance
#' most_similar(dt3, ~ boy - he + she, topn=5)  # evaluate performance
#' }
#' @export
train_wordvec = function(
    text,
    method=c("word2vec", "glove", "fasttext"),
    dims=300,
    window=5,
    min.freq=5,
    threads=8,
    model=c("skip-gram", "cbow"),
    loss=c("ns", "hs"),
    negative=5,
    subsample=0.0001,
    learning=0.05,
    ngrams=c(3, 6),
    x.max=10,
    convergence=-1,
    stopwords=character(0),
    encoding="UTF-8",
    tolower=FALSE,
    normalize=FALSE,
    iteration,
    tokenizer,
    remove,
    file.save,
    compress="bzip2",
    verbose=TRUE
) {
  ## Initialize
  method = match.arg(method)
  model = match.arg(model)
  loss = match.arg(loss)
  if(dims < 0)
    stop("`dims` must be a positive integer.", call.=FALSE)
  if(missing(tokenizer))
    tokenizer = text2vec::word_tokenizer
  if(missing(remove))
    remove = "_|'|<br/>|<br />|e\\.g\\.|i\\.e\\."  # see tokenize()
  if(missing(iteration))
    iteration = ifelse(method=="glove", 10, 5)
  if(method=="glove") {
    method.text = "GloVe"
  } else {
    method.text = paste0(
      ifelse(method=="word2vec", "Word2Vec (", "FastText ("),
      ifelse(model=="skip-gram",
             ifelse(loss=="ns",
                    "Skip-Gram with Negative Sampling",
                    "Skip-Gram with Hierarchical Softmax"),
             ifelse(loss=="ns",
                    "Continuous Bag-of-Words with Negative Sampling",
                    "Continuous Bag-of-Words with Hierarchical Softmax")),
      ")")
  }

  ## Import text if necesssary
  if(length(text) == 1) {
    if(file.exists(text)) {
      t0 = Sys.time()
      if(verbose) Print("Reading text from file...")
      text = readLines(text, encoding=encoding, warn=FALSE)
      if(verbose) cli::cli_alert_success("Raw text corpus has been loaded (time cost = {dtime(t0, 'auto')})")
    }
  }

  ## Tokenize text and count token/word frequency
  split = ifelse(model=="word2vec", "\t", " ")
  text = tokenize(text, tokenizer, split, remove, TRUE, verbose)  # Print()
  if(tolower) text = tolower(text)
  tokens = unlist(strsplit(text, split))
  freq = as.data.table(table(tokens))
  names(freq) = c("token", "freq")
  if(verbose)
    cli::cli_alert_success("Text corpus: {sum(nchar(tokens))} characters, {length(tokens)} tokens (roughly words)")

  ## Train word vectors
  t1 = Sys.time()
  if(verbose) {
    cli::cli_h1("Training model information")
    Print("
    <<cyan
    - Method:      {method.text}
    - Dimensions:  {dims}
    - Window size: {window} ({window} words behind and {window} words ahead the current word)
    - Subsampling: {ifelse(method=='glove', 'N/A', subsample)}
    - Min. freq.:  {min.freq} occurrences in text
    - Iterations:  {iteration} training iterations
    - CPU threads: {threads}
    >>
    ")
    cli::cli_h3("Training...")
  }
  gc()

  ##---- Word2Vec ----##
  if(method == "word2vec") {
    model = word2vec::word2vec(
      x = text,
      type = model,
      dim = dims,
      window = window,
      iter = iteration,
      lr = learning,
      hs = ifelse(loss == "hs", TRUE, FALSE),
      negative = negative,
      sample = subsample,
      min_count = min.freq,
      stopwords = stopwords,
      threads = threads,
      encoding = encoding
    )
    wv = as.matrix(model) %>% as_wordvec()
  }

  ##---- GloVe ----##
  if(method == "glove") {
    itoken = text2vec::itoken(text2vec::space_tokenizer(text))
    vocab = text2vec::prune_vocabulary(
      text2vec::create_vocabulary(
        itoken, stopwords = stopwords),
      term_count_min = min.freq)
    tcm = text2vec::create_tcm(
      # Term Co-occurence Matrix
      itoken,
      text2vec::vocab_vectorizer(vocab),
      skip_grams_window = window)
    model = rsparse::GloVe$new(rank = dims, x_max = x.max)
    temp = capture.output({
      wv.main = model$fit_transform(
        tcm,  # input: term co-occurence matrix
        n_iter = iteration,  # number of SGD iterations
        convergence_tol = convergence,  # convergence tolerance
        n_threads = threads)
    })
    wv.context = model$components
    wv = wv.main + t(wv.context)
    wv = as_wordvec(wv)
  }

  ##---- FastText ----##
  if(method == "fasttext") {
    tmp_file_text = tempfile()
    on.exit({
      if(file.exists(tmp_file_text)) file.remove(tmp_file_text)
    })
    writeLines(text, tmp_file_text)
    control = fastTextR::ft_control(
      loss = loss,
      learning_rate = learning,
      word_vec_size = dims,
      window_size = window,
      epoch = iteration,
      min_count = min.freq,
      neg = negative,
      min_ngram = ngrams[1],
      max_ngram = ngrams[2],
      nthreads = threads,
      threshold = subsample)
    model = fastTextR::ft_train(
      file = tmp_file_text,
      method = gsub("-", "", model),
      control = control)
    wv = as.matrix(model$word_vectors(model$words())) %>%
      as_wordvec()
  }

  ## Order by word frequency
  wv = left_join(wv, freq, by=c("word"="token"))
  wv = wv[order(-freq), ][freq>=min.freq, ]
  if(verbose) cli::cli_alert_success("Word vectors trained: {nrow(wv)} unique tokens (time cost = {dtime(t1, 'auto')})")

  ## Normalize
  if(normalize) wv = normalize(wv)

  ## Save
  if(!missing(file.save)) {
    t2 = Sys.time()
    if(verbose) Print("\n\n\nCompressing and saving...")
    compress = switch(compress,
                      `1`="gzip",
                      `2`="bzip2",
                      `3`="xz",
                      compress)
    save(wv, file=file.save,
         compress=compress,
         compression_level=9)
    if(verbose) cli::cli_alert_success("Saved to \"{file.save}\" (time cost = {dtime(t2, 'auto')})")
  }

  gc()  # Garbage Collection: Free the Memory
  return(wv)
}

