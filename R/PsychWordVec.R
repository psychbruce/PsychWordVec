#### Initialize ####


#' @import stringr
#' @import ggplot2
#' @import data.table
#' @importFrom dplyr %>% select left_join
#' @importFrom bruceR cc dtime import export Print
.onAttach = function(libname, pkgname) {
  ## Version Check
  inst.ver=as.character(utils::packageVersion("PsychWordVec"))
  xml=suppressWarnings({
    try({
      readLines("https://cran.r-project.org/web/packages/PsychWordVec/index.html")
    }, silent=TRUE)
  })

  ## Update Message
  if(!inherits(xml, "try-error")) {
    try({
      cran.ver=xml[grep("Version:", xml, fixed=TRUE)+1]
      cran.ymd=xml[grep("Published:", xml, fixed=TRUE)+1]
      if(!is.na(cran.ver) & length(cran.ver)==1) {
        cran.ver=substr(cran.ver, 5, nchar(cran.ver)-5)
        cran.ymd=substr(cran.ymd, 5, nchar(cran.ymd)-5)
        if(numeric_version(inst.ver)<numeric_version(cran.ver))
          packageStartupMessage(Glue("
          \n
          NEWS: A new version of PsychWordVec (version {cran.ver}) is available on {cran.ymd}!
          Please update:
          install.packages(\"PsychWordVec\")
          update.packages(ask=FALSE)
          "))
      }
    }, silent=TRUE)
  }

  ## Loaded Package
  pkgs=c("dplyr", "stringr", "ggplot2", "data.table")

  suppressMessages({
    suppressWarnings({
      loaded=sapply(pkgs, require, character.only=TRUE)
    })
  })

  ## Welcome Message
  if(all(loaded)) {
    Print("
    \n
    <<bold PsychWordVec (v{inst.ver})>>

    <<bold Packages also loaded:>>
    <<green
    \u221a dplyr
    \u221a stringr
    \u221a ggplot2
    \u221a data.table
    >>
    \n
    ")
  } else {
    packageStartupMessage(Glue("
    \n
    These R packages are not installed:
    {paste(pkgs[loaded==FALSE], collapse=', ')}

    Please install them.
    \n
    "))
  }
}


#### Basic ####


#' Cosine similarity/distance between two vectors.
#'
#' @details
#' Cosine similarity =
#'
#' \code{sum(v1 * v2) / ( sqrt(sum(v1^2)) * sqrt(sum(v2^2)) )}
#'
#' Cosine distance =
#'
#' \code{1 - cosine_similarity(v1, v2)}
#'
#' @describeIn cosine_similarity cosine similarity
#'
#' @param v1,v2 Numeric vector (of the same length).
#'
#' @return A value of cosine similarity/distance.
#'
#' @examples
#' cosine_similarity(v1=c(1,1,1), v2=c(2,2,2))  # output: 1
#' cosine_similarity(v1=c(1,4,1), v2=c(4,1,1))  # output: 0.5
#' cosine_similarity(v1=c(1,1,0), v2=c(0,0,1))  # output: 0
#'
#' cosine_distance(v1=c(1,1,1), v2=c(2,2,2))  # output: 0
#' cosine_distance(v1=c(1,4,1), v2=c(4,1,1))  # output: 0.5
#' cosine_distance(v1=c(1,1,0), v2=c(0,0,1))  # output: 1
#'
#' @export
cosine_similarity = function(v1, v2) {
  if(length(v1)!=length(v2)) stop("v1 and v2 should be of the same length!", call.=FALSE)
  sum(v1 * v2) / ( sqrt(sum(v1^2)) * sqrt(sum(v2^2)) )
}


#' @describeIn cosine_similarity cosine distance (= 1 - cosine similarity)
#' @export
cosine_distance = function(v1, v2) {
  1 - cosine_similarity(v1, v2)
}


#### Data File ####


#' Transform plain text data of word vectors into a compressed ".RData" file.
#'
#' @param file.load File name of raw data (must be plain text).
#'
#' Data should be in this format (values separated by \code{sep}):
#'
#' cat 0.001 0.002 0.003 0.004 0.005 ... 0.300
#'
#' dog 0.301 0.302 0.303 0.304 0.305 ... 0.600
#' @param file.save File name of to-be-saved R data (must be .rda or .RData).
#' @param encoding File encoding. Default is \code{"UTF-8"}.
#' @param sep Column separator. Default is \code{" "}.
#' @param header First row is header or meta-information? Default is \code{FALSE}.
#' If \code{TRUE}, then the first row will be removed.
#' @param compress Compression method for the saved file. Default is \code{"bzip2"}.
#'
#' Options can be:
#' \itemize{
#'   \item \code{1} or \code{"gzip"}: modest file size (fastest)
#'   \item \code{2} or \code{"bzip2"}: small file size (fast)
#'   \item \code{3} or \code{"xz"}: minimized file size (slow)
#' }
#' @param compress.level Compression level from \code{0} (none) to \code{9}
#' (maximal compression for minimal file size). Default is \code{9}.
#'
#' @return
#' A \code{data.table} with two variables: \code{word} and \code{vec}.
#'
#' @export
data_transform = function(file.load, file.save=NULL,
                          encoding="UTF-8", sep=" ", header=FALSE,
                          compress="bzip2", compress.level=9) {
  cat("\n")
  if(is.data.table(file.load)) {
    dt = file.load  # 2 variables: word, vec
  } else {
    t0 = Sys.time()
    Print("Loading file... \"{file.load}\"")
    d = readLines(file.load, encoding=encoding)
    if(header)
      dt = data.table(x=d[-1])
    else
      dt = data.table(x=d)
    rm(d)
    dt[, `:=`(
      word = str_split(dt$x, sep, n=2, simplify=TRUE)[,1],
      vec = do.call("list", lapply(
        str_split(dt$x, sep, n=2, simplify=TRUE)[,2], function(v) {
          as.numeric(cc(v, sep=sep))
        }))
    )]
    dt$x = NULL
    ndim = length(dt[[1, "vec"]])
    Print("Word vector data: {nrow(dt)} words with {ndim} dimensions (time cost = {dtime(t0)})")
  }
  if(!is.null(file.save)) {
    t0 = Sys.time()
    compress = switch(compress,
                      `1`="gzip",
                      `2`="bzip2",
                      `3`="xz",
                      compress)
    if(!str_detect(file.save, "\\.rda$|\\.[Rr][Dd]ata$"))
      stop("`file.save` should be .rda or .RData!", call.=FALSE)
    save(dt, file=file.save,
         compress=compress,
         compression_level=compress.level)
    Print("<<green \u221a>> Saved to \"{file.save}\" (time cost = {dtime(t0)})")
  }
  invisible(dt)
}


#' Load word vector data from an ".RData" file.
#'
#' @param file File name of the data (must be .rda or .RData transformed by
#' \code{\link{data_transform}}, with only two variables \code{word} and \code{vec}).
#'
#' @return
#' A \code{data.table} with three variables: \code{word}, \code{vec}, \code{dim} (number of dimensions).
#'
#' You may drop/delete the variable \code{dim} but should always keep \code{word} and \code{vec} in the data.
#'
#' @export
data_wordvec_load = function(file) {
  t0 = Sys.time()
  cat("Loading...")
  if(!str_detect(file, "\\.rda$|\\.[Rr][Dd]ata$"))
    stop("`file` should be .rda or .RData!", call.=FALSE)
  envir = new.env()
  load(file=file, envir=envir)
  if(length(ls(envir)) > 1)
    warning("RData file contains multiple objects. Returning the first object.", call.=FALSE)
  data = get(ls(envir)[1], envir)
  rm(envir)
  if(!all(c("word", "vec") %in% names(data)))
    stop("`file` should be prepared using `data_transform()`!", call.=FALSE)
  data$dim = sapply(data$vec, length)
  dims = unique(data$dim)
  if(length(dims) > 1)
    warning("The number of dimensions is not consistent between words!", call.=FALSE)
  cat("\015")
  Print("<<green \u221a>> Word Vectors: {nrow(data)} words, {dims[1]} dims (loading time: {dtime(t0)})")
  return(data)
}


#### Demo Data ####

# library(PsychWordVec)
# d = data_wordvec_load("data-raw/GoogleNews/word2vec_googlenews_eng_1word.RData")
# d$dim = NULL
# demodata = head(d[!str_detect(word, "[^A-Za-z]")], 10000)
# usethis::use_data(demodata, overwrite=TRUE, compress="xz")

#' Demo data of word vectors (corpus: Google News; algorithm: word2vec; vocabulary: 10000; dimensions: 300).
#'
#' @description
#' The demo data contains a sample of Top-10k frequent English single words
#' with their 300-d word embeddings (word vectors) trained using
#' the "word2vec" algorithm based on the Google News corpus
#' (for data source and details, see the URL in Source).
#'
#' @format
#' A \code{data.table} with two variables \code{word} and \code{vec},
#' transformed from the raw data (see the URL in Source) into \code{.RData}
#' using the \code{\link{data_transform}} function.
#'
#' @source
#' Google Code - word2vec (\url{https://code.google.com/archive/p/word2vec/})
#'
#' @name demodata
NULL


#### Get Word Vectors ####


#' Extract word vector of a single word.
#'
#' @param data Data table (\code{data.table}) with variables \code{word} and \code{vec}.
#'
#' See \code{\link{data_transform}} for details about data format.
#' @param word Word string (a single word).
#'
#' @return
#' A numeric vector of the word (or \code{NA} if the word is not in the data).
#'
#' @export
get_wordvec = function(data, word) {
  if(!is.data.table(data)) stop("Data should be of class `data.table`.", call.=FALSE)
  if(!all(c("word", "vec") %in% names(data))) stop("Data should have two variables `word` and `vec`.", call.=FALSE)
  if(word %in% data$word) {
    vec = data[[which(data$word==word), "vec"]]
  } else {
    vec = NA
  }
  return(vec)
}


#' Extract word vectors of multiple words.
#'
#' Extract word vectors of multiple words,
#' using either wordlist (a vector of words; using \code{words})
#' or regular expression (a pattern of words; using \code{pattern}).
#' If both (\code{words} and \code{pattern}) are specified, \code{words} wins.
#'
#' @inheritParams get_wordvec
#' @param words [Option 1] Word string (\code{NULL}; a single word; a vector of words).
#' @param pattern [Option 2] Pattern of regular expression (see \code{\link[stringr:str_detect]{str_detect}}).
#'
#' @return
#' A \code{data.table} with words as columns and dimensions of word vectors as rows.
#'
#' @export
get_wordvecs = function(data, words=NULL, pattern=NULL) {
  dt = data.table()
  if(is.null(words)) {
    if(is.null(pattern)) {
      stop("Please specify either `words` or `pattern`!", call.=FALSE)
    } else {
      words = data$word[str_detect(data$word, pattern)]
    }
  }
  for(word in words) {
    wordvec = get_wordvec(data, word)
    if(length(wordvec)==1) {
      # NA
      Print("\"{word}\" not found...")
    } else {
      dt[, word] = wordvec
    }
  }
  return(dt)
}


#### Word Similarity ####


#' Find the Top-N most similar words.
#'
#' @inheritParams get_wordvec
#' @param topn Top-N most similar words. Default is \code{10}.
#' @param above A threshold of cosine similarity to get all words with
#' similarity above this value. Default is \code{NULL}.
#' If both \code{topn} and \code{above} are specified, \code{above} wins.
#'
#' @return
#' A \code{data.table} with the most similar words and their cosine similarities.
#' The row number of each word in the raw data is also returned,
#' which may help determine the relative word frequency in some cases.
#'
#' @export
most_similar = function(data, word, topn=10, above=NULL) {
  ms = data.table()
  wordvec = get_wordvec(data, word)
  if(length(wordvec)==1) {
    # NA
    Print("\"{word}\" not found...")
  } else {
    cos_sim = NULL
    data$cos_sim = sapply(data$vec, function(vec) {
      cosine_similarity(wordvec, vec)
    })
    data$row_id = 1:nrow(data)
    if(is.null(above))
      ms = head(data[order(-cos_sim), c("word", "cos_sim", "row_id")], topn)
    else
      ms = data[order(-cos_sim), c("word", "cos_sim", "row_id")][cos_sim >= above]
  }
  return(ms)
}


#' Find all words more similar to a word than a critical word is to the word.
#'
#' @inheritParams get_wordvec
#' @param than Critical word (a single word).
#'
#' @return
#' A \code{data.table} with all words with cosine similarities higher than
#' that of the critical word.
#'
#' @export
more_similar_than = function(data, word, than) {
  wv = get_wordvec(data, word)
  wv.than = get_wordvec(data, than)
  if(length(wv.than)==1) {
    # NA
    Print("\"{than}\" not found...")
  } else {
    ms = most_similar(data, word, above=cosine_similarity(wv, wv.than))
    return(ms)
  }
}


#' Visualize word vector(s).
#'
#' @inheritParams get_wordvecs
#'
#' @export
plot_wordvecs = function(data, words, dims=NULL) {
  dt = get_wordvecs(data, words)
  if(!is.null(dims)) dt = dt[dims, ]
  eigen.value = eigen(cor(dt), only.values=TRUE)$values
  pca = psych::principal(dt, nfactors=sum(eigen.value>1), rotate="varimax")
  dts = cbind(dt, pca$scores)
  dts[order(-RC1), ]
  order =
  dp = melt(dt, measure.vars=names(dt),
            variable.name="word", value.name="value")
  dp$dim = rep(1:nrow(dt), length(dt))
  ggplot(dp, aes(x=dim, y=factor(word, levels=rev(words)))) +
    geom_tile(aes(fill=value)) +
    labs(x="Dimension", y="Word", fill=NULL,
         title="Word Vectors (" %^% nrow(dt) %^% " Dimensions)") +
    theme_void(base_size=12) +
    theme(axis.text.y=element_text(size=12, hjust=0,
                                   margin=margin(0, 0, 0, 1, "lines")),
          plot.title=element_text(hjust=0.5),
          plot.margin=margin(0.05, 0.05, 0.05, 0.05, "npc"))
}


#### Psych Test ####

