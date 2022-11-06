#### Initialize ####


#' @import stringr
#' @import ggplot2
#' @import data.table
#' @importFrom dplyr %>% select left_join
#' @importFrom bruceR %notin% cc dtime import export Glue Print print_table
.onAttach = function(libname, pkgname) {
  ## Version Check
  new = FALSE
  inst.ver = as.character(utils::packageVersion("PsychWordVec"))
  xml = suppressWarnings({
    try({
      readLines("https://cran.r-project.org/web/packages/PsychWordVec/index.html")
    }, silent=TRUE)
  })

  ## Update Message
  if(!inherits(xml, "try-error")) {
    try({
      cran.ver = xml[grep("Version:", xml, fixed=TRUE) + 1]
      cran.ymd = xml[grep("Published:", xml, fixed=TRUE) + 1]
      if(!is.na(cran.ver) & length(cran.ver)==1) {
        cran.ver = substr(cran.ver, 5, nchar(cran.ver) - 5)
        cran.ymd = substr(cran.ymd, 5, nchar(cran.ymd) - 5)
        if(numeric_version(inst.ver) < numeric_version(cran.ver))
          new = TRUE
      }
    }, silent=TRUE)
  }

  ## Loaded Package
  pkgs = c("dplyr", "stringr", "ggplot2", "data.table")

  suppressMessages({
    suppressWarnings({
      loaded = sapply(pkgs, require, character.only=TRUE)
    })
  })

  ## Welcome Message
  if(all(loaded)) {
    cli::cli_h1("PsychWordVec (v{inst.ver})")
    cn()
    cli::cli_alert_success("Packages also loaded: dplyr, stringr, ggplot2, data.table")
    cn()
    cli::cli_text("
    {.href [Documentation](https://psychbruce.github.io/PsychWordVec)}
    | Download pre-trained word vectors:
    {.url https://psychbruce.github.io/WordVector_RData.pdf}
    ")
    cn()
  } else {
    Print("
    \n
    These R packages are not installed:
    {paste(pkgs[loaded==FALSE], collapse=', ')}

    Please install them.
    \n
    ")
  }

  ## Update Info
  if(new)
    Print("
    NEWS: A new version of PsychWordVec ({cran.ver}) is available on {cran.ymd}!
    ***** Please Update *****
    install.packages(\"PsychWordVec\")
    \n
    ")
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
#' @param v1,v2 Numeric vector (of the same length).
#' @param distance Compute cosine distance instead?
#' Defaults to \code{FALSE} (cosine similarity).
#'
#' @return A value of cosine similarity/distance.
#'
#' @seealso
#' \code{\link{pair_similarity}}
#'
#' \code{\link{tab_similarity}}
#'
#' \code{\link{most_similar}}
#'
#' @examples
#' cosine_similarity(v1=c(1,1,1), v2=c(2,2,2))  # 1
#' cosine_similarity(v1=c(1,4,1), v2=c(4,1,1))  # 0.5
#' cosine_similarity(v1=c(1,1,0), v2=c(0,0,1))  # 0
#'
#' cosine_similarity(v1=c(1,1,1), v2=c(2,2,2), distance=TRUE)  # 0
#' cosine_similarity(v1=c(1,4,1), v2=c(4,1,1), distance=TRUE)  # 0.5
#' cosine_similarity(v1=c(1,1,0), v2=c(0,0,1), distance=TRUE)  # 1
#'
#' @export
cosine_similarity = function(v1, v2, distance=FALSE) {
  if(length(v1) != length(v2)) stop("v1 and v2 must have equal length!", call.=FALSE)
  cos_sim = sum(v1 * v2) / ( sqrt(sum(v1^2)) * sqrt(sum(v2^2)) )
  if(distance)
    return(1 - cos_sim)
  else
    return(cos_sim)
}


# Normalized vectors: vec / sqrt(sum(vec^2))
# cosine_similarity_norm = function(v1, v2) {
#   sum(v1 * v2)
# }


#### Utils ####


#' @importFrom bruceR cc
#' @export
bruceR::cc


check_data_validity = function(data) {
  if(!inherits(data, "wordvec"))
    stop("Data must be loaded using `data_wordvec_load()`.", call.=FALSE)
}


check_load_validity = function(file.load) {
  if(!is.null(file.load))
    if(!str_detect(file.load, "\\.rda$|\\.[Rr][Dd]ata$"))
      stop("`file.load` must be .RData!", call.=FALSE)
}


check_save_validity = function(file.save) {
  if(!is.null(file.save))
    if(!str_detect(file.save, "\\.rda$|\\.[Rr][Dd]ata$"))
      stop("`file.save` must be .RData!", call.=FALSE)
}


extract_valid_words = function(data, words=NULL, pattern=NULL) {
  if(is.null(words)) {
    if(is.null(pattern)) {
      stop("Please specify either `words` or `pattern`!", call.=FALSE)
    } else {
      words.valid = str_subset(data$word, pattern)
      message(Glue("{length(words.valid)} words matched..."))
    }
  } else {
    words.contain = data[word %in% words]$word
    words.valid = words[which(words %in% words.contain)]
  }
  if(length(words.valid) < length(words)) {
    not.found = setdiff(words, words.valid)
    cli::cli_alert_danger("{length(not.found)} words not found: {.val {not.found}}")
  }
  return(words.valid)
}


#### Data File ####


#' Transform plain text data of word vectors into a compressed ".RData" file.
#'
#' @description
#' Transform plain text data of word vectors into a compressed ".RData" file.
#'
#' \emph{Speed}: In total (preprocess + compress + save),
#' it can process about 30000 words/min
#' with the slowest settings (\code{compress="xz"}, \code{compress.level=9})
#' on a modern computer (HP ProBook 450, Windows 11, Intel i7-1165G7 CPU, 32GB RAM).
#'
#' @param file.load File name of raw data (must be plain text).
#'
#' Data must be in this format (values separated by \code{sep}):
#'
#' cat 0.001 0.002 0.003 0.004 0.005 ... 0.300
#'
#' dog 0.301 0.302 0.303 0.304 0.305 ... 0.600
#' @param file.save File name of to-be-saved R data (must be .RData).
#' @param sep Column separator. Defaults to \code{" "}.
#' @param header Is the 1st row a header (e.g., meta-information such as "2000000 300")?
#' Defaults to \code{"auto"}, which automatically determines whether there is a header.
#' If \code{TRUE}, then the 1st row will be dropped.
#' @param encoding File encoding. Defaults to \code{"auto"}
#' (using \code{\link[vroom:vroom_lines]{vroom::vroom_lines()}} to fast read the file).
#' If specified to any other value (e.g., \code{"UTF-8"}),
#' then it uses \code{\link[base:readLines]{readLines()}} to read the file,
#' which is much slower than \code{vroom}.
#' @param compress Compression method for the saved file. Defaults to \code{"bzip2"}.
#'
#' Options include:
#' \itemize{
#'   \item \code{1} or \code{"gzip"}: modest file size (fastest)
#'   \item \code{2} or \code{"bzip2"}: small file size (fast)
#'   \item \code{3} or \code{"xz"}: minimized file size (slow)
#' }
#' @param compress.level Compression level from \code{0} (none) to \code{9}
#' (maximal compression for minimal file size). Defaults to \code{9}.
#' @param verbose Print information to the console? Defaults to \code{TRUE}.
#'
#' @return
#' A \code{data.table} (of new class \code{wordvec}) with two variables: \code{word} and \code{vec}.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{data_wordvec_load}}
#'
#' \code{\link{data_wordvec_normalize}}
#'
#' \code{\link{data_wordvec_reshape}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' \dontrun{
#' # please first manually download plain text data of word vectors
#' # e.g., from: https://fasttext.cc/docs/en/crawl-vectors.html
#'
#' # the text file must be on your disk
#' # the following code cannot run unless you have the file
#' library(bruceR)
#' set.wd()
#' data_transform(file.load="cc.zh.300.vec",   # plain text file
#'                file.save="cc.zh.300.vec.RData",  # RData file
#'                header=TRUE, compress="xz")  # of minimal size
#' }
#'
#' @export
data_transform = function(file.load, file.save,
                          sep=" ", header="auto", encoding="auto",
                          compress="bzip2", compress.level=9,
                          verbose=TRUE) {
  t0 = Sys.time()
  if(!missing(file.save)) check_save_validity(file.save)
  if(inherits(file.load, "wordvec")) {
    dt = file.load  # 2 variables: word, vec
  } else {
    if(verbose) {
      cn()
      Print("****** Data Transformation (~ 30000 words/min in total) ******")
      cn()
      Print("Loading file... \"{file.load}\"")
    }
    gc()  # Garbage Collection: Free the Memory
    suppressWarnings({
      if(encoding=="auto")
        d = vroom::vroom_lines(file.load)
      else
        d = readLines(file.load, encoding=encoding)
      if(header=="auto")
        header = nchar(d[1]) < 0.2 * nchar(d[2])
      if(header)
        dt = data.table(x=d[-1])
      else
        dt = data.table(x=d)
      rm(d)
    })
    if(verbose)
      Print("Preprocessing... ({nrow(dt)} words)")
    dt[, `:=`(
      word = str_split(dt$x, sep, n=2, simplify=TRUE)[,1],
      vec = do.call("list", lapply(
        str_split(dt$x, sep, n=2, simplify=TRUE)[,2], function(v) {
          as.numeric(cc(v, sep=sep))
        }))
    )]
    dt$x = NULL
    dims = unique(sapply(dt$vec, length))
    if(length(dims) > 1)
      warning("The number of dimensions is not consistent between words!", call.=FALSE)
    ndim = length(dt[[1, "vec"]])
    if(verbose)
      Print("Word vectors data: {nrow(dt)} words, {ndim} dimensions (time cost = {dtime(t0, 'auto')})")
  }
  class(dt) = c("wordvec", "data.table", "data.frame")
  if(!missing(file.save)) {
    t1 = Sys.time()
    k = 9  # coefficient for time estimate (based on preprocessing time cost)
    est.time = format(difftime(Sys.time(), t0, units='mins') * k, digits=1, nsmall=0)
    if(verbose)
      Print("\n\n\nCompressing and saving... (estimated time cost ~= {est.time})")
    gc()  # Garbage Collection: Free the Memory
    compress = switch(compress,
                      `1`="gzip",
                      `2`="bzip2",
                      `3`="xz",
                      compress)
    save(dt, file=file.save,
         compress=compress,
         compression_level=compress.level)
    if(verbose)
      cli::cli_alert_success("Saved to \"{file.save}\" (time cost = {dtime(t1, 'mins')})")
  }
  gc()  # Garbage Collection: Free the Memory
  if(verbose)
    Print("\n\n\n****** Total time cost: {dtime(t0, 'mins')} ******")
  invisible(dt)
}


#' Load word vectors data from an ".RData" file.
#'
#' @inheritParams data_transform
#' @param file.load File name (must be .RData transformed by
#' \code{\link{data_transform}}).
#' @param normalize Normalize all word vectors to unit length?
#' Defaults to \code{FALSE}. See \code{\link{data_wordvec_normalize}}.
#'
#' @return
#' A \code{data.table} (of new class \code{wordvec}) with two variables:
#' \describe{
#'   \item{\code{word}}{words (tokens)}
#'   \item{\code{vec}}{\strong{raw} \emph{or} \strong{normalized} word vectors}
#' }
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_normalize}}
#'
#' \code{\link{data_wordvec_reshape}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' \dontrun{
#' # please first manually download the .RData file
#' # (see https://psychbruce.github.io/WordVector_RData.pdf)
#' # or transform plain text data by using `data_transform()`
#'
#' # the RData file must be on your disk
#' # the following code cannot run unless you have the file
#' library(bruceR)
#' set.wd()
#' d = data_wordvec_load("GloVe/glove_wiki_50d.RData")
#' }
#'
#' @export
data_wordvec_load = function(file.load, normalize=FALSE,
                             verbose=TRUE) {
  t0 = Sys.time()
  check_load_validity(file.load)
  if(verbose) cat("Loading...")
  envir = new.env()
  load(file=file.load, envir=envir)
  if(length(ls(envir)) > 1)
    warning("RData file contains multiple objects. Return the first object.", call.=FALSE)
  data = get(ls(envir)[1], envir)
  rm(envir)
  if(!all(c("word", "vec") %in% names(data)))
    stop("Data file must be preprocessed using `data_transform()`!", call.=FALSE)
  ndim = length(data[[1, "vec"]])
  attr(data, "dims") = ndim
  attr(data, "normalized") = FALSE
  class(data) = c("wordvec", "data.table", "data.frame")
  if(verbose) {
    cat("\015")
    cli::cli_alert_success("Word vector data: {nrow(data)} words, {ndim} dims (loading time: {dtime(t0)})")
  }
  if(normalize) data = data_wordvec_normalize(data, verbose)
  gc()  # Garbage Collection: Free the Memory
  return(data)
}


#' Normalize all word vectors to unit length.
#'
#' @description
#' L2-normalization (scaling to unit euclidean length):
#' the \emph{norm} of each vector in the vector space will be normalized to 1.
#'
#' R formula: \code{normalized_vec = vec / sqrt(sum(vec^2))}
#'
#' \emph{Note}: Normalization does not change the results of cosine similarity and
#' can make the computation faster.
#'
#' @inheritParams data_transform
#' @param data A \code{data.table} (of new class \code{wordvec})
#' loaded by \code{\link{data_wordvec_load}}.
#'
#' @return
#' A \code{data.table} (of new class \code{wordvec}) with \strong{normalized} word vectors.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_load}}
#'
#' \code{\link{data_wordvec_reshape}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#'
#' data_wordvec_normalize(d)  # already normalized
#'
#' @export
data_wordvec_normalize = function(data, verbose=TRUE) {
  check_data_validity(data)
  if(attr(data, "normalized")) {
    if(verbose) cli::cli_alert_warning("Word vectors have already been normalized.")
  } else {
    data = normalize(data)
    if(verbose) cli::cli_alert_success("All word vectors have now been normalized.")
  }
  gc()  # Garbage Collection: Free the Memory
  invisible(data)
}


normalize = function(data) {
  # L2-normalized (unit euclidean length)
  vec = NULL
  data[, vec := lapply(vec, function(vec) { vec / sqrt(sum(vec^2)) } )]
  attr(data, "normalized") = TRUE
  return(data)
}


#' Reshape word vectors data.
#'
#' Reshape word vectors data from dense (\code{data.table})
#' to plain (\code{matrix}) or vice versa.
#' For easier use, two wrappers
#' \code{as_matrix()} and \code{as_wordvec()}
#' are also provided.
#'
#' @inheritParams data_wordvec_load
#' @param x Data to be reshaped, could be a \code{matrix}, \code{data.frame}, or \code{data.table}. See examples.
#' @param to Options include:
#' \itemize{
#'   \item{\code{"plain"} (default) reshapes the data
#'   from \code{data.table} (with two variables \code{word} and \code{vec},
#'   loaded by \code{\link{data_wordvec_load}})
#'   to \code{matrix} (with dimensions as columns and words as row names).}
#'   \item{\code{"dense"} just does the reverse.}
#' }
#'
#' @return
#' A \code{data.table} (dense) or \code{matrix} (plain) of word vectors.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_load}}
#'
#' \code{\link{data_wordvec_normalize}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' d = head(demodata, 10)
#' d
#'
#' d.plain = as_matrix(d)
#' d.plain
#'
#' d.dense = as_wordvec(d.plain)
#' d.dense  # identical to `d`
#'
#' @export
data_wordvec_reshape = function(x, to=c("plain", "dense"),
                                normalize=FALSE,
                                verbose=TRUE) {
  to = match.arg(to)
  data = x
  rm(x)
  if(to == "plain") {
    check_data_validity(data)
    if(normalize) data = data_wordvec_normalize(data, verbose)
    data.new = do.call(rbind, lapply(data$vec, function(x) {
      as.matrix(t(x))  # matrix is much faster
    }))
    row.names(data.new) = data$word
  }
  if(to == "dense") {
    data = as.matrix(data)  # much faster
    data.new = data.table(
      word = row.names(data),
      vec = do.call("list", lapply(
        1:nrow(data), function(i) {
          as.numeric(data[i,])
        }))
    )
    attr(data.new, "dims") = length(data.new[[1, "vec"]])
    attr(data.new, "normalized") = FALSE
    class(data.new) = c("wordvec", "data.table", "data.frame")
    if(normalize) data.new = data_wordvec_normalize(data.new, verbose)
  }
  gc()  # Garbage Collection: Free the Memory
  return(data.new)
}


#' @rdname data_wordvec_reshape
#' @export
as_wordvec = function(x, normalize=FALSE) {
  data_wordvec_reshape(x, to="dense", normalize=normalize)
}


#' @rdname data_wordvec_reshape
#' @export
as_matrix = function(x, normalize=FALSE) {
  data_wordvec_reshape(x, to="plain", normalize=normalize)
}


#' Extract a subset of word vectors data.
#'
#' @description
#' Extract a subset of word vectors data.
#' You may specify either a \code{data.table} loaded by \code{\link{data_wordvec_load}})
#' or an .RData file transformed by \code{\link{data_transform}}).
#'
#' @inheritParams data_transform
#' @param x Can be one of the following:
#' \itemize{
#'   \item{a \code{data.table} (of new class \code{wordvec}) loaded by \code{\link{data_wordvec_load}}}
#'   \item{an .RData file transformed by \code{\link{data_transform}}}
#' }
#' @param words [Option 1] Word strings (\code{NULL}; a single word; a vector of words).
#' @param pattern [Option 2] Pattern of regular expression (see \code{\link[stringr:str_subset]{str_subset}}).
#'
#' @return
#' A subset of word vectors data of valid (available) words.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_load}}
#'
#' \code{\link{data_wordvec_normalize}}
#'
#' \code{\link{data_wordvec_reshape}}
#'
#' @examples
#' ## specify `x` as a data.table:
#' d = data_wordvec_subset(demodata, c("China", "Japan", "Korea"))
#' d
#'
#' ## specify `x` and `pattern`, and save with `file.save`:
#' data_wordvec_subset(demodata, pattern="Chin[ae]|Japan|Korea",
#'                     file.save="subset.RData")
#'
#' ## load the subset:
#' d.subset = data_wordvec_load("subset.RData")
#' d.subset
#'
#' ## specify `x` as an .RData file and save with `file.save`:
#' data_wordvec_subset("subset.RData",
#'                     words=c("China", "Chinese"),
#'                     file.save="new.subset.RData")
#' d.new.subset = data_wordvec_load("new.subset.RData")
#' d.new.subset
#'
#' unlink("subset.RData")  # delete file for code check
#' unlink("new.subset.RData")  # delete file for code check
#'
#' @export
data_wordvec_subset = function(x, words=NULL, pattern=NULL,
                               file.save,
                               compress="bzip2",
                               compress.level=9,
                               verbose=TRUE) {
  if(!missing(file.save)) check_save_validity(file.save)
  if(is.data.table(x)) {
    data = x
    check_data_validity(data)
  } else if(is.character(x)) {
    file.load = x
    if(!str_detect(file.load, "\\.rda$|\\.[Rr][Dd]ata$"))
      stop("`x` must be .RData!", call.=FALSE)
    data = data_wordvec_load(file.load)
  } else {
    stop("`x` must be one of them:
      - a `data.table` loaded by `data_wordvec_load()`
      - an .RData file transformed by `data_transform()`", call.=FALSE)
  }
  words.valid = extract_valid_words(data, words, pattern)
  dt = data[word %in% words.valid]  # much faster
  if(!missing(file.save)) {
    t1 = Sys.time()
    if(verbose)
      Print("\n\n\nCompressing and saving...")
    compress = switch(compress,
                      `1`="gzip",
                      `2`="bzip2",
                      `3`="xz",
                      compress)
    save(dt, file=file.save,
         compress=compress,
         compression_level=compress.level)
    if(verbose)
      cli::cli_alert_success("Saved to \"{file.save}\" (time cost = {dtime(t1, 'auto')})")
  }
  gc()  # Garbage Collection: Free the Memory
  invisible(dt)
}


#### Demo Data ####


if(FALSE) {
  library(PsychWordVec)
  d1 = data_wordvec_load("data-raw/GoogleNews/word2vec_googlenews_eng_1word.RData",
                         normalize=FALSE)
  # demodata = head(d1[!str_detect(word, "[^A-Za-z]")], 10000)  # Size < 5MB is OK!
  # demodata = head(d1[!str_detect(word, "[^A-Za-z]")], 50000)
  # bruceR::export(demodata[, .(word)], "data-raw/demodata_1.xlsx")
  filter = bruceR::import("data-raw/demodata_filter.xlsx", as="data.table")
  demodata = d1[word %in% filter[use==1]$word]
  class(demodata) = c("wordvec", "data.table", "data.frame")
  usethis::use_data(demodata, overwrite=TRUE, compress="xz")

  # d2 = data_wordvec_load("data-raw/GoogleNews/word2vec_googlenews_eng_2words.RData",
  #                        normalize=FALSE)
  # bruceR::export(d2[1:20000, .(word)], "data-raw/demodata_2.xlsx")
}


#' Demo data (corpus: Google News; algorithm: word2vec; vocabulary: 8000; dimensions: 300).
#'
#' @description
#' This demo data contains a sample of 8000 English words
#' with their 300-d word embeddings (word vectors) trained
#' using the "word2vec" algorithm based on the Google News corpus.
#' Most of these words are from the Top 8000 frequent wordlist,
#' whereas a few are selected from less frequent words and appended.
#'
#' @format
#' A \code{data.table} (of new class \code{wordvec}) with two variables \code{word} and \code{vec},
#' transformed from the raw data (see the URL in Source) into \code{.RData}
#' using the \code{\link{data_transform}} function.
#'
#' @source
#' Google Code - word2vec (\url{https://code.google.com/archive/p/word2vec/})
#'
#' @usage
#' data(demodata)
#'
#' @examples
#' class(demodata)
#' head(demodata, 10)
#' data_wordvec_normalize(demodata)
#'
#' @name demodata
NULL


#### Get Word Vectors ####


#' Extract the word vector of a single word.
#'
#' @inheritParams data_wordvec_normalize
#' @param word Word string (a single word).
#'
#' @return
#' A numeric vector of the word
#' (or \code{NA} if the word does not appear in the data).
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{get_wordvecs}}
#'
#' \code{\link{plot_wordvec}}
#'
#' \code{\link{plot_wordvec_tSNE}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#'
#' v1 = get_wordvec(demodata, "China")  # raw vector
#' v2 = get_wordvec(d, "China")  # normalized vector
#' cor(v1, v2)
#' cosine_similarity(v1, v2)
#'
#' @export
get_wordvec = function(data, word) {
  WORD = word
  check_data_validity(data)
  if(length(WORD)>1)
    stop("Please use `get_wordvecs()` for more than one word.", call.=FALSE)
  di = data[word %in% WORD]
  if(nrow(di)==1) vec = di[[1, "vec"]] else vec = NA
  return(vec)
}


#' Extract the word vectors of multiple words.
#'
#' Extract the word vectors of multiple words,
#' using either wordlist (a vector of words; using \code{words})
#' or regular expression (a pattern of words; using \code{pattern}).
#' If both the \code{words} and \code{pattern} arguments are specified, \code{words} wins.
#'
#' @inheritParams data_wordvec_normalize
#' @inheritParams data_wordvec_subset
#' @param plot Generate a plot to illustrate the word vectors? Defaults to \code{FALSE}.
#' @param plot.dims Dimensions to be plotted (e.g., \code{1:100}).
#' Defaults to \code{NULL} (plot all dimensions).
#' @param plot.step Step for value breaks. Defaults to \code{0.05}.
#' @param plot.border Color of tile border. Defaults to \code{"white"}.
#' To remove the border color, set \code{plot.border=NA}.
#'
#' @return
#' A \code{data.table} with words as columns and dimensions as rows.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{get_wordvec}}
#'
#' \code{\link{plot_wordvec}}
#'
#' \code{\link{plot_wordvec_tSNE}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#'
#' get_wordvecs(d, c("China", "Japan", "Korea"))
#' get_wordvecs(d, cc(" China, Japan; Korea "))
#'
#' ## specify `pattern`:
#' get_wordvecs(d, pattern="Chin[ae]|Japan|Korea")
#'
#' ## plot word vectors:
#' get_wordvecs(d, cc("China, Japan, Korea,
#'                     Mac, Linux, Windows"),
#'              plot=TRUE, plot.dims=1:100)
#'
#' \donttest{## a more complex example:
#'
#' words = cc("
#' China
#' Chinese
#' Japan
#' Japanese
#' good
#' bad
#' great
#' terrible
#' morning
#' evening
#' king
#' queen
#' man
#' woman
#' he
#' she
#' cat
#' dog
#' ")
#'
#' dt = get_wordvecs(
#'   d, words,
#'   plot=TRUE,
#'   plot.dims=1:100,
#'   plot.step=0.06)
#'
#' # if you want to change something:
#' attr(dt, "ggplot") +
#'   scale_fill_viridis_b(n.breaks=10, show.limits=TRUE) +
#'   theme(legend.key.height=unit(0.1, "npc"))
#'
#' # or to save the plot:
#' ggsave(attr(dt, "ggplot"),
#'        filename="wordvecs.png",
#'        width=8, height=5, dpi=500)
#' unlink("wordvecs.png")  # delete file for code check
#' }
#' @export
get_wordvecs = function(data, words=NULL, pattern=NULL,
                        plot=FALSE,
                        plot.dims=NULL,
                        plot.step=0.05,
                        plot.border="white") {
  check_data_validity(data)
  words.valid = extract_valid_words(data, words, pattern)
  data.subset = data[word %in% words.valid]
  dt = do.call(cbind, lapply(words.valid, function(wi) {
    di = data.table(word = data.subset[word %in% wi][[1, "vec"]])
    names(di) = wi
    return(di)
  }))
  if(plot) {
    p = plot_wordvec(dt, dims=plot.dims,
                     step=plot.step, border=plot.border)
    attr(dt, "ggplot") = p
    print(p)
  }
  return(dt)
}


#' Visualize word vectors.
#'
#' @param dt A \code{data.table} returned by \code{\link{get_wordvecs}}
#' or loaded by \code{\link{data_wordvec_load}}.
#' @param dims Dimensions to be plotted (e.g., \code{1:100}).
#' Defaults to \code{NULL} (plot all dimensions).
#' @param step Step for value breaks. Defaults to \code{0.05}.
#' @param border Color of tile border. Defaults to \code{"white"}.
#' To remove the border color, set \code{border=NA}.
#'
#' @return
#' A \code{ggplot} object.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{plot_wordvec_tSNE}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#'
#' dt = get_wordvecs(d, cc("king, queen, man, woman"))
#' dt[, QUEEN := king - man + woman]
#' dt[, QUEEN := QUEEN / sqrt(sum(QUEEN^2))]  # normalize
#' names(dt)[5] = "king - man + woman"
#' plot_wordvec(dt[, c(1,3,4,5,2)], dims=1:50)
#'
#' dt = get_wordvecs(d, cc("boy, girl, he, she"))
#' dt[, GIRL := boy - he + she]
#' dt[, GIRL := GIRL / sqrt(sum(GIRL^2))]  # normalize
#' names(dt)[5] = "boy - he + she"
#' plot_wordvec(dt[, c(1,3,4,5,2)], dims=1:50)
#'
#' \donttest{dt = get_wordvecs(d, cc("
#'   male, man, boy, he, his,
#'   female, woman, girl, she, her"))
#'
#' p = plot_wordvec(dt, dims=1:100)
#'
#' # if you want to change something:
#' p + theme(legend.key.height=unit(0.1, "npc"))
#'
#' # or to save the plot:
#' ggsave(p, filename="wordvecs.png",
#'        width=8, height=5, dpi=500)
#' unlink("wordvecs.png")  # delete file for code check
#' }
#' @export
plot_wordvec = function(dt, dims=NULL, step=0.05, border="white") {
  if(!is.null(attr(dt, "normalized")))
    dt = as.data.table(t(data_wordvec_reshape(dt)))
  if(!is.null(dims)) dt = dt[dims, ]
  steps = step*(0:100)
  breaks = sort(unique(c(steps, -steps)))
  max = max(abs(range(dt)))
  max = steps[max(which(max > steps)) + 1]
  dp = melt(dt, measure.vars=names(dt),
            variable.name="word", value.name="value")
  dp$dim = rep(1:nrow(dt), length(dt))
  value = NULL
  ggplot(dp, aes(x=dim, y=factor(word, levels=rev(names(dt))))) +
    geom_tile(aes(fill=value), color=border) +
    scale_x_discrete(expand=expansion()) +
    scale_y_discrete(expand=expansion()) +
    # scale_fill_binned(type="viridis", n.breaks=8, show.limits=TRUE) +
    # scale_fill_viridis_b(n.breaks=8, show.limits=TRUE) +
    # scale_fill_steps2(n.breaks=8, show.limits=TRUE) +
    scale_fill_steps2(breaks=breaks, limits=c(-max, max), show.limits=TRUE) +
    labs(x="Dimension", y="Word", fill=NULL,
         title=paste0("Word Vector (",
                      ifelse(is.null(dims), "", "Subset of "),
                      nrow(dt),
                      " Dimensions)")) +
    theme_void(base_size=12) +
    theme(axis.ticks.y=element_line(0.5, color="grey", lineend="butt"),
          axis.ticks.length.y=unit(0.2, "lines"),
          axis.text.y=element_text(size=12, hjust=1,
                                   margin=margin(0, 0.5, 0, 1, "lines")),
          legend.key.height=unit(0.15, "npc"),
          legend.box.spacing=unit(0.5, "lines"),
          plot.title=element_text(hjust=0.5,
                                  margin=margin(0, 0, 0.5, 0, "lines")),
          plot.margin=margin(0.05, 0.02, 0.05, 0.01, "npc"),
          plot.background=element_rect(fill="white"))
}


#' Visualize word vectors with dimensionality reduced using t-SNE.
#'
#' Visualize word vectors with dimensionality reduced
#' using the t-Distributed Stochastic Neighbor Embedding (t-SNE) method
#' (i.e., projecting high-dimensional vectors into a low-dimensional vector space),
#' implemented by \code{\link[Rtsne:Rtsne]{Rtsne::Rtsne()}}.
#' You should specify a random seed if you expect reproducible results.
#'
#' @inheritParams plot_wordvec
#' @param dims Output dimensionality: \code{2} (default, the most common choice) or \code{3}.
#' @param perplexity Perplexity parameter, should not be larger than (number of words - 1) / 3.
#' Defaults to \code{floor((length(dt)-1)/3)} (where columns of \code{dt} are words).
#' See the \code{\link[Rtsne:Rtsne]{Rtsne}} package for details.
#' @param theta Speed/accuracy trade-off (increase for less accuracy), set to 0 for exact t-SNE. Defaults to 0.5.
#' @param colors A character vector specifying (1) the categories of words (for 2-D plot only)
#' or (2) the exact colors of words (for 2-D and 3-D plot). See examples for its usage.
#' @param seed Random seed for reproducible results. Defaults to \code{NULL}.
#' @param custom.Rtsne User-defined \code{\link[Rtsne:Rtsne]{Rtsne}} object using the same \code{dt}.
#'
#' @return
#' 2-D: A \code{ggplot} object.
#' You may extract the data from this object using \code{$data}.
#'
#' 3-D: Nothing but only the data was invisibly returned,
#' because \code{\link[rgl:plot3d]{rgl::plot3d()}} is
#' "called for the side effect of drawing the plot"
#' and thus cannot return any 3-D plot object.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @references
#' Hinton, G. E., & Salakhutdinov, R. R. (2006).
#' Reducing the dimensionality of data with neural networks.
#' \emph{Science, 313}(5786), 504--507.
#'
#' van der Maaten, L., & Hinton, G. (2008).
#' Visualizing data using t-SNE.
#' \emph{Journal of Machine Learning Research, 9}, 2579--2605.
#'
#' @seealso
#' \code{\link{plot_wordvec}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#'
#' dt = get_wordvecs(d, cc("
#'   man, woman,
#'   king, queen,
#'   China, Beijing,
#'   Japan, Tokyo"))
#'
#' ## 2-D (default):
#' plot_wordvec_tSNE(dt, seed=1234)
#'
#' plot_wordvec_tSNE(dt, seed=1234)$data
#'
#' colors = c(rep("#2B579A", 4), rep("#B7472A", 4))
#' plot_wordvec_tSNE(dt, colors=colors, seed=1234)
#'
#' \donttest{category = c(rep("gender", 4), rep("country", 4))
#' plot_wordvec_tSNE(dt, colors=category, seed=1234) +
#'   scale_x_continuous(limits=c(-200, 200),
#'                      labels=function(x) x/100) +
#'   scale_y_continuous(limits=c(-200, 200),
#'                      labels=function(x) x/100) +
#'   scale_color_manual(values=c("#B7472A", "#2B579A"))
#'
#' ## 3-D:
#' colors = c(rep("#2B579A", 4), rep("#B7472A", 4))
#' plot_wordvec_tSNE(dt, dims=3, colors=colors, seed=1)
#' }
#' @export
plot_wordvec_tSNE = function(dt, dims=2,
                             perplexity, theta=0.5,
                             colors=NULL, seed=NULL,
                             custom.Rtsne=NULL) {
  if(!is.null(attr(dt, "normalized")))
    dt = as.data.table(t(data_wordvec_reshape(dt)))
  if(missing(perplexity))
    perplexity = floor((length(dt)-1)/3)
  if(is.null(custom.Rtsne)) {
    if(length(dt) < 4)
      stop("`dt` must contain at least 4 words (columns).", call.=FALSE)
    set.seed(seed)
    sne = Rtsne::Rtsne(as.data.frame(t(dt)), dims=dims,
                       perplexity=perplexity, theta=theta)
  } else {
    if(!inherits(custom.Rtsne, "Rtsne"))
      stop("`custom.Rtsne` must be an `Rtsne` object.", call.=FALSE)
    sne = custom.Rtsne
  }
  dp = cbind(data.frame(word=names(dt)),
             as.data.frame(sne$Y))
  if(dims == 2) {
    V1 = V2 = word = NULL
    p = ggplot(dp, aes(x=V1, y=V2, color=colors)) +
      geom_hline(yintercept=0, color="grey") +
      geom_vline(xintercept=0, color="grey") +
      geom_point(show.legend=!all(grepl("#", colors))) +
      ggrepel::geom_text_repel(aes(label=word),
                               seed=ifelse(is.null(seed),
                                           NA, seed),
                               show.legend=FALSE) +
      labs(x="t-SNE Dimension 1", y="t-SNE Dimension 2",
           color="Category",
           title="Word Vectors (t-SNE for Dimensionality Reduction)") +
      bruceR::theme_bruce()
    if(all(grepl("#", colors)))
      p = p + scale_color_manual(values=sort(unique(colors)))
    return(p)
  } else if(dims == 3) {
    rgl::open3d()
    rgl::plot3d(x=dp$V1, y=dp$V2, z=dp$V3,
                xlab="t-SNE Dimension 1",
                ylab="t-SNE Dimension 2",
                zlab="t-SNE Dimension 3",
                xlim=max(abs(range(dp$V1)))*1.2*c(-1,1),
                ylim=max(abs(range(dp$V2)))*1.2*c(-1,1),
                zlim=max(abs(range(dp$V3)))*1.2*c(-1,1),
                col=colors,
                size=8)
    rgl::text3d(x=dp$V1, y=dp$V2, z=dp$V3,
                color=colors,
                adj=c(1, 1, 1),
                texts=dp$word)
    invisible(dp)
  } else {
    stop("Please directly use `Rtsne::Rtsne()` for more than 3 dimensions:
       sne = Rtsne::Rtsne(as.data.frame(t(dt)), dims=dims)", call.=FALSE)
  }
}


#### Word Similarity Analysis ####


#' Find the Top-N most similar words.
#'
#' Find the Top-N most similar words, which replicates the results produced
#' by the Python \code{gensim} module \code{most_similar()} function.
#' (Exact replication of \code{gensim} requires the same word vectors data,
#' not the \code{demodata} used here in examples.)
#'
#' @inheritParams get_wordvec
#' @inheritParams data_transform
#' @param x Can be one of the following:
#' \itemize{
#'   \item{a single word:
#'
#'   \code{"China"}}
#'
#'   \item{a list of words:
#'
#'   \code{c("king", "queen")}}
#'
#'   \code{cc(" king , queen ; man | woman")}
#'
#'   \item{an R formula (\code{~ xxx}) specifying
#'   words that positively and negatively
#'   contribute to the similarity (for word analogy):
#'
#'   \code{~ boy - he + she}
#'
#'   \code{~ king - man + woman}
#'
#'   \code{~ Beijing - China + Japan}}
#' }
#' @param topn Top-N most similar words. Defaults to \code{10}.
#' @param keep Keep words specified in \code{x} in results?
#' Defaults to \code{FALSE}.
#' @param above Defaults to \code{NULL}. Can be one of the following:
#' \itemize{
#'   \item{a threshold value to find all words with cosine similarities
#'   higher than this value}
#'   \item{a critical word to find all words with cosine similarities
#'   higher than that with this critical word}
#' }
#' If both \code{topn} and \code{above} are specified, \code{above} wins.
#'
#' @return
#' A \code{data.table} with the most similar words and their cosine similarities.
#' The row number of each word in the raw data is also returned,
#' which may help determine the relative word frequency in some cases.
#'
#' Two attributes are appended to the returned \code{data.table} (see examples):
#' \code{wordvec} and \code{wordvec.formula}.
#' Users may extract them for further use.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{cosine_similarity}}
#'
#' \code{\link{pair_similarity}}
#'
#' \code{\link{tab_similarity}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#'
#' most_similar(d, "China")
#' most_similar(d, c("king", "queen"))
#' most_similar(d, cc(" king , queen ; man | woman "))
#'
#' \donttest{# the same as above:
#' most_similar(d, ~ China)
#' most_similar(d, ~ king + queen)
#' most_similar(d, ~ king + queen + man + woman)
#'
#' most_similar(d, ~ boy - he + she)
#' most_similar(d, ~ Jack - he + she)
#' most_similar(d, ~ Rose - she + he)
#'
#' most_similar(d, ~ king - man + woman)
#' most_similar(d, ~ Tokyo - Japan + China)
#' most_similar(d, ~ Beijing - China + Japan)
#'
#' most_similar(d, "China", above=0.7)
#' most_similar(d, "China", above="Shanghai")
#'
#' # automatically normalized for more accurate results
#' ms = most_similar(demodata, ~ king - man + woman)
#' ms
#' str(ms)
#' attr(ms, "dims")
#' attr(ms, "normalized")
#' attr(ms, "wordvec.formula")
#' attr(ms, "wordvec")
#' # final word vector computed according to the formula
#' }
#' @export
most_similar = function(data, x, topn=10,
                        keep=FALSE, above=NULL,
                        verbose=TRUE) {
  if(attr(data, "normalized")==FALSE) {
    cli::cli_alert_warning("Results may be inaccurate if word vectors are not normalized.")
    data = data_wordvec_normalize(data, verbose)  # pre-normalized
  }
  ms = data.table()
  if(inherits(x, "character"))
    f = stats::as.formula(paste("~", paste(x, collapse="+")))
  else if(inherits(x, "formula"))
    f = x
  else
    stop("`x` must be a character vector or an R formula!", call.=FALSE)
  xt = str_replace_all(as.character(f[2]), "`", "")
  xts = str_split(xt, " (?=[+-])", simplify=TRUE)[1,]
  positive = str_remove(str_subset(xts, "^\\-", negate=TRUE), "\\+ *")
  negative = str_remove(str_subset(xts, "^\\-"), "\\- *")
  x = c(positive, negative)
  wordvecs.pos = get_wordvecs(data, words=positive)
  wordvecs.neg = get_wordvecs(data, words=negative)
  if(length(wordvecs.neg) == 0)
    wordvec = rowSums(wordvecs.pos)
  else
    wordvec = rowSums(wordvecs.pos) - rowSums(wordvecs.neg)
  wordvec = wordvec / sqrt(sum(wordvec^2))  # post-normalized
  cos_sim = NULL
  data$cos_sim = sapply(data$vec, function(vec_i) {
    sum(wordvec * vec_i)  # faster for normalized vectors
  })
  data$row_id = 1:nrow(data)
  if(keep==FALSE)
    data = data[word %notin% x]
  if(is.null(above)) {
    ms = utils::head(data[order(-cos_sim), c("word", "cos_sim", "row_id")], topn)
  } else if(is.numeric(above)) {
    ms = data[order(-cos_sim), c("word", "cos_sim", "row_id")][cos_sim >= above]
  } else if(is.character(above)) {
    ms = data[order(-cos_sim), c("word", "cos_sim", "row_id")][
      cos_sim >= sum(wordvec * get_wordvec(data, above))
    ]
  } else {
    stop("`above` must be a numeric value or a character string.", call.=FALSE)
  }
  gc()  # Garbage Collection: Free the Memory
  attr(ms, "wordvec") = wordvec
  attr(ms, "wordvec.formula") = f
  if(verbose) {
    Print("<<cyan [Word Vector]>> =~ {as.character(f[2])}")
    message("(normalized to unit length)")
  }
  return(ms)
}


#' Compute cosine similarity/distance for a pair of words.
#'
#' @inheritParams cosine_similarity
#' @inheritParams get_wordvec
#' @param word1,word2 Word string (a single word).
#'
#' @return A value of cosine similarity/distance.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{cosine_similarity}}
#'
#' \code{\link{tab_similarity}}
#'
#' \code{\link{most_similar}}
#'
#' @examples
#' pair_similarity(demodata, "China", "Chinese")
#'
#' @export
pair_similarity = function(data, word1, word2, distance=FALSE) {
  check_data_validity(data)
  dt = get_wordvecs(data, c(word1, word2))
  cosine_similarity(dt[[1]], dt[[2]], distance=distance)
}


#' Tabulate data for cosine similarity/distance of all word pairs.
#'
#' @inheritParams cosine_similarity
#' @inheritParams get_wordvecs
#' @param unique Word pairs: unique pairs (\code{TRUE})
#' or full pairs with duplicates (\code{FALSE}; default).
#'
#' @return
#' A \code{data.table} of all words and
#' their unique or full pairs,
#' with their cosine similarity (\code{cos_sim})
#' or cosine distance (\code{cos_dist}).
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{cosine_similarity}}
#'
#' \code{\link{pair_similarity}}
#'
#' \code{\link{most_similar}}
#'
#' \code{\link{test_WEAT}}
#'
#' \code{\link{test_RND}}
#'
#' @examples
#' tab_similarity(demodata, cc("king, queen, man, woman"))
#' tab_similarity(demodata, cc("king, queen, man, woman"),
#'                unique=TRUE)
#'
#' tab_similarity(demodata, cc("Beijing, China, Tokyo, Japan"))
#' tab_similarity(demodata, cc("Beijing, China, Tokyo, Japan"),
#'                unique=TRUE)
#'
#' ## select n1 * n2 word pairs crossing two word lists w1 & w2
#' ## using data.table syntax
#' w1 = cc("king, queen")
#' w2 = cc("man, woman")
#' tab_similarity(demodata, c(w1, w2)) %>%
#'   .[word1 %in% w1 & word2 %in% w2]
#'
#' @export
tab_similarity = function(data, words=NULL, pattern=NULL,
                          unique=FALSE, distance=FALSE) {
  check_data_validity(data)
  words.valid = extract_valid_words(data, words, pattern)
  dt = data[word %in% words.valid]
  if(unique) {
    words.mat = utils::combn(words.valid, 2)
    dts = data.table(
      word1 = words.mat[1,],
      word2 = words.mat[2,]
    )
  } else {
    dts = as.data.table(expand.grid(
      word2 = words.valid,
      word1 = words.valid
    )[c("word1", "word2")])
  }
  word1 = word2 = wordpair = NULL
  dts[, wordpair := paste0(word1, "-", word2)]
  dts$cos_sim = sapply(1:nrow(dts), function(i) {
    cosine_similarity(get_wordvec(dt, dts[[i, 1]]),
                      get_wordvec(dt, dts[[i, 2]]),
                      distance=distance)
  })
  if(distance) names(dts)[4] = "cos_dist"
  return(dts)
}


#### Word Association Test ####


#' Word Embedding Association Test (WEAT) and Single-Category WEAT.
#'
#' Tabulate data (cosine similarity and standardized effect size) and
#' conduct the permutation test of significance for the
#' \emph{Word Embedding Association Test} (WEAT) and
#' \emph{Single-Category Word Embedding Association Test} (SC-WEAT).
#' \itemize{
#'   \item{For WEAT, two-samples permutation test is conducted (i.e., rearrangements of data).}
#'   \item{For SC-WEAT, one-sample permutation test is conducted (i.e., rearrangements of +/- signs to data).}
#' }
#'
#' @inheritParams tab_similarity
#' @param T1,T2 Target words (a vector of words or a pattern of regular expression).
#' If only \code{T1} is specified,
#' it will tabulate data for single-category WEAT (SC-WEAT).
#' @param A1,A2 Attribute words (a vector of words or a pattern of regular expression).
#' Both must be specified.
#' @param use.pattern Defaults to \code{FALSE} (using a vector of words).
#' If you use regular expression in \code{T1}, \code{T2}, \code{A1}, and \code{A2},
#' please specify this argument as \code{TRUE}.
#' @param labels Labels for target and attribute concepts (a named \code{list}),
#' such as (the default)
#' \code{list(T1="Target1", T2="Target2", A1="Attrib1", A2="Attrib2")}.
#' @param p.perm Permutation test to get exact or approximate \emph{p} value of the overall effect.
#' Defaults to \code{TRUE}. See also the \code{\link[sweater:weat_exact]{sweater}} package.
#' @param p.nsim Number of samples for resampling in permutation test. Defaults to \code{10000}.
#'
#' If \code{p.nsim} is larger than the number of all possible permutations (rearrangements of data),
#' then it will be ignored and an exact permutation test will be conducted.
#' Otherwise (in most cases for real data and always for SC-WEAT), a resampling test is performed,
#' which takes much less computation time and produces the approximate \emph{p} value (comparable to the exact one).
#' @param p.side One-sided (\code{1}) or two-sided (\code{2}) \emph{p} value.
#' Defaults to \code{2}.
#'
#' In Caliskan et al.'s (2017) article, they reported one-sided \emph{p} value for WEAT.
#' Here, I suggest reporting two-sided \emph{p} value as a more conservative estimate.
#' The users take the full responsibility for the choice.
#' \itemize{
#'   \item{The one-sided \emph{p} value is calculated as the proportion of sampled permutations
#'         where the difference in means is greater than the test statistic.}
#'   \item{The two-sided \emph{p} value is calculated as the proportion of sampled permutations
#'         where the absolute difference is greater than the test statistic.}
#' }
#' @param seed Random seed for reproducible results of permutation test. Defaults to \code{NULL}.
#' @param pooled.sd Method used to calculate the pooled \emph{SD} for effect size estimate in WEAT.
#' \itemize{
#'   \item{Defaults to \code{"Caliskan"}: \code{sd(data.diff$cos_sim_diff)}, which is highly suggested
#'         and identical to Caliskan et al.'s (2017) original approach.}
#'   \item{Otherwise specified, it will calculate the pooled \emph{SD} as:
#'         \eqn{\sqrt{[(n_1 - 1) * \sigma_1^2 + (n_2 - 1) * \sigma_2^2] / (n_1 + n_2 - 2)}}.
#'
#'         This is \strong{NOT suggested} because it may \emph{overestimate} the effect size,
#'         especially when there are only a few T1 and T2 words that have small variances.}
#' }
#'
#' @return
#' A \code{list} of objects (of new class \code{weat}):
#' \describe{
#'   \item{\code{words.valid}}{
#'     valid (actually matched) words}
#'   \item{\code{words.not.found}}{
#'     words not found}
#'   \item{\code{data.raw}}{
#'     \code{data.table} of cosine similarities between all word pairs}
#'   \item{\code{data.mean}}{
#'     \code{data.table} of \emph{mean} cosine similarities
#'     \emph{across} all attribute words}
#'   \item{\code{data.diff}}{
#'     \code{data.table} of \emph{differential} mean cosine similarities
#'     \emph{between} the two attribute concepts}
#'   \item{\code{eff.label}}{
#'     description for the difference between the two attribute concepts}
#'   \item{\code{eff.type}}{
#'     effect type: WEAT or SC-WEAT}
#'   \item{\code{eff}}{
#'     raw effect, standardized effect size, and p value (if \code{p.perm=TRUE})}
#' }
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @references
#' Caliskan, A., Bryson, J. J., & Narayanan, A. (2017).
#' Semantics derived automatically from language corpora contain human-like biases.
#' \emph{Science, 356}(6334), 183--186.
#'
#' @seealso
#' \code{\link{tab_similarity}}
#'
#' \code{\link{test_RND}}
#'
#' @examples
#' ## Remember: cc() is more convenient than c()!
#'
#' weat = test_WEAT(
#'   demodata,
#'   labels=list(T1="King", T2="Queen", A1="Male", A2="Female"),
#'   T1=cc("king, King"),
#'   T2=cc("queen, Queen"),
#'   A1=cc("male, man, boy, brother, he, him, his, son"),
#'   A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
#'   seed=1)
#' weat
#'
#' sc_weat = test_WEAT(
#'   demodata,
#'   labels=list(T1="Occupation", A1="Male", A2="Female"),
#'   T1=cc("
#'     architect, boss, leader, engineer, CEO, officer, manager,
#'     lawyer, scientist, doctor, psychologist, investigator,
#'     consultant, programmer, teacher, clerk, counselor,
#'     salesperson, therapist, psychotherapist, nurse"),
#'   A1=cc("male, man, boy, brother, he, him, his, son"),
#'   A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
#'   seed=1)
#' sc_weat
#'
#' \dontrun{
#'
#' ## the same as the first example, but using regular expression
#' weat = test_WEAT(
#'   demodata,
#'   labels=list(T1="King", T2="Queen", A1="Male", A2="Female"),
#'   use.pattern=TRUE,  # use regular expression below
#'   T1="^[kK]ing$",
#'   T2="^[qQ]ueen$",
#'   A1="^male$|^man$|^boy$|^brother$|^he$|^him$|^his$|^son$",
#'   A2="^female$|^woman$|^girl$|^sister$|^she$|^her$|^hers$|^daughter$",
#'   seed=1)
#' weat
#'
#' ## replicating Caliskan et al.'s (2017) results
#' ## WEAT7 (Table 1): d = 1.06, p = .018
#' ## (requiring installation of the `sweater` package)
#' Caliskan.WEAT7 = test_WEAT(
#'   as_wordvec(sweater::glove_math),
#'   labels=list(T1="Math", T2="Arts", A1="Male", A2="Female"),
#'   T1=cc("math, algebra, geometry, calculus, equations, computation, numbers, addition"),
#'   T2=cc("poetry, art, dance, literature, novel, symphony, drama, sculpture"),
#'   A1=cc("male, man, boy, brother, he, him, his, son"),
#'   A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
#'   p.side=1, seed=1234)
#' Caliskan.WEAT7
#' # d = 1.055, p = .0173 (= 173 counts / 10000 permutation samples)
#'
#' ## replicating Caliskan et al.'s (2017) supplemental results
#' ## WEAT7 (Table S1): d = 0.97, p = .027
#' Caliskan.WEAT7.supp = test_WEAT(
#'   demodata,
#'   labels=list(T1="Math", T2="Arts", A1="Male", A2="Female"),
#'   T1=cc("math, algebra, geometry, calculus, equations, computation, numbers, addition"),
#'   T2=cc("poetry, art, dance, literature, novel, symphony, drama, sculpture"),
#'   A1=cc("male, man, boy, brother, he, him, his, son"),
#'   A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
#'   p.side=1, seed=1234)
#' Caliskan.WEAT7.supp
#' # d = 0.966, p = .0221 (= 221 counts / 10000 permutation samples)
#' }
#'
#' @export
test_WEAT = function(data, T1, T2, A1, A2,
                     use.pattern=FALSE,
                     labels=list(),
                     p.perm=TRUE,
                     p.nsim=10000,
                     p.side=2,
                     seed=NULL,
                     pooled.sd="Caliskan") {
  if(!p.side %in% 1:2) stop("`p.side` should be 1 or 2.", call.=FALSE)
  if(missing(A1)) stop("Please specify `A1`.", call.=FALSE)
  if(missing(A2)) stop("Please specify `A2`.", call.=FALSE)
  if(missing(T1)) stop("Please specify `T1`.", call.=FALSE)
  if(missing(T2)) T2 = NULL
  if(length(labels)==0) {
    if(missing(T2))
      labels = list(T1="Target", T2=NA, A1="Attrib1", A2="Attrib2")
    else
      labels = list(T1="Target1", T2="Target2", A1="Attrib1", A2="Attrib2")
  }
  check_data_validity(data)
  if(use.pattern) {
    if(!is.null(T1)) {
      message(Glue("T1 ({labels$T1}):"))
      T1 = names(get_wordvecs(data, pattern=T1))
    }
    if(!is.null(T2)) {
      message(Glue("T2 ({labels$T2}):"))
      T2 = names(get_wordvecs(data, pattern=T2))
    }
    if(!is.null(A1)) {
      message(Glue("A1 ({labels$A1}):"))
      A1 = names(get_wordvecs(data, pattern=A1))
    }
    if(!is.null(A2)) {
      message(Glue("A2 ({labels$A2}):"))
      A2 = names(get_wordvecs(data, pattern=A2))
    }
  }
  words = c(T1, T2, A1, A2)
  if(attr(data, "normalized")==FALSE)
    data = normalize(data[word %in% words])

  dt = get_wordvecs(data, words)
  # words not found:
  not.found = setdiff(words, names(dt))
  # valid words:
  T1 = T1[T1 %in% names(dt)]
  T2 = T2[T2 %in% names(dt)]
  A1 = A1[A1 %in% names(dt)]
  A2 = A2[A2 %in% names(dt)]

  dweat = rbind(
    expand.grid(Target=labels$T1, Attrib=labels$A1, T_word=T1, A_word=A1),
    expand.grid(Target=labels$T1, Attrib=labels$A2, T_word=T1, A_word=A2),
    expand.grid(Target=labels$T2, Attrib=labels$A1, T_word=T2, A_word=A1),
    expand.grid(Target=labels$T2, Attrib=labels$A2, T_word=T2, A_word=A2))
  attr(dweat, "out.attrs") = NULL
  dweat$cos_sim = sapply(1:nrow(dweat), function(i) {
    T_word = as.character(dweat[[i, "T_word"]])
    A_word = as.character(dweat[[i, "A_word"]])
    sum(dt[[T_word]] * dt[[A_word]])
  })
  dweat = as.data.table(dweat)

  . = Target = Attrib = T_word = cos_sim = cos_sim_mean = cos_sim_diff = std_mean = std_diff = NULL

  dweat.mean = dweat[, .(
    Target = Target[1],
    cos_sim_mean = mean(cos_sim)
  ), by=.(T_word, Attrib)]
  dweat.sd = dweat[, .(
    Target = Target[1],
    std_dev = stats::sd(cos_sim)
  ), by=T_word]
  dweat.mean = dweat.mean[order(Target, T_word, Attrib),
                          .(Target, T_word, Attrib, cos_sim_mean)]
  dweat.mean = left_join(dweat.mean, dweat.sd, by=c("Target", "T_word"))
  dweat.mean$std_mean = dweat.mean$cos_sim_mean / dweat.mean$std_dev

  dweat.diff = dweat.mean[, .(
    Target = Target[1],
    cos_sim_diff = cos_sim_mean[1] - cos_sim_mean[2],
    std_diff = std_mean[1] - std_mean[2]
  ), by=T_word]
  dweat.diff = dweat.diff[order(Target, T_word),
                          .(Target, T_word, cos_sim_diff, std_diff)]

  if(!is.null(T2)) {

    # WEAT
    eff_label = list(
      contrast=paste(labels$T1, "vs.", labels$T2, "::", labels$A1, "vs.", labels$A2),
      labels=labels[c("T1", "T2", "A1", "A2")],
      words=sapply(list(T1, T2, A1, A2), length)
    )
    eff_type = "WEAT (Word Embedding Association Test)"
    mean_diffs = dweat.diff[, .(
      mean_diff = mean(cos_sim_diff),
      std_diff = mean(std_diff)
    ), by=Target]
    eff_raw = mean_diffs$mean_diff[1] - mean_diffs$mean_diff[2]
    if(pooled.sd=="Caliskan")
      std_dev = stats::sd(dweat.diff$cos_sim_diff)
    else
      std_dev = pooled_sd(dweat.diff$cos_sim_diff, T1, T2)
    eff_size = eff_raw / std_dev

    if(p.perm) {
      ids = c(rep(TRUE, length(T1)), rep(FALSE, length(T2)))
      set.seed(seed)
      p = p_perm(dweat.diff$cos_sim_diff,
                 ids, eff_raw, p.nsim, p.side)
      set.seed(seed)
      p.T1 = p_perm(dweat.diff[Target==labels$T1]$cos_sim_diff,
                    nsim=p.nsim, side=2)
      set.seed(seed)
      p.T2 = p_perm(dweat.diff[Target==labels$T2]$cos_sim_diff,
                    nsim=p.nsim, side=2)
    } else {
      p = p.T1 = p.T2 = NULL
    }
    eff = data.table(Target=paste0(labels$T1, "/", labels$T2),
                     Attrib=paste0(labels$A1, "/", labels$A2),
                     eff_raw, eff_size, p)

    # Single-Target Tests
    eff.ST = cbind(mean_diffs, data.table(pval=c(p.T1, p.T2)))
    names(eff.ST) = c("Target", "mean_raw_diff",
                      "mean_std_diff", names(p.T1))

  } else {

    # SC-WEAT
    eff_label = list(
      contrast=paste(labels$T1, "::", labels$A1, "vs.", labels$A2),
      labels=labels[c("T1", "A1", "A2")],
      words=sapply(list(T1, A1, A2), length)
    )
    eff_type = "SC-WEAT (Single-Category Word Embedding Association Test)"
    dweat.mean$Target = NULL
    dweat.diff$Target = NULL

    if(p.perm) {
      set.seed(seed)
      p = p_perm(dweat.diff$cos_sim_diff,
                 nsim=p.nsim, side=p.side)
    } else {
      p = NULL
    }
    eff = dweat.diff[, .(
      Target=labels$T1,
      Attrib=paste0(labels$A1, "/", labels$A2),
      eff_raw=mean(cos_sim_diff),
      eff_size=mean(std_diff),
      pval=p)]

  }

  dweat.diff$closer_to = ifelse(dweat.diff$cos_sim_diff > 0, labels$A1, labels$A2)

  names(eff) = c("Target", "Attrib",
                 "mean_diff_raw", "eff_size",
                 names(p))

  weat = list(
    words.valid=list(T1=T1, T2=T2, A1=A1, A2=A2),
    words.not.found=not.found,
    data.raw=dweat,
    data.mean=dweat.mean,
    data.diff=dweat.diff,
    eff.label=eff_label,
    eff.type=eff_type,
    eff=eff
  )
  if(!is.null(T2)) weat = c(weat, list(eff.ST=eff.ST))
  class(weat) = "weat"
  return(weat)
}


#' @export
print.weat = function(x, digits=3, ...) {
  cli::cli_h1("{x$eff.type}")
  cn()
  cat(valid_words_info(x))
  cn(2)
  data.diff = copy(x$data.diff)
  if(!is.null(x$words.valid$T2)) {
    data.diff = data.diff[, c(2, 1, 3, 4, 5)]
    data.diff$Target = fixed_string(data.diff$Target)
  }
  data.diff$T_word = paste0("\"", data.diff$T_word, "\"")
  data.diff$closer_to = fixed_string(data.diff$closer_to)
  names(data.diff)[1] = " "
  print_table(data.diff, row.names=FALSE, digits=digits,
              title="Relative semantic similarities (differences):")
  if(!is.null(x$words.valid$T2)) {
    cn()
    x$eff.ST$Target = fixed_string(x$eff.ST$Target)
    note.ST = ifelse(length(x$eff.ST)<4, "",
                     "Permutation test: approximate p values (forced to two-sided)")
    print_table(x$eff.ST, row.names=FALSE, digits=digits,
                title="Mean differences for single target category:",
                note=note.ST)
  }
  cn()
  x$eff$Attrib = paste0(" ", x$eff$Attrib)
  if(length(x$eff)>=5) {
    p.type = names(x$eff)[5]
    note = paste(
      ifelse(grepl("exact", p.type),
             "Permutation test: exact p value",
             "Permutation test: approximate p value"),
      "=", sprintf("%.2e", x$eff[[5]]),
      ifelse(grepl("1", p.type),
             "(one-sided)",
             "(two-sided)"))
  } else {
    note = "Note: To get p value with permutation test, specify `p.perm=TRUE`"
  }
  print_table(x$eff, row.names=FALSE, digits=digits,
              title="Overall effect (raw and standardized mean differences):",
              note=note)
  cn()
}


#' Relative Norm Distance (RND) analysis.
#'
#' Tabulate data and conduct the permutation test of significance
#' for the \emph{Relative Norm Distance} (RND; also known as \emph{Relative Euclidean Distance}).
#' This is an alternative method to \link[PsychWordVec:test_WEAT]{Single-Category WEAT}.
#'
#' @inheritParams test_WEAT
#' @param T1 Target words of a single category (a vector of words or a pattern of regular expression).
#' @param labels Labels for target and attribute concepts (a named \code{list}),
#' such as (the default)
#' \code{list(T1="Target", A1="Attrib1", A2="Attrib2")}.
#'
#' @return
#' A \code{list} of objects (of new class \code{rnd}):
#' \describe{
#'   \item{\code{words.valid}}{
#'     valid (actually matched) words}
#'   \item{\code{words.not.found}}{
#'     words not found}
#'   \item{\code{data.raw}}{
#'     \code{data.table} of (absolute and relative) norm distances}
#'   \item{\code{eff.label}}{
#'     description for the difference between the two attribute concepts}
#'   \item{\code{eff.type}}{
#'     effect type: RND}
#'   \item{\code{eff}}{
#'     raw effect and p value (if \code{p.perm=TRUE})}
#'   \item{\code{eff.interpretation}}{
#'     interpretation of the RND score}
#' }
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @references
#' Garg, N., Schiebinger, L., Jurafsky, D., & Zou, J. (2018).
#' Word embeddings quantify 100 years of gender and ethnic stereotypes.
#' \emph{Proceedings of the National Academy of Sciences, 115}(16), E3635--E3644.
#'
#' Bhatia, N., & Bhatia, S. (2021).
#' Changes in gender stereotypes over time: A computational analysis.
#' \emph{Psychology of Women Quarterly, 45}(1), 106--125.
#'
#' @seealso
#' \code{\link{tab_similarity}}
#'
#' \code{\link{test_WEAT}}
#'
#' @examples
#' rnd = test_RND(
#'   demodata,
#'   labels=list(T1="Occupation", A1="Male", A2="Female"),
#'   T1=cc("
#'     architect, boss, leader, engineer, CEO, officer, manager,
#'     lawyer, scientist, doctor, psychologist, investigator,
#'     consultant, programmer, teacher, clerk, counselor,
#'     salesperson, therapist, psychotherapist, nurse"),
#'   A1=cc("male, man, boy, brother, he, him, his, son"),
#'   A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
#'   seed=1)
#' rnd
#'
#' @export
test_RND = function(data, T1, A1, A2,
                    use.pattern=FALSE, labels,
                    p.perm=TRUE,
                    p.nsim=10000,
                    p.side=2,
                    seed=NULL) {
  if(!p.side %in% 1:2) stop("`p.side` should be 1 or 2.", call.=FALSE)
  if(missing(A1)) stop("Please specify `A1`.", call.=FALSE)
  if(missing(A2)) stop("Please specify `A2`.", call.=FALSE)
  if(missing(T1)) stop("Please specify `T1`.", call.=FALSE)
  if(missing(labels))
    labels = list(T1="Target", A1="Attrib1", A2="Attrib2")
  check_data_validity(data)
  if(use.pattern) {
    if(!is.null(T1)) {
      message(Glue("T1 ({labels$T1}):"))
      T1 = names(get_wordvecs(data, pattern=T1))
    }
    if(!is.null(A1)) {
      message(Glue("A1 ({labels$A1}):"))
      A1 = names(get_wordvecs(data, pattern=A1))
    }
    if(!is.null(A2)) {
      message(Glue("A2 ({labels$A2}):"))
      A2 = names(get_wordvecs(data, pattern=A2))
    }
  }
  words = c(T1, A1, A2)
  if(attr(data, "normalized")==FALSE)
    data = normalize(data[word %in% words])

  dt = get_wordvecs(data, words)
  # words not found:
  not.found = setdiff(words, names(dt))
  # valid words:
  T1 = T1[T1 %in% names(dt)]
  A1 = A1[A1 %in% names(dt)]
  A2 = A2[A2 %in% names(dt)]

  v1 = rowMeans(dt[, A1, with=FALSE])  # average vector for A1
  v2 = rowMeans(dt[, A2, with=FALSE])  # average vector for A2
  drnd = data.table(T_word = T1)
  drnd$norm_dist_A1 = sapply(1:length(T1), function(i) {
    vm = dt[[T1[i]]]
    sqrt(sum((vm - v1)^2))
  })
  drnd$norm_dist_A2 = sapply(1:length(T1), function(i) {
    vm = dt[[T1[i]]]
    sqrt(sum((vm - v2)^2))
  })
  drnd$rnd = drnd$norm_dist_A1 - drnd$norm_dist_A2
  drnd$closer_to = ifelse(drnd$rnd < 0, labels$A1, labels$A2)

  eff_label = list(
    contrast=paste(labels$T1, "::", labels$A1, "vs.", labels$A2),
    labels=labels[c("T1", "A1", "A2")],
    words=sapply(list(T1, A1, A2), length)
  )
  interp = c(Glue("If RND < 0: {labels$T1} is more associated with {labels$A1} than {labels$A2}"),
             Glue("If RND > 0: {labels$T1} is more associated with {labels$A2} than {labels$A1}"))

  if(p.perm) {
    set.seed(seed)
    p = p_perm(drnd$rnd, nsim=p.nsim, side=p.side)
  } else {
    p = NULL
  }
  eff = data.table(
    Target=labels$T1,
    Attrib=paste0(labels$A1, "/", labels$A2),
    eff_raw=sum(drnd$rnd),
    pval=p)
  names(eff) = c("Target", "Attrib",
                 "rnd_sum",
                 names(p))

  rnd = list(
    words.valid=list(T1=T1, A1=A1, A2=A2),
    words.not.found=not.found,
    data.raw=drnd,
    eff.label=eff_label,
    eff.type="Relative Norm Distance (RND)",
    eff=eff,
    eff.interpretation=interp
  )
  class(rnd) = "rnd"
  return(rnd)
}


#' @export
print.rnd = function(x, digits=3, ...) {
  cli::cli_h1("{x$eff.type}")
  cn()
  cat(valid_words_info(x))
  cn(2)
  data.rnd = copy(x$data.raw)
  data.rnd$T_word = paste0("\"", data.rnd$T_word, "\"")
  data.rnd$closer_to = fixed_string(data.rnd$closer_to)
  names(data.rnd)[1] = " "
  print_table(data.rnd[, c(1, 4, 5, 2, 3)],
              row.names=FALSE, digits=digits,
              title="Relative norm distances (differences):",
              note=paste(x$eff.interpretation, collapse="\n"))
  cn()
  x$eff$Attrib = paste0(" ", x$eff$Attrib)
  if(length(x$eff)>=4) {
    p.type = names(x$eff)[4]
    note = paste("Permutation test: approximate p value =",
                 sprintf("%.2e", x$eff[[4]]),
                 ifelse(grepl("1", p.type),
                        "(one-sided)",
                        "(two-sided)"))
  } else {
    note = "Note: To get p value with permutation test, specify `p.perm=TRUE`"
  }
  print_table(x$eff, row.names=FALSE, digits=digits,
              title="Overall effect (raw):",
              note=note)
  cn()
}


#### Train Static Word Vectors ####


## Default UTF-8 separators used to split words and sentences.
##
## Used only in \code{\link{train_wordvec}}.
##
## @return
## A character vector of length 2:
## (1) the first element indicates how to split words and
## (2) the second element indicates how to split sentences.
##
## @examples
## utf8_split_default()
utf8_split_default = function() {
  c(paste0(" \n,.-?!:;/\"#$%&'()*+<=>@[]\\^_`{|}~\t\v\f\r",
           "\u3001\u3002\uff01\uff02\uff03\uff04\uff05\uff06\uff07\uff08\uff09\uff0a\uff0b\uff0c\uff0d\uff0e\uff0f",
           "\uff1a\uff1b\uff1c\uff1d\uff1e\uff1f\u2014\u2018\u2019\u201c\u201d\u3010\u3011\u300a\u300b"),
    "\n.?!\u3002\uff1f\uff01\u2026")
}


#' Tokenize raw texts for training word vectors.
#'
#' @inheritParams data_transform
#' @param text A character vector of the text,
#' or a file path on disk containing the text.
#' @param tokenizer Function used to tokenize the text.
#' Defaults to \code{\link[text2vec:tokenizers]{text2vec::word_tokenizer}}.
#' @param split Separator between tokens, only used when \code{simplify=TRUE}.
#' Defaults to \code{" "}.
#' @param remove Strings (in regular expression) to be removed from the text.
#' Defaults to \code{"_|'|<br/>|<br />|e\\\\.g\\\\.|i\\\\.e\\\\."}.
#' You may turn off this by specifying \code{remove=NULL}.
#' @param encoding Text encoding. Defaults to \code{"UTF-8"}.
#' @param simplify Return a character vector (\code{TRUE}) or a list of character vectors (\code{FALSE}).
#' Defaults to \code{TRUE}.
#'
#' @return
#' \itemize{
#'   \item{\code{simplify=TRUE}: A tokenized character vector,
#'   with each element as a sentence.}
#'   \item{\code{simplify=FALSE}: A list of tokenized character vectors,
#'   with each element as a vector of tokens in a sentence.}
#' }
#'
#' @seealso
#' \code{\link{train_wordvec}}
#'
#' @examples
#' txt1 = c(
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
#'
#' @export
tokenize = function(text,
                    tokenizer=text2vec::word_tokenizer,
                    split=" ",
                    remove="_|'|<br/>|<br />|e\\.g\\.|i\\.e\\.",
                    # '\\w+
                    encoding="UTF-8",
                    simplify=TRUE,
                    verbose=TRUE) {
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


#' Train word vectors using the Word2Vec, GloVe, or FastText algorithm.
#'
#' Train word vectors using the
#' \code{\link[word2vec:word2vec]{Word2Vec}},
#' \code{\link[rsparse:GloVe]{GloVe}}, or
#' \code{\link[fastTextR:ft_train]{FastText}} algorithm
#' with multi-threading.
#'
#' @inheritParams data_transform
#' @inheritParams data_wordvec_load
#' @inheritParams tokenize
#' @param method Training algorithm:
#' \itemize{
#'   \item{\code{"word2vec"} (default): using the \code{\link[word2vec:word2vec]{word2vec}} package}
#'   \item{\code{"glove"}: using the \code{\link[rsparse:GloVe]{rsparse}} and \code{\link[text2vec:text2vec]{text2vec}} packages}
#'   \item{\code{"fasttext"}: using the \code{\link[fastTextR:ft_train]{fastTextR}} package}
#' }
#' @param dims Number of dimensions of word vectors to be trained.
#' Common choices include 50, 100, 200, 300, and 500.
#' Defaults to \code{300}.
#' @param window Window size (number of nearby words behind/ahead the current word).
#' It defines how many surrounding words to be included in training:
#' [window] words behind and [window] words ahead ([window]*2 in total).
#' Defaults to \code{5}.
#' @param min.freq Minimum frequency of words to be included in training.
#' Words that appear less than this value of times will be excluded from vocabulary.
#' Defaults to \code{5} (take words that appear at least five times).
#' @param threads Number of CPU threads used for training.
#' A modest value produces the fastest training.
#' Too many threads are not always helpful.
#' Defaults to \code{8}.
#'
#' @param model \strong{<Only for Word2Vec / FastText>}
#'
#' Learning model architecture:
#' \itemize{
#'   \item{\code{"skip-gram"} (default): Skip-Gram, which predicts surrounding words given the current word}
#'   \item{\code{"cbow"}: Continuous Bag-of-Words, which predicts the current word based on the context}
#' }
#'
#' @param loss \strong{<Only for Word2Vec / FastText>}
#'
#' Loss function (computationally efficient approximation):
#' \itemize{
#'   \item{\code{"ns"} (default): Negative Sampling}
#'   \item{\code{"hs"}: Hierarchical Softmax}
#' }
#'
#' @param negative \strong{<Only for Negative Sampling in Word2Vec / FastText>}
#'
#' Number of negative examples.
#' Values in the range 5~20 are useful for small training datasets,
#' while for large datasets the value can be as small as 2~5.
#' Defaults to \code{5}.
#'
#' @param subsample \strong{<Only for Word2Vec / FastText>}
#'
#' Subsampling of frequent words (threshold for occurrence of words).
#' Those that appear with higher frequency in the training data will be randomly down-sampled.
#' Defaults to \code{0.0001} (\code{1e-04}).
#'
#' @param learning \strong{<Only for Word2Vec / FastText>}
#'
#' Initial (starting) learning rate, also known as alpha.
#' Defaults to \code{0.05}.
#'
#' @param ngrams \strong{<Only for FastText>}
#'
#' Minimal and maximal ngram length.
#' Defaults to \code{c(3, 6)}.
#'
#' @param x.max \strong{<Only for GloVe>}
#'
#' Maximum number of co-occurrences to use in the weighting function.
#' Defaults to \code{10}.
#'
#' @param convergence \strong{<Only for GloVe>}
#'
#' Convergence tolerance for SGD iterations. Defaults to \code{-1}.
#'
#' @param stopwords \strong{<Only for Word2Vec / GloVe>}
#'
#' A character vector of stopwords to be excluded from training.
#'
#' @param encoding Text encoding. Defaults to \code{"UTF-8"}.
#' @param tolower Convert all upper-case characters to lower-case?
#' Defaults to \code{FALSE}.
#' @param iteration Number of training iterations.
#' More iterations makes a more precise model,
#' but computational cost is linearly proportional to iterations.
#' Defaults to \code{5} for Word2Vec and FastText
#' while \code{10} for GloVe.
#'
#' @return
#' A \code{data.table} (of new class \code{wordvec}) with two variables:
#' \describe{
#'   \item{\code{word}}{words (tokens)}
#'   \item{\code{vec}}{\strong{raw} \emph{or} \strong{normalized} word vectors}
#' }
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{tokenize}}
#'
#' @references
#' All-in-one package:
#' \itemize{
#'   \item{\url{https://CRAN.R-project.org/package=wordsalad}}
#' }
#' Word2Vec:
#' \itemize{
#'   \item{\url{https://code.google.com/archive/p/word2vec/}}
#'   \item{\url{https://CRAN.R-project.org/package=word2vec}}
#'   \item{\url{https://github.com/maxoodf/word2vec}}
#' }
#' GloVe:
#' \itemize{
#'   \item{\url{https://nlp.stanford.edu/projects/glove/}}
#'   \item{\url{https://text2vec.org/glove.html}}
#'   \item{\url{https://CRAN.R-project.org/package=text2vec}}
#'   \item{\url{https://CRAN.R-project.org/package=rsparse}}
#' }
#' FastText:
#' \itemize{
#'   \item{\url{https://fasttext.cc/}}
#'   \item{\url{https://CRAN.R-project.org/package=fastTextR}}
#' }
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
#' as_tibble(dt1)  # just check
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
#' as_tibble(dt2)  # just check
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
#' as_tibble(dt3)  # just check
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
    verbose=TRUE) {

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
  if(verbose) cli::cli_alert_success("Text corpus: {sum(nchar(tokens))} characters, {length(tokens)} tokens (roughly words)")

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
    temp = utils::capture.output({
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
  if(normalize) wv = data_wordvec_normalize(wv, verbose)

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

  gc()
  return(wv)
}


