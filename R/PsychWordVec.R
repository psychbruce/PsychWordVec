#### Initialize ####


#' @import stringr
#' @import ggplot2
#' @import data.table
#' @importFrom dplyr %>% select left_join
#' @importFrom bruceR %notin% cc dtime import export Glue Print
.onAttach = function(libname, pkgname) {
  ## Version Check
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
  pkgs = c("dplyr", "stringr", "ggplot2", "data.table")

  suppressMessages({
    suppressWarnings({
      loaded = sapply(pkgs, require, character.only=TRUE)
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

    Download pre-trained word vectors data (.RData):
    https://psychbruce.github.io/WordVector_RData.pdf
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
#' @param v1,v2 Numeric vector (of the same length).
#' @param distance Compute cosine distance instead?
#' Default is \code{FALSE} (cosine similarity).
#'
#' @return A value of cosine similarity/distance.
#'
#' @seealso
#' \code{\link{most_similar}}
#'
#' \code{\link{pair_similarity}}
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
  if(length(v1)!=length(v2)) stop("v1 and v2 must be of the same length!", call.=FALSE)
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
  if(!is.data.table(data) | is.null(attr(data, "normalized")))
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


check_word_validity = function(data, words=NULL, pattern=NULL) {
  if(is.null(words)) {
    if(is.null(pattern)) {
      stop("Please specify either `words` or `pattern`!", call.=FALSE)
    } else {
      words = str_subset(data$word, pattern)
      Print("{length(words)} words are matched...")
    }
  }
  words.valid = data[word %in% words]$word
  if(length(words.valid) < length(words)) {
    for(word in setdiff(words, words.valid))
      Print("<<red X>> \"{word}\" not found...")
    message("Warning: Some words are not found!")
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
#' it can process about 30000 words per minute
#' with the slowest settings (\code{compress="xz"}, \code{compress.level=9})
#' on a modern computer (HP ProBook 450, Windows 11, Intel i7-1165G7 CPU, 32GB RAM, power supply mode).
#'
#' @param file.load File name of raw data (must be plain text).
#'
#' Data must be in this format (values separated by \code{sep}):
#'
#' cat 0.001 0.002 0.003 0.004 0.005 ... 0.300
#'
#' dog 0.301 0.302 0.303 0.304 0.305 ... 0.600
#' @param file.save File name of to-be-saved R data (must be .RData).
#' @param encoding File encoding. Default is \code{"auto"} (using \code{\link[vroom:vroom_lines]{vroom::vroom_lines()}} to fast read the file).
#' If specified to any other value (e.g., \code{"UTF-8"}), then it uses \code{\link[base:readLines]{readLines()}} to read the file, which is much slower than \code{vroom}.
#' @param sep Column separator. Default is \code{" "}.
#' @param header Is the 1st row a header (e.g., meta-information such as "2000000 300")?
#' Default is \code{"auto"}, which automatically determines whether there is a header.
#' If \code{TRUE}, then the 1st row will be dropped.
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
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{data_wordvec_load}}
#'
#' \code{\link{data_wordvec_normalize}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @export
data_transform = function(file.load, file.save=NULL,
                          encoding="auto", sep=" ", header="auto",
                          compress="bzip2", compress.level=9) {
  t00 = Sys.time()
  check_save_validity(file.save)
  if(is.data.table(file.load)) {
    dt = file.load  # 2 variables: word, vec
  } else {
    t0 = Sys.time()
    cat("\n")
    Print("****** Data Transformation (~ 30000 words/min in total) ******")
    cat("\n")
    Print("Loading file... \"{file.load}\"")
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
    Print("Word vectors data: {nrow(dt)} words, {ndim} dimensions (time cost = {dtime(t0, 'auto')})")
  }
  if(!is.null(file.save)) {
    t1 = Sys.time()
    cat("\n")
    k = 9  # coefficient for time estimate (based on preprocessing time cost)
    est.time = format(difftime(Sys.time(), t0, 'mins') * k, digits=1, nsmall=0)
    Print("Compressing and saving... (estimated time cost ~= {est.time})")
    gc()  # Garbage Collection: Free the Memory
    compress = switch(compress,
                      `1`="gzip",
                      `2`="bzip2",
                      `3`="xz",
                      compress)
    save(dt, file=file.save,
         compress=compress,
         compression_level=compress.level)
    Print("<<green \u221a>> Saved to \"{file.save}\" (time cost = {dtime(t1, 'mins')})")
  }
  gc()  # Garbage Collection: Free the Memory
  cat("\n")
  Print("****** Total time cost: {dtime(t00, 'mins')} ******")
  invisible(dt)
}


#' Load word vectors data from an ".RData" file.
#'
#' @param file.load File name (must be .RData transformed by
#' \code{\link{data_transform}}, with only two variables \code{word} and \code{vec}).
#' @param normalize Normalize all word vectors to unit length?
#' Default is \code{FALSE}. See \code{\link{data_wordvec_normalize}}.
#'
#' @return
#' A \code{data.table} with two variables:
#' \describe{
#'   \item{\code{word}}{words}
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
#' \code{\link{data_wordvec_subset}}
#'
#' @export
data_wordvec_load = function(file.load, normalize=FALSE) {
  t0 = Sys.time()
  check_load_validity(file.load)
  cat("Loading...")
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
  attr(data, "normalized") = normalize
  if(normalize) data = normalize(data)
  gc()  # Garbage Collection: Free the Memory
  cat("\015")
  Print("<<green \u221a>> Word vector data: {nrow(data)} words, {ndim} dims (loading time: {dtime(t0)})")
  if(normalize)
    Print("<<green \u221a>> All word vectors have now been normalized.")
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
#' @param data A \code{data.table} with variables \code{word} and \code{vec}
#' loaded by \code{\link{data_wordvec_load}}.
#'
#' @return
#' A \code{data.table} with \strong{normalized} word vectors.
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
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#'
#' data_wordvec_normalize(d)  # already normalized
#'
#' @export
data_wordvec_normalize = function(data) {
  check_data_validity(data)
  if(attr(data, "normalized")) {
    Print("<<red \u221a>> Word vectors have already been normalized.")
  } else {
    data = normalize(data)
    Print("<<green \u221a>> All word vectors have now been normalized.")
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
#'   \item{a \code{data.table} loaded by \code{\link{data_wordvec_load}}}
#'   \item{an .RData file transformed by \code{\link{data_transform}}}
#' }
#' @param words [Option 1] Word string (\code{NULL}; a single word; a vector of words).
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
                               file.save=NULL,
                               compress="bzip2", compress.level=9) {
  check_save_validity(file.save)
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
  words.valid = check_word_validity(data, words, pattern)
  dt = data[word %in% words.valid]  # much faster
  if(!is.null(file.save)) {
    t1 = Sys.time()
    cat("\n")
    Print("Compressing and saving...")
    compress = switch(compress,
                      `1`="gzip",
                      `2`="bzip2",
                      `3`="xz",
                      compress)
    save(dt, file=file.save,
         compress=compress,
         compression_level=compress.level)
    Print("<<green \u221a>> Saved to \"{file.save}\" (time cost = {dtime(t1, 'auto')})")
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
#' A \code{data.table} with two variables \code{word} and \code{vec},
#' transformed from the raw data (see the URL in Source) into \code{.RData}
#' using the \code{\link{data_transform}} function.
#'
#' @source
#' Google Code - word2vec (\url{https://code.google.com/archive/p/word2vec/})
#'
#' @usage
#' data(demodata)
#'
#' @name demodata
NULL


#### Get Word Vectors ####


#' Extract word vector of a single word.
#'
#' @inheritParams data_wordvec_normalize
#' @param word Word string (a single word).
#'
#' @return
#' A numeric vector of the word (or \code{NA} if the word is not in the data).
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{get_wordvecs}}
#'
#' \code{\link{plot_wordvecs}}
#'
#' \code{\link{data_wordvec_subset}}
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


#' Extract word vectors of multiple words.
#'
#' Extract word vectors of multiple words,
#' using either wordlist (a vector of words; using \code{words})
#' or regular expression (a pattern of words; using \code{pattern}).
#' If both (\code{words} and \code{pattern}) are specified, \code{words} wins.
#'
#' @inheritParams data_wordvec_normalize
#' @inheritParams data_wordvec_subset
#' @param plot Generate a plot to illustrate the word vectors? Default is \code{FALSE}.
#' @param plot.dims Dimensions to be plotted (e.g., \code{1:100}).
#' Default is \code{NULL} (plot all dimensions).
#' @param plot.step Step for value breaks. Default is \code{0.05}.
#' @param plot.border Color of tile border. Default is \code{"white"}.
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
#' \code{\link{plot_wordvecs}}
#'
#' \code{\link{data_wordvec_subset}}
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
#' ## a more complex example:
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
#' \dontrun{
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
#' }
#'
#' @export
get_wordvecs = function(data, words=NULL, pattern=NULL,
                        plot=FALSE,
                        plot.dims=NULL,
                        plot.step=0.05,
                        plot.border="white") {
  data.subset = data_wordvec_subset(data, words, pattern)
  dt = do.call(cbind, lapply(data.subset$word, function(wi) {
    di = data.table(word = data.subset[word %in% wi][[1, "vec"]])
    names(di) = wi
    return(di)
  }))
  if(plot) {
    p = plot_wordvecs(dt, dims=plot.dims,
                      step=plot.step, border=plot.border)
    attr(dt, "ggplot") = p
    print(p)
  }
  return(dt)
}


#' Visualize word vectors.
#'
#' @param dt A \code{data.table} returned from \code{\link{get_wordvecs}}.
#' @param dims Dimensions to be plotted (e.g., \code{1:100}).
#' Default is \code{NULL} (plot all dimensions).
#' @param step Step for value breaks. Default is \code{0.05}.
#' @param border Color of tile border. Default is \code{"white"}.
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
#' \code{\link{get_wordvecs}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#'
#' dt = get_wordvecs(d, cc("king, queen, man, woman"))
#' dt[, QUEEN := king - man + woman]
#' dt[, QUEEN := QUEEN / sqrt(sum(QUEEN^2))]  # normalize
#' names(dt)[5] = "king - man + woman"
#' plot_wordvecs(dt[, c(1,3,4,5,2)], dims=1:50)
#'
#' dt = get_wordvecs(d, cc("boy, girl, he, she"))
#' dt[, GIRL := boy - he + she]
#' dt[, GIRL := GIRL / sqrt(sum(GIRL^2))]  # normalize
#' names(dt)[5] = "boy - he + she"
#' plot_wordvecs(dt[, c(1,3,4,5,2)], dims=1:50)
#'
#' \dontrun{
#'
#' dt = get_wordvecs(d, cc("
#'   male, man, boy, he, his,
#'   female, woman, girl, she, her"))
#'
#' p = plot_wordvecs(dt, dims=1:100)
#'
#' # if you want to change something:
#' p + theme(legend.key.height=unit(0.1, "npc"))
#'
#' # or to save the plot:
#' ggsave(p, filename="wordvecs.png",
#'        width=8, height=5, dpi=500)
#' }
#'
#' @export
plot_wordvecs = function(dt, dims=NULL, step=0.05, border="white") {
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


#### Word Analysis ####


#' Find the Top-N most similar words.
#'
#' Find the Top-N most similar words, which replicates the results produced
#' by the Python \code{gensim} module \code{most_similar()} function.
#' (Exact replication of \code{gensim} requires the same word vectors data,
#' not the \code{demodata} used here in examples.)
#'
#' @inheritParams get_wordvec
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
#' @param keep Keep words specified in \code{x} in results?
#' Default is \code{FALSE}.
#' @param topn Top-N most similar words. Default is \code{10}.
#' @param above Default is \code{NULL}. Can be one of the following:
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
#' # the same as above:
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
#'
#' @export
most_similar = function(data, x, keep=FALSE, topn=10, above=NULL) {
  if(attr(data, "normalized")==FALSE) {
    Print("<<red *>> Results may be inaccurate if word vectors are not normalized.")
    data = data_wordvec_normalize(data)  # pre-normalized
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
  Print("<<bold <<cyan [Word Vector]>>>> =~ {as.character(f[2])}")
  Print("<<blue (normalized to unit length)>>")
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
#' @examples
#' pair_similarity(demodata, "China", "Chinese")
#'
#' @export
pair_similarity = function(data, word1, word2, distance=FALSE) {
  check_data_validity(data)
  if(attr(data, "normalized")==FALSE)
    data = normalize(data[word %in% c(word1, word2)])
  dt = get_wordvecs(data, c(word1, word2))
  if(distance)
    return(1 - sum(dt[[1]] * dt[[2]]))
  else
    return(sum(dt[[1]] * dt[[2]]))
}


#### Tabulate Data ####


#' Tabulate cosine similarity/distance of all word pairs.
#'
#' @inheritParams cosine_similarity
#' @inheritParams get_wordvecs
#'
#' @return
#' A \code{data.table} of all combinations of the words,
#' with their wordpair and cosine similarity/distance
#' (\code{cos_sim} or \code{cos_dist}).
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{tab_WEAT}}
#'
#' @examples
#' dts = tab_similarity(demodata, cc("king, queen, man, woman"))
#' dts
#'
#' dts = tab_similarity(demodata, cc("Beijing, China, Tokyo, Japan"))
#' dts
#'
#' @export
tab_similarity = function(data, words=NULL, pattern=NULL, distance=FALSE) {
  check_data_validity(data)
  words = check_word_validity(data, words, pattern)
  if(attr(data, "normalized")==FALSE)
    data = normalize(data[word %in% words])
  dt = get_wordvecs(data, words)
  words.valid = names(dt)
  words.mat = utils::combn(words.valid, 2)
  dts = data.table(
    word1 = words.mat[1,],
    word2 = words.mat[2,],
    wordpair = as.character(utils::combn(words.valid, 2, function(x) paste(x, collapse="-"))),
    cos_sim = as.numeric(utils::combn(dt, 2, function(x) sum(x[[1]] * x[[2]])))
  )
  if(distance) {
    dts$cos_dist = 1 - dts$cos_sim
    dts$cos_sim = NULL
  }
  return(dts)
}


#' Tabulate cosine similarity for WEAT / WEFAT analysis.
#'
#' @inheritParams tab_similarity
#' @param T1,T2 Target words. If only \code{T1} is specified,
#' then data for single-target WEAT (i.e., WEFAT; Caliskan et al., 2017)
#' can be tabulated.
#' @param A1,A2 Attribute words. Both should be specified.
#' @param labels Labels for target and attribute concepts.
#' Should be a named \code{list}, such as (the default)
#' \code{list(T1="Target1", T2="Target2", A1="Attrib1", A2="Attrib2")}.
#'
#' @return
#' A \code{list} of objects:
#' \describe{
#'   \item{\code{words.valid}}{
#'     valid (actually matched) words}
#'   \item{\code{data.raw}}{
#'     \code{data.table} of cosine similarities between all word pairs}
#'   \item{\code{data.mean}}{
#'     \code{data.table} of \emph{mean} cosine similarities
#'     \emph{across} all attribute words}
#'   \item{\code{data.diff}}{
#'     \code{data.table} of \emph{differential} mean cosine similarities
#'     \emph{between} the two attribute concepts}
#'   \item{\code{code.diff}}{
#'     description for the difference between the two attribute concepts}
#'   \item{\code{eff.type}}{
#'     effect type: WEAT or WEFAT}
#'   \item{\code{eff.size}}{
#'     effect size for WEAT (a single value) or WEFAT (a data table)}
#' }
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @references
#' Caliskan, A., Bryson, J. J., & Narayanan, A. (2017).
#' Semantics derived automatically from language corpora contain human-like biases.
#' \emph{Science, 356}(6334), 183-186.
#'
#' @seealso
#' \code{\link{tab_similarity}}
#'
#' @examples
#' ## Remember: cc() is more convenient than c()!
#'
#' weat = tab_WEAT(
#'   demodata,
#'   T1=cc("king, King"),
#'   T2=cc("queen, Queen"),
#'   A1=cc("male, man, boy, brother, he, him, his, son"),
#'   A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
#'   labels=list(T1="King", T2="Queen", A1="Male", A2="Female"))
#' weat
#'
#' wefat = tab_WEAT(
#'   demodata,
#'   T1=cc("
#'     architect, boss, leader, engineer, CEO, officer, manager,
#'     lawyer, scientist, doctor, psychologist, investigator,
#'     consultant, programmer, teacher, clerk, counselor,
#'     salesperson, therapist, psychotherapist, nurse"),
#'   A1=cc("male, man, boy, brother, he, him, his, son"),
#'   A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
#'   labels=list(T1="Occupation", A1="Male", A2="Female"))
#' wefat
#'
#' @export
tab_WEAT = function(data, T1, T2, A1, A2, labels) {
  if(missing(A1)) stop("Please specify `A1`.", call.=FALSE)
  if(missing(A2)) stop("Please specify `A2`.", call.=FALSE)
  if(missing(T1)) stop("Please specify `T1`.", call.=FALSE)
  if(missing(T2)) T2 = NULL
  if(missing(labels)) {
    if(missing(T2))
      labels = list(T1="Target", T2=NA, A1="Attrib1", A2="Attrib2")
    else
      labels = list(T1="Target1", T2="Target2", A1="Attrib1", A2="Attrib2")
  }
  check_data_validity(data)
  words = c(T1, T2, A1, A2)
  if(attr(data, "normalized")==FALSE)
    data = normalize(data[word %in% words])

  dt = get_wordvecs(data, words)
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
  if(is.null(T2)) {
    # WEFAT
    code_diff = paste(labels$T1, "::", labels$A1, "vs.", labels$A2)
    eff_type = "WEFAT (Word-Embedding Factual Association Test)"
    # dweat.mean$Target = NULL
    # dweat.diff$Target = NULL
    eff_size = dweat.diff[, .(T_word, std_diff)]
    names(eff_size)[2] = "eff_size"
  } else {
    # WEAT
    code_diff = paste(labels$T1, "vs.", labels$T2, "::", labels$A1, "vs.", labels$A2)
    eff_type = "WEAT (Word-Embedding Association Test)"
    dweat.diff$std_diff = NULL
    std_dev = stats::sd(dweat.diff$cos_sim_diff)
    dweat.diff$std_dev = std_dev
    mean_diffs = dweat.diff[, .(
      mean_diff = mean(cos_sim_diff)
    ), by=Target]$mean_diff
    eff_size = (mean_diffs[1] - mean_diffs[2]) / std_dev
  }

  return(list(
    words.valid=list(T1=T1, T2=T2, A1=A1, A2=A2),
    data.raw=dweat,
    data.mean=dweat.mean,
    data.diff=dweat.diff,
    code.diff=code_diff,
    eff.type=eff_type,
    eff.size=eff_size
  ))
}


#### Psych Test ####


