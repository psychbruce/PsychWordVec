#### Initialize ####


#' @import stringr
#' @import ggplot2
#' @import data.table
#' @importFrom corrplot corrplot
#' @importFrom grDevices png pdf dev.off
#' @importFrom dplyr %>% select left_join
#' @importFrom bruceR cc dtime import export Glue Print print_table
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
#' cos_sim(v1=c(1,1,1), v2=c(2,2,2))  # 1
#' cos_sim(v1=c(1,4,1), v2=c(4,1,1))  # 0.5
#' cos_sim(v1=c(1,1,0), v2=c(0,0,1))  # 0
#'
#' cos_dist(v1=c(1,1,1), v2=c(2,2,2))  # 0
#' cos_dist(v1=c(1,4,1), v2=c(4,1,1))  # 0.5
#' cos_dist(v1=c(1,1,0), v2=c(0,0,1))  # 1
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


#' @rdname cosine_similarity
#' @export
cos_sim = function(v1, v2) {
  cosine_similarity(v1, v2, distance=FALSE)
}


#' @rdname cosine_similarity
#' @export
cos_dist = function(v1, v2) {
  cosine_similarity(v1, v2, distance=TRUE)
}


# Normalized vectors: vec / sqrt(sum(vec^2))
# cosine_similarity_norm = function(v1, v2) {
#   sum(v1 * v2)
# }


#' Reshape word vectors data between \code{wordvec} and \code{embed}.
#'
#' Reshape word vectors data between
#' \code{wordvec} (data.table, with two variables \code{word} and \code{vec})
#' and \code{embed} (matrix, with dimensions as columns and words as row names).
#'
#' @describeIn as_wordvec From \code{embed} (matrix) to \code{wordvec} (data.table).
#'
#' @inheritParams data_wordvec_load
#' @param x Object to be reshaped. See examples.
#'
#' @return
#' \code{wordvec} (data.table) or \code{embed} (matrix).
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
#'
#' embed = as_embed(d)
#' embed
#'
#' wordvec = as_wordvec(embed)
#' wordvec
#'
#' @export
as_wordvec = function(x, normalize=FALSE, verbose=TRUE) {
  if(is.data.table(x)) {
    wordvec = check_data_validity(x)
  } else {
    mat = as.matrix(x)  # much faster
    wordvec = data.table(
      word = row.names(mat),
      vec = do.call("list", lapply(
        1:nrow(mat), function(i) {
          as.numeric(mat[i,])
        }))
    )
  }
  if(is.null(attr(x, "dims")))
    attr(wordvec, "dims") = length(wordvec[[1, "vec"]])
  else
    attr(wordvec, "dims") = attr(x, "dims")
  if(is.null(attr(x, "normalized")))
    attr(wordvec, "normalized") = FALSE
  else
    attr(wordvec, "normalized") = attr(x, "normalized")
  class(wordvec) = c("wordvec", "data.table", "data.frame")
  if(normalize) wordvec = data_wordvec_normalize(wordvec, verbose)
  return(wordvec)
}


#' @describeIn as_wordvec From \code{wordvec} (data.table) to \code{embed} (matrix).
#' @export
as_embed = function(x, normalize=FALSE, verbose=TRUE) {
  if(is.matrix(x)) {
    embed = x
  } else if(is.numeric(x)) {
    embed = t(matrix(x))
    rownames(embed) = ""
  } else {
    data = check_data_validity(x)
    embed = do.call("rbind", lapply(data$vec, function(x) {
      t(x)  # matrix is much faster
    }))
    rownames(embed) = data$word
  }
  if(is.null(colnames(embed)))
    colnames(embed) = paste0("dim", 1:ncol(embed))
  if(is.null(attr(x, "dims")))
    attr(embed, "dims") = ncol(embed)
  else
    attr(embed, "dims") = attr(x, "dims")
  if(is.null(attr(x, "normalized")))
    attr(embed, "normalized") = FALSE
  else
    attr(embed, "normalized") = attr(x, "normalized")
  class(embed) = c("embed", "matrix", "array")
  if(normalize) embed = data_wordvec_normalize(embed, verbose)
  return(embed)
}


#' @export
print.wordvec = function(x, ...) {
  norm = ifelse(attr(x, "normalized"), "(normalized)", "(NOT normalized)")
  dims = paste0("<", attr(x, "dims"), " dims>")
  x$vec = sapply(x$vec, function(i) {
    paste0("[", sprintf("% .4f", i[1]), ", ...", dims, "]")
  })
  cli::cli_text(grey("{.strong # wordvec (data.table)}: {nrow(x)} \u00d7 {ncol(x)} {norm}"))
  print(as.data.table(x))
}


#' @export
print.embed = function(x, ...) {
  norm = ifelse(attr(x, "normalized"), "(normalized)", "(NOT normalized)")
  ndim = ncol(x)
  dims = paste0("<", ndim, " dims>")
  n = nrow(x)
  rownames = rownames(x)
  x = x[, c(1, 2, ndim)]
  if(!is.matrix(x)) x = t(matrix(x))
  rowids = sprintf(paste0("% ", nchar(n), "s:"), 1:n)
  rownames(x) = paste(rowids, rownames)
  colnames(x) = paste0("dim", c(1, 2, ndim))
  x[, 1] = sprintf("% .4f", x[, 1])
  x[, 2] = "..."
  x[, 3] = dims
  if(n > 100) {
    null = t(matrix(rep("", 3)))
    rownames(null) = paste(rep("-", nchar(n)+1), collapse="")
    x = rbind(x[1:5,],
              null,
              x[(n-4):n,])
  }
  x = as.data.frame(x)
  names(x)[2] = "..."
  cli::cli_text(grey("{.strong # embed (matrix)}: {n} \u00d7 {ndim} {norm}"))
  print(x)
}


#' @export
rbind.wordvec = function(...) {
  wordvec = rbindlist(lapply(list(...), function(d) {
    as.data.table(d)
  }))
  attr(wordvec, "dims") = attr(list(...)[[1]], "dims")
  attr(wordvec, "normalized") = attr(list(...)[[1]], "normalized")
  class(wordvec) = c("wordvec", "data.table", "data.frame")
  return(wordvec)
}


#' @export
rbind.embed = function(...) {
  embed = do.call("rbind", lapply(list(...), function(m) {
    m = as.matrix(m)
    class(m) = c("matrix", "array")
    return(m)
  }))
  attr(embed, "dims") = attr(list(...)[[1]], "dims")
  attr(embed, "normalized") = attr(list(...)[[1]], "normalized")
  class(embed) = c("embed", "matrix", "array")
  return(embed)
}


#### Utils ####


#' @importFrom bruceR cc
#' @export
bruceR::cc


is.wordvec = function(x) inherits(x, "wordvec")
is.embed = function(x) inherits(x, "embed")
is.valid = function(x) inherits(x, c("wordvec", "embed"))
grey = cli::make_ansi_style("grey60")


check_data_validity = function(x) {
  if(!is.valid(x))
    stop("Data must be loaded using `data_wordvec_load()`.", call.=FALSE)
  if(is.wordvec(x)) {
    if(names(x)[1] != "word")
      names(x)[1] = "word"
    if(length(unique(x$word)) < nrow(x))
      x$word = number_duplicate(x$word)
  }
  return(x)
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


force_normalize = function(x, verbose=TRUE) {
  x = check_data_validity(x)
  if(attr(x, "normalized")==FALSE) {
    if(verbose) cli::cli_alert_warning("Results may be inaccurate if word vectors are not normalized.")
    x = data_wordvec_normalize(x, verbose)
  }
  return(x)
}


extract_valid_subset = function(x, words=NULL, pattern=NULL) {
  xs = as_embed(check_data_validity(x))
  vocab = rownames(xs)
  if(is.null(words)) {
    if(is.null(pattern)) {
      return(xs)  # new default
    } else {
      words.valid = str_subset(vocab, pattern)
      message(Glue("{length(words.valid)} words matched..."))
    }
  } else {
    words.valid = intersect(words, vocab)  # faster
  }
  if(length(words.valid) < length(words)) {
    not.found = setdiff(words, words.valid)
    cli::cli_alert_danger("{length(not.found)} words not found: {.val {not.found}}")
  }
  if(length(words.valid) == 0) {
    return(NULL)
  } else {
    xs = xs[words.valid,]
    if(length(words.valid) == 1) {
      xs = t(xs)
      rownames(xs) = words.valid
    }
    xs = as_embed(xs)
    if(is.wordvec(x)) xs = as_wordvec(xs)
    return(xs)
  }
}


#### Transform and Load ####


#' Transform plain text of word vectors into
#' \code{wordvec} (data.table) or \code{embed} (matrix),
#' saved in a compressed ".RData" file.
#'
#' @description
#' Transform plain text of word vectors into
#' \code{wordvec} (data.table) or \code{embed} (matrix),
#' saved in a compressed ".RData" file.
#'
#' \emph{Speed}: In total (preprocess + compress + save),
#' it can process about 30000 words/min
#' with the slowest settings (\code{compress="xz"}, \code{compress.level=9})
#' on a modern computer (HP ProBook 450, Windows 11, Intel i7-1165G7 CPU, 32GB RAM).
#'
#' @param file.load File name of raw text (must be plain text).
#'
#' Data must be in this format (values separated by \code{sep}):
#'
#' cat 0.001 0.002 0.003 0.004 0.005 ... 0.300
#'
#' dog 0.301 0.302 0.303 0.304 0.305 ... 0.600
#' @param file.save File name of to-be-saved R data (must be .RData).
#' @param as Transform the text to which R object?
#' \code{\link[PsychWordVec:as_wordvec]{wordvec}} (data.table) or
#' \code{\link[PsychWordVec:as_embed]{embed}} (matrix).
#' Defaults to \code{wordvec}.
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
#' A \code{wordvec} (data.table) or \code{embed} (matrix).
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{as_wordvec}} / \code{\link{as_embed}}
#'
#' \code{\link{data_wordvec_load}}
#'
#' \code{\link{data_wordvec_normalize}}
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
data_transform = function(
    file.load,
    file.save,
    as=c("wordvec", "embed"),
    sep=" ",
    header="auto",
    encoding="auto",
    compress="bzip2",
    compress.level=9,
    verbose=TRUE
) {
  as = match.arg(as)
  t0 = Sys.time()
  if(!missing(file.save)) check_save_validity(file.save)
  if(is.wordvec(file.load)) {
    dt = file.load  # 2 variables: word, vec
  } else {
    if(verbose) {
      cli::cli_h1("Data Transformation (~ 30000 words/min in total)")
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
    if(as=="wordvec") {
      # wordvec
      x = data.table(
        word = str_split(dt$x, sep, n=2, simplify=TRUE)[,1],
        vec = do.call("list", lapply(
          str_split(dt$x, sep, n=2, simplify=TRUE)[,2], function(v) {
            as.numeric(cc(v, sep=sep))
          }))
      )
      dims = unique(sapply(x$vec, length))
      ndim = length(x[[1, "vec"]])
      class(x) = c("wordvec", "data.table", "data.frame")
    } else {
      # matrix
      x = do.call("rbind", lapply(
        str_split(dt$x, sep, n=2, simplify=TRUE)[,2], function(v) {
          as.numeric(cc(v, sep=sep))
        }))
      rownames(x) = str_split(dt$x, sep, n=2, simplify=TRUE)[,1]
      dims = ndim = ncol(x)
      colnames(x) = paste0("dim", 1:ndim)
      class(x) = c("embed", "matrix", "array")
    }
    if(length(dims) > 1)
      warning("The number of dimensions is not consistent between words!", call.=FALSE)
    if(verbose)
      Print("Word vectors data: {nrow(x)} words, {ndim} dimensions (time cost = {dtime(t0, 'auto')})")
  }
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
    save(x, file=file.save,
         compress=compress,
         compression_level=compress.level)
    if(verbose)
      cli::cli_alert_success("Saved to \"{file.save}\" (time cost = {dtime(t1, 'mins')})")
  }
  gc()  # Garbage Collection: Free the Memory
  if(verbose)
    cli::cli_h2("Total time cost: {dtime(t0, 'mins')}")
  invisible(x)
}


#' Load word vectors data (\code{wordvec} or \code{embed}) from ".RData" file.
#'
#' @inheritParams data_transform
#' @param file.load File name (must be .RData transformed by
#' \code{\link{data_transform}}).
#' @param as Load as
#' \code{\link[PsychWordVec:as_wordvec]{wordvec}} (data.table) or
#' \code{\link[PsychWordVec:as_embed]{embed}} (matrix).
#' Defaults to the original class of the R object in \code{file.load}.
#' @param normalize Normalize all word vectors to unit length?
#' Defaults to \code{FALSE}. See \code{\link{data_wordvec_normalize}}.
#'
#' @return
#' A \code{wordvec} (data.table) or \code{embed} (matrix).
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{as_wordvec}} / \code{\link{as_embed}}
#'
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_normalize}}
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
data_wordvec_load = function(
    file.load,
    as=c("wordvec", "embed"),
    normalize=FALSE,
    verbose=TRUE
) {
  if(!any(as %in% c("wordvec", "embed")))
    stop("`as` should be \"wordvec\" or \"embed\".", call.=FALSE)
  t0 = Sys.time()
  check_load_validity(file.load)
  if(verbose) cat("Loading...")
  envir = new.env()
  load(file=file.load, envir=envir)
  if(length(ls(envir)) > 1)
    warning("RData file contains multiple objects. Return the first object.", call.=FALSE)
  x = get(ls(envir)[1], envir)
  rm(envir)
  if(is.data.table(x)) {
    ndim = length(x[[1, "vec"]])
    class(x) = c("wordvec", "data.table", "data.frame")
  }
  if(is.matrix(x)) {
    ndim = ncol(x)
    class(x) = c("embed", "matrix", "array")
  }
  attr(x, "dims") = ndim
  attr(x, "normalized") = normalize
  if(verbose) {
    cat("\015")
    cli::cli_alert_success("Word vector data: {nrow(x)} vocab, {ndim} dims (loading time: {dtime(t0)})")
  }
  if(length(as)==1) {
    if(as=="wordvec")
      x = as_wordvec(x, normalize, verbose)
    if(as=="embed")
      x = as_embed(x, normalize, verbose)
  } else {
    if(is.wordvec(x))
      x = as_wordvec(x, normalize, verbose)
    if(is.embed(x))
      x = as_embed(x, normalize, verbose)
  }
  gc()  # Garbage Collection: Free the Memory
  return(x)
}


#### Normalize ####


#' Normalize all word vectors to the unit length 1.
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
#' @param data A \code{\link[PsychWordVec:as_wordvec]{wordvec}} (data.table) or
#' \code{\link[PsychWordVec:as_embed]{embed}} (matrix),
#' see \code{\link{data_wordvec_load}}.
#'
#' @return
#' A \code{wordvec} (data.table) or \code{embed} (matrix) with \strong{normalized} word vectors.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{as_wordvec}} / \code{\link{as_embed}}
#'
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_load}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' d = data_wordvec_normalize(demodata)
#' # the same: d = as_wordvec(demodata, normalize=TRUE)
#'
#' data_wordvec_normalize(d)  # already normalized
#'
#' @export
data_wordvec_normalize = function(data, verbose=TRUE) {
  data = check_data_validity(data)
  if(attr(data, "normalized")) {
    if(verbose) cli::cli_alert_warning("Word vectors have already been normalized.")
  } else {
    data = normalize(data)
    if(verbose) cli::cli_alert_success("All word vectors have now been normalized.")
  }
  gc()  # Garbage Collection: Free the Memory
  invisible(data)
}


normalize = function(x) {
  # L2-normalized (unit euclidean length)
  if(is.wordvec(x)) {
    vec = NULL
    x[, vec := lapply(vec, function(vec) { vec / sqrt(sum(vec^2)) } )]
  }
  if(is.embed(x)) {
    x = x / sqrt(rowSums(x^2))
  }
  attr(x, "normalized") = TRUE
  return(x)
}


#### Subset ####


#' Extract a subset of word vectors data (with S3 methods).
#'
#' @description
#' Extract a subset of word vectors data (with S3 methods).
#' You may specify either a \code{wordvec} or \code{embed} loaded by \code{\link{data_wordvec_load}})
#' or an .RData file transformed by \code{\link{data_transform}}).
#'
#' @inheritParams data_transform
#' @param x Can be:
#' \itemize{
#'   \item{a \code{wordvec} or \code{embed} loaded by \code{\link{data_wordvec_load}}}
#'   \item{an .RData file transformed by \code{\link{data_transform}}}
#' }
#' @param words [Option 1] Word strings (\code{NULL}; a single word; a vector of words).
#' @param pattern [Option 2] Pattern of regular expression (see \code{\link[stringr:str_subset]{str_subset}}).
#' If neither \code{words} nor \code{pattern} are specified (i.e., both are \code{NULL}),
#' then all words in the data will be extracted.
#' @param as Reshape to
#' \code{\link[PsychWordVec:as_wordvec]{wordvec}} (data.table) or
#' \code{\link[PsychWordVec:as_embed]{embed}} (matrix).
#' Defaults to the original class of \code{x}.
#' @param ... Parameters passed to \code{data_wordvec_subset}
#' when using the S3 method \code{subset}.
#'
#' @return
#' A subset of \code{wordvec} or \code{embed} of valid (available) words.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{as_wordvec}} / \code{\link{as_embed}}
#'
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_load}}
#'
#' \code{\link{data_wordvec_normalize}}
#'
#' @examples
#' ## specify `x` as a `wordvec` or `embed` object:
#' subset(demodata, c("China", "Japan", "Korea"))
#' subset(as_embed(demodata), c("China", "Japan", "Korea"))
#'
#' \donttest{## specify `x` and `pattern`, and save with `file.save`:
#' subset(demodata, pattern="Chin[ae]|Japan|Korea",
#'        file.save="subset.RData")
#'
#' ## load the subset:
#' d.subset = data_wordvec_load("subset.RData")
#' d.subset
#'
#' ## specify `x` as an .RData file and save with `file.save`:
#' subset("subset.RData",
#'        words=c("China", "Chinese"),
#'        file.save="new.subset.RData")
#' d.new.subset = data_wordvec_load("new.subset.RData", as="embed")
#' d.new.subset
#'
#' unlink("subset.RData")  # delete file for code check
#' unlink("new.subset.RData")  # delete file for code check
#' }
#' @export
data_wordvec_subset = function(
    x,
    words=NULL,
    pattern=NULL,
    as=c("wordvec", "embed"),
    file.save,
    compress="bzip2",
    compress.level=9,
    verbose=TRUE
) {
  if(!any(as %in% c("wordvec", "embed")))
    stop("`as` should be \"wordvec\" or \"embed\".", call.=FALSE)
  if(!missing(file.save)) check_save_validity(file.save)
  if(is.valid(x)) {
    x = check_data_validity(x)
  } else if(is.character(x)) {
    file.load = x
    if(!str_detect(file.load, "\\.rda$|\\.[Rr][Dd]ata$"))
      stop("`x` must be .RData!", call.=FALSE)
    x = data_wordvec_load(file.load)
  } else {
    stop("`x` must be one of them:
      - a `wordvec` or `embed` loaded by `data_wordvec_load()`
      - an .RData file transformed by `data_transform()`", call.=FALSE)
  }
  x = extract_valid_subset(x, words, pattern)
  if(length(as)==1) {
    if(as=="wordvec")
      x = as_wordvec(x)
    if(as=="embed")
      x = as_embed(x)
  } else {
    if(is.wordvec(x))
      x = as_wordvec(x)
    if(is.embed(x))
      x = as_embed(x)
  }
  if(!missing(file.save)) {
    t1 = Sys.time()
    if(verbose)
      Print("\n\n\nCompressing and saving...")
    compress = switch(compress,
                      `1`="gzip",
                      `2`="bzip2",
                      `3`="xz",
                      compress)
    save(x, file=file.save,
         compress=compress,
         compression_level=compress.level)
    if(verbose)
      cli::cli_alert_success("Saved to \"{file.save}\" (time cost = {dtime(t1, 'auto')})")
  }
  gc()  # Garbage Collection: Free the Memory
  return(x)
}


#' @rdname data_wordvec_subset
#' @export
subset.wordvec = function(x, ...) {
  data_wordvec_subset(x, ...)
}


#' @rdname data_wordvec_subset
#' @export
subset.embed = function(x, ...) {
  data_wordvec_subset(x, ...)
}


#' @rdname data_wordvec_subset
#' @export
subset.character = function(x, ...) {
  data_wordvec_subset(x, ...)
}


#### Orthogonal Procrustes ####


# adapted from cds::orthprocr()
# same as psych::Procrustes() and pracma::procrustes()


#' Orthogonal Procrustes solution for matrix alignment.
#'
#' In order to compare word embeddings from different time periods,
#' we must ensure that the embedding matrices are aligned to
#' the same semantic space (coordinate axes).
#' The Orthogonal Procrustes solution (Schönemann, 1966) is
#' commonly used to align historical embeddings over time
#' (Hamilton et al., 2016; Li et al., 2020).
#' This function produces the same results as by
#' \code{cds::orthprocr()},
#' \code{psych::Procrustes()}, and
#' \code{pracma::procrustes()}.
#'
#' @param M,X Two embedding matrices of the same size (rows and columns),
#' or two \code{\link[PsychWordVec:as_wordvec]{wordvec}} objects
#' as loaded by \code{\link{data_wordvec_load}} or
#' transformed from matrices by \code{\link{as_wordvec}}.
#' \itemize{
#'   \item{\code{M} is the reference (anchor/baseline/target) matrix,
#'         e.g., the embedding matrix learned at
#'         the later year (\eqn{t + 1}).}
#'   \item{\code{X} is the matrix to be transformed/rotated.}
#' }
#' \emph{Note}: The function automatically extracts only
#' the intersection (overlapped part) of words in \code{M} and \code{X}
#' and sorts them in the same order (according to \code{M}).
#'
#' @return
#' A \code{matrix} or \code{wordvec} object of
#' \code{X} after rotation, depending on the class of
#' \code{M} and \code{X}.
#'
#' @references
#' Hamilton, W. L., Leskovec, J., & Jurafsky, D. (2016).
#' Diachronic word embeddings reveal statistical laws of semantic change.
#' In \emph{Proceedings of the 54th Annual Meeting of the Association for Computational Linguistics}
#' (Vol. 1, pp. 1489--1501). Association for Computational Linguistics.
#'
#' Li, Y., Hills, T., & Hertwig, R. (2020).
#' A brief history of risk. \emph{Cognition, 203}, 104344.
#'
#' Schönemann, P. H. (1966).
#' A generalized solution of the orthogonal Procrustes problem.
#' \emph{Psychometrika, 31}(1), 1--10.
#'
#' @seealso
#' \code{\link{as_wordvec}} / \code{\link{as_embed}}
#'
#' @examples
#' M = matrix(c(0,0,  1,2,  2,0,  3,2,  4,0), ncol=2, byrow=TRUE)
#' X = matrix(c(0,0, -2,1,  0,2, -2,3,  0,4), ncol=2, byrow=TRUE)
#' rownames(M) = rownames(X) = cc("A, B, C, D, E")  # words
#' colnames(M) = colnames(X) = cc("dim1, dim2")  # dimensions
#'
#' ggplot() +
#'   geom_path(data=as.data.frame(M), aes(x=dim1, y=dim2),
#'             color="red") +
#'   geom_path(data=as.data.frame(X), aes(x=dim1, y=dim2),
#'             color="blue") +
#'   coord_equal()
#'
#' # Usage 1: input two matrices
#' XR = orth_procrustes(M, X)
#' XR  # aligned with M
#'
#' ggplot() +
#'   geom_path(data=as.data.frame(XR), aes(x=dim1, y=dim2)) +
#'   coord_equal()
#'
#' # Usage 2: input two `wordvec` objects
#' M.wv = as_wordvec(M)
#' X.wv = as_wordvec(X)
#' XR.wv = orth_procrustes(M.wv, X.wv)
#' XR.wv  # aligned with M.wv
#'
#' # M and X must have the same set and order of words
#' # and the same number of word vector dimensions.
#' # The function extracts only the intersection of words
#' # and sorts them in the same order according to M.
#'
#' Y = rbind(X, X[rev(rownames(X)),])
#' rownames(Y)[1:5] = cc("F, G, H, I, J")
#' M.wv = as_wordvec(M)
#' Y.wv = as_wordvec(Y)
#' M.wv  # words: A, B, C, D, E
#' Y.wv  # words: F, G, H, I, J, E, D, C, B, A
#' YR.wv = orth_procrustes(M.wv, Y.wv)
#' YR.wv  # aligned with M.wv, with the same order of words
#'
#' @export
orth_procrustes = function(M, X) {
  stopifnot(all.equal(class(M), class(X)))
  class = "matrix"
  if(is.wordvec(M)) {
    class = "wordvec"
    M = as_embed(M)
    X = as_embed(X)
  }
  if(!is.matrix(M))
    stop("M and X should be of class matrix or wordvec.", call.=FALSE)
  ints = intersect(rownames(M), rownames(X))
  XR = orthogonal_procrustes(M[ints,], X[ints,])
  if(class=="wordvec")
    XR = as_wordvec(XR)
  return(XR)
}


orthogonal_procrustes = function(M, X) {
  stopifnot(all.equal(dim(M), dim(X)))
  MX = crossprod(M, X)  # = t(M) %*% X
  svdMX = svd(MX)
  R = tcrossprod(svdMX$v, svdMX$u)  # = svdMX$v %*% t(svdMX$u)
  XR = X %*% R
  colnames(XR) = colnames(X)
  # attr(XR, "Rotation") = R
  return(XR)
}


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
#' d = as_wordvec(demodata, normalize=TRUE)
#'
#' v1 = get_wordvec(demodata, "China")  # raw vector
#' v2 = get_wordvec(d, "China")  # normalized vector
#' cor(v1, v2)
#' cos_sim(v1, v2)
#'
#' @export
get_wordvec = function(data, word) {
  data = check_data_validity(data)
  if(length(word)>1)
    stop("Please use `get_wordvecs()` for more than one word.", call.=FALSE)
  if(is.wordvec(data)) {
    WORD = word
    di = data[word %in% WORD]
    if(nrow(di)==1) vec = di[[1, "vec"]] else vec = NA
  }
  if(is.embed(data)) {
    id = which(word == rownames(data))
    if(length(id)==1) vec = as.numeric(data[id,]) else vec = NA
  }
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
#' d = as_wordvec(demodata, normalize=TRUE)
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
get_wordvecs = function(
    data,
    words=NULL,
    pattern=NULL,
    plot=FALSE,
    plot.dims=NULL,
    plot.step=0.05,
    plot.border="white"
) {
  embed = get_wordembed(data, words, pattern)
  if(is.null(embed)) return(NULL)
  dt = as.data.table(t(embed))
  if(plot) {
    p = plot_wordvec(dt, dims=plot.dims,
                     step=plot.step, border=plot.border)
    attr(dt, "ggplot") = p
    print(p)
  }
  return(dt)
}


get_wordembed = function(x, words=NULL, pattern=NULL) {
  extract_valid_subset(as_embed(x), words, pattern)
}


#' Visualize word vectors.
#'
#' @param x Can be:
#' \itemize{
#'   \item{a \code{data.table} returned by \code{\link{get_wordvecs}}}
#'   \item{a \code{\link[PsychWordVec:as_wordvec]{wordvec}} (data.table)
#'   or \code{\link[PsychWordVec:as_embed]{embed}} (matrix)
#'   loaded by \code{\link{data_wordvec_load}}}
#' }
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
#' d = as_wordvec(demodata, normalize=TRUE)
#'
#' plot_wordvec(d[1:10])
#' plot_wordvec(as_embed(d[1:10]))
#'
#' \donttest{dt = get_wordvecs(d, cc("king, queen, man, woman"))
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
#' dt = get_wordvecs(d, cc("
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
plot_wordvec = function(
    x,
    dims=NULL,
    step=0.05,
    border="white"
) {
  if(is.valid(x))
    dt = as.data.table(t(as_embed(x)))
  else
    dt = x
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
#' @param theta Speed/accuracy trade-off (increase for less accuracy), set to 0 for exact t-SNE.
#' Defaults to 0.5.
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
plot_wordvec_tSNE = function(
    x,
    dims=2,
    perplexity,
    theta=0.5,
    colors=NULL,
    seed=NULL,
    custom.Rtsne=NULL
) {
  if(is.valid(x))
    dt = as.data.table(t(as_embed(x)))
  else
    dt = x
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


#' Calculate the sum vector of multiple words.
#'
#' @inheritParams most_similar
#'
#' @return
#' Normalized sum vector.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{most_similar}}
#'
#' \code{\link{dict_expand}}
#'
#' \code{\link{dict_reliability}}
#'
#' @examples
#' sum_wordvec(demodata, ~ king - man + woman)
#'
#' @export
sum_wordvec = function(data, x=NULL, verbose=TRUE) {
  embed = force_normalize(as_embed(data), verbose)  # pre-normalized
  if(is.null(x)) {
    sum.vec = colSums(embed)
    sum.vec = sum.vec / sqrt(sum(sum.vec^2))  # post-normalized
    attr(sum.vec, "formula") = "<all>"
    attr(sum.vec, "x.words") = "<all>"
    return(sum.vec)
  }

  if(inherits(x, "character")) {
    ft = paste(x, collapse=" + ")
    positive = x
    negative = character()
  } else if(inherits(x, "formula")) {
    ft = as.character(x[2])
    xt = str_replace_all(ft, "`", "")
    xts = str_split(xt, " (?=[+-])", simplify=TRUE)[1,]
    positive = str_remove(str_subset(xts, "^\\-", negate=TRUE), "\\+ *")
    negative = str_remove(str_subset(xts, "^\\-"), "\\- *")
    x = c(positive, negative)
  } else {
    stop("`x` must be a character vector or an R formula!", call.=FALSE)
  }

  embed.pos = get_wordembed(embed, words=positive)
  embed.neg = get_wordembed(embed, words=negative)
  if(length(negative) == 0)
    sum.vec = colSums(embed.pos)
  else
    sum.vec = colSums(embed.pos) - colSums(embed.neg)
  sum.vec = sum.vec / sqrt(sum(sum.vec^2))  # post-normalized
  attr(sum.vec, "formula") = ft
  attr(sum.vec, "x.words") = x
  return(sum.vec)
}


#### Similarity ####


#' Find the Top-N most similar words.
#'
#' Find the Top-N most similar words, which replicates the results produced
#' by the Python \code{gensim} module \code{most_similar()} function.
#' (Exact replication of \code{gensim} requires the same word vectors data,
#' not the \code{demodata} used here in examples.)
#'
#' @inheritParams get_wordvec
#' @inheritParams data_transform
#' @param x Can be:
#' \itemize{
#'   \item{\code{NULL}: use the sum of all word vectors in \code{data}}
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
#' @param above Defaults to \code{NULL}. Can be:
#' \itemize{
#'   \item{a threshold value to find all words with cosine similarities
#'   higher than this value}
#'   \item{a critical word to find all words with cosine similarities
#'   higher than that with this critical word}
#' }
#' If both \code{topn} and \code{above} are specified, \code{above} wins.
#' @param keep Keep words specified in \code{x} in results?
#' Defaults to \code{FALSE}.
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
#' \code{\link{sum_wordvec}}
#'
#' \code{\link{dict_expand}}
#'
#' \code{\link{dict_reliability}}
#'
#' \code{\link{cosine_similarity}}
#'
#' \code{\link{pair_similarity}}
#'
#' \code{\link{plot_similarity}}
#'
#' \code{\link{tab_similarity}}
#'
#' @examples
#' d = as_wordvec(demodata, normalize=TRUE)
#'
#' most_similar(d)
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
#' }
#' @export
most_similar = function(
    data,
    x=NULL,
    topn=10,
    above=NULL,
    keep=FALSE,
    verbose=TRUE
) {
  embed = force_normalize(as_embed(data), verbose)
  sum.vec = sum_wordvec(embed, x)
  x = attr(sum.vec, "x.words")
  f = attr(sum.vec, "formula")
  cos.sim = embed %*% sum.vec  # faster
  ms = data.table(
    word = rownames(embed),
    cos_sim = as.numeric(cos.sim),
    row_id = 1:nrow(embed)
  )
  word = cos_sim = NULL
  if(keep==FALSE)
    ms = ms[!word %in% x]
  if(is.null(above)) {
    ms = ms[order(-cos_sim)][1:topn]
  } else if(is.numeric(above)) {
    ms = ms[order(-cos_sim)][cos_sim >= above]
  } else if(is.character(above)) {
    ms = ms[order(-cos_sim)][cos_sim >= cos.sim[above,]]
  } else {
    stop("`above` must be a numeric value or a character string.", call.=FALSE)
  }
  gc()  # Garbage Collection: Free the Memory
  attr(ms, "sum.vec") = sum.vec
  attr(ms, "sum.vec.formula") = f
  if(verbose) {
    Print("<<cyan [Word Vector]>> =~ {f}")
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
#' \code{\link{plot_similarity}}
#'
#' \code{\link{tab_similarity}}
#'
#' \code{\link{most_similar}}
#'
#' @examples
#' pair_similarity(demodata, "China", "Chinese")
#'
#' @export
pair_similarity = function(
    data,
    word1,
    word2,
    distance=FALSE
) {
  embed = get_wordembed(data, c(word1, word2))
  cosine_similarity(embed[1,], embed[2,], distance=distance)
}


#' Tabulate cosine similarity/distance of word pairs.
#'
#' @describeIn tab_similarity Tabulate data for all word pairs.
#'
#' @inheritParams cosine_similarity
#' @inheritParams get_wordvecs
#' @param unique Word pairs: unique pairs (\code{TRUE})
#' or full pairs with duplicates (\code{FALSE}; default).
#'
#' @return
#' A \code{data.table} of words, word pairs,
#' and their cosine similarity (\code{cos_sim})
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
#' \code{\link{plot_similarity}}
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
#' ## only n1 * n2 word pairs crossing two sets of words w1 & w2
#' w1 = cc("king, queen")
#' w2 = cc("man, woman")
#' tab_similarity_cross(demodata, w1, w2)
#'
#' @export
tab_similarity = function(
    data,
    words=NULL,
    pattern=NULL,
    unique=FALSE,
    distance=FALSE
) {
  if(!is.null(words) & !is.null(pattern))
    stop("You may use `tab_similarity_cross()` instead.", call.=FALSE)
  embed = get_wordembed(data, words, pattern)
  words.valid = rownames(embed)
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
    cosine_similarity(embed[dts[[i, 1]],],
                      embed[dts[[i, 2]],],
                      distance=distance)
  })
  if(distance) names(dts)[4] = "cos_dist"
  return(dts)
}


#' @describeIn tab_similarity Tabulate data for only n1 * n2 word pairs.
#'
#' @param words1,words2 [Used in \code{tab_similarity_cross}]
#' Two sets of words for computing similarities of n1 * n2 word pairs.
#'
#' @export
tab_similarity_cross = function(
    data,
    words1,
    words2,
    distance=FALSE
) {
  word1 = word2 = NULL
  tab_similarity(data, c(words1, words2))[word1 %in% words1 & word2 %in% words2]
}


#' Visualize cosine similarity of word pairs.
#'
#' @inheritParams tab_similarity
#' @inheritParams corrplot::corrplot
#' @param label Position of text labels.
#' Defaults to \code{"auto"} (add labels if less than 20 words).
#' Can be \code{TRUE} (left and top), \code{FALSE} (add no labels of words),
#' or a character string (see the usage of \code{tl.pos} in \code{\link[corrplot:corrplot]{corrplot}}.
#' @param value.color Color of values added on the plot.
#' Defaults to \code{NULL} (add no values).
#' @param value.percent Whether to transform values into percentage style for space saving.
#' Defaults to \code{FALSE}.
#' @param hclust.n Number of rectangles to be drawn on the plot according to
#' the hierarchical clusters, only valid when \code{order="hclust"}.
#' Defaults to \code{NULL} (add no rectangles).
#' @param hclust.color Color of rectangle border, only valid when \code{hclust.n} >= 1.
#' Defaults to \code{"black"}.
#' @param hclust.line Line width of rectangle border, only valid when \code{hclust.n} >= 1.
#' Defaults to \code{2}.
#' @param file File name to be saved, should be of \code{png} or \code{pdf}.
#' @param width,height Width and height (in inches) for the saved file.
#' Defaults to \code{8} and \code{6}.
#' @param dpi Dots per inch. Defaults to \code{500} (i.e., file resolution: 4000 * 3000).
#' @param ... Other parameters passed to \code{\link[corrplot:corrplot]{corrplot}}.
#'
#' @return
#' Invisibly return a matrix of cosine similarity between each pair of words.
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
#' \code{\link{most_similar}}
#'
#' @examples
#' w1 = cc("king, queen, man, woman")
#' plot_similarity(demodata, w1)
#' plot_similarity(demodata, w1,
#'                 value.color="grey",
#'                 value.percent=TRUE)
#' plot_similarity(demodata, w1,
#'                 value.color="grey",
#'                 order="hclust",
#'                 hclust.n=2)
#'
#' w2 = cc("China, Chinese,
#'          Japan, Japanese,
#'          Korea, Korean,
#'          man, woman, boy, girl,
#'          good, bad, positive, negative")
#' plot_similarity(demodata, w2,
#'                 order="hclust",
#'                 hclust.n=3)
#' plot_similarity(demodata, w2,
#'                 order="hclust",
#'                 hclust.n=7,
#'                 file="plot.png")
#'
#' unlink("plot.png")  # delete file for code check
#'
#' @export
plot_similarity = function(
    data,
    words=NULL,
    pattern=NULL,
    label="auto",
    value.color=NULL,
    value.percent=FALSE,
    order=c("original", "AOE", "FPC", "hclust", "alphabet"),
    hclust.method=c("complete", "ward", "ward.D", "ward.D2",
                    "single", "average", "mcquitty",
                    "median", "centroid"),
    hclust.n=NULL,
    hclust.color="black",
    hclust.line=2,
    file=NULL,
    width=8,
    height=6,
    dpi=500,
    ...
) {
  tab = tab_similarity(data=data,
                       words=words,
                       pattern=pattern,
                       unique=FALSE,
                       distance=FALSE)
  words = unique(tab$word1)
  mat = matrix(tab[[4]], nrow=length(words))
  rownames(mat) = colnames(mat) = words
  if(label=="auto") label = ifelse(length(words)<20, TRUE, FALSE)

  if(!is.null(file)) {
    if(str_detect(file, "\\.png$"))
      png(file, width=width, height=height, units="in", res=dpi)
    if(str_detect(file, "\\.pdf$"))
      pdf(file, width=width, height=height)
  }
  corrplot(mat, method="color",
           tl.pos=label,
           tl.col="black",
           order=order,
           hclust.method=hclust.method,
           addCoef.col=value.color,
           addCoefasPercent=value.percent,
           addrect=hclust.n,
           rect.col=hclust.color,
           rect.lwd=hclust.line,
           ...)
  if(!is.null(file)) {
    dev.off()
    cli::cli_alert_success("Saved to {paste0(getwd(), '/', file)}")
  }

  invisible(mat)
}


#### Dictionary Expansion and Analysis ####


#' Expand a dictionary from the most similar words.
#'
#' @inheritParams most_similar
#' @param words A single word or a list of words,
#' used to calculate the
#' \link[PsychWordVec:sum_wordvec]{sum vector}.
#' @param threshold Threshold of cosine similarity,
#' used to find all words with similarities higher than this value.
#' Defaults to \code{0.5}. A low threshold may lead to failure of convergence.
#' @param iteration Number of maximum iterations. Defaults to \code{5}.
#'
#' @return
#' An expanded list (character vector) of words.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{sum_wordvec}}
#'
#' \code{\link{most_similar}}
#'
#' \code{\link{dict_reliability}}
#'
#' @examples
#' \donttest{dict = dict_expand(demodata, "king")
#' dict
#'
#' dict = dict_expand(demodata, cc("king, queen"))
#' dict
#'
#' most_similar(demodata, dict)
#'
#' dict.cn = dict_expand(demodata, "China")
#' dict.cn  # too inclusive if setting threshold = 0.5
#'
#' dict.cn = dict_expand(demodata,
#'                       cc("China, Chinese"),
#'                       threshold=0.6)
#' dict.cn  # adequate to represent "China"
#' }
#' @export
dict_expand = function(
    data,
    words,
    threshold=0.5,
    iteration=5,
    verbose=TRUE
) {
  embed = force_normalize(as_embed(data), verbose)
  cos_sim = NULL
  i = 1
  while(TRUE) {
    ms = most_similar(data=embed, x=words, keep=FALSE,
                      above=threshold, verbose=FALSE)
    new.words = ms[cos_sim >= threshold]$word
    n.new = length(new.words)
    words = unique(c(words, new.words))
    if(verbose) {
      cli::cli_h1("Iteration {i} (threshold of cosine similarity = {threshold})")
      if(n.new>0)
        cli::cli_alert_success("{n.new} more words appended: {.val {new.words}}")
      else
        cli::cli_alert_success("No more words appended. Successfully convergent.")
    }
    if(n.new==0 | i>=iteration) break
    i = i + 1
  }
  if(verbose) cli::cli_h2("Finish ({ifelse(n.new==0, 'convergent', 'NOT convergent')})")
  return(words)
}


#' Reliability analysis and PCA of a dictionary.
#'
#' Reliability analysis (Cronbach's \eqn{\alpha}) and
#' Principal Component Analysis (PCA) of a dictionary,
#' with \link[PsychWordVec:plot_similarity]{visualization of cosine similarities}
#' between words (ordered by the first principal component loading).
#' Note that Cronbach's \eqn{\alpha} may be misleading
#' when the number of items/words is large.
#'
#' @inheritParams plot_similarity
#' @param sort Sort items by the first principal component loading (PC1)?
#' Defaults to \code{TRUE}.
#' @param ... Other parameters passed to \code{\link{plot_similarity}}.
#'
#' @return
#' A \code{list} object of new class \code{reliability}:
#' \describe{
#'   \item{\code{alpha}}{
#'     Cronbach's \eqn{\alpha}}
#'   \item{\code{eigen}}{
#'     Eigen values from PCA}
#'   \item{\code{pca}}{
#'     PCA (only 1 principal component)}
#'   \item{\code{pca.rotation}}{
#'     PCA with varimax rotation (if potential principal components > 1)}
#'   \item{\code{items}}{
#'     Item statistics}
#'   \item{\code{cos.sim.mat}}{
#'     A matrix of cosine similarities of all word pairs}
#'   \item{\code{cos.sim}}{
#'     Lower triangular part of the matrix of cosine similarities}
#' }
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @references
#' Nicolas, G., Bai, X., & Fiske, S. T. (2021).
#' Comprehensive stereotype content dictionaries using a semi-automated method.
#' \emph{European Journal of Social Psychology, 51}(1), 178--196.
#'
#' @seealso
#' \code{\link{cosine_similarity}}
#'
#' \code{\link{pair_similarity}}
#'
#' \code{\link{plot_similarity}}
#'
#' \code{\link{tab_similarity}}
#'
#' \code{\link{most_similar}}
#'
#' \code{\link{dict_expand}}
#'
#' @examples
#' \donttest{d = data_wordvec_normalize(demodata)
#'
#' dict = dict_expand(d, "king")
#' dict_reliability(d, dict)
#'
#' dict.cn = dict_expand(d, "China", threshold=0.65)
#' dict_reliability(d, dict.cn)
#'
#' dict_reliability(d, c(dict, dict.cn))
#' # low-loading items should be removed
#' }
#' @export
dict_reliability = function(
    data,
    words=NULL,
    pattern=NULL,
    sort=TRUE,
    ...
) {
  embed = force_normalize(as_embed(data))
  embed = get_wordembed(embed, words, pattern)
  sum.vec = sum_wordvec(embed)
  words.valid = rownames(embed)
  suppressMessages({
    suppressWarnings({
      alpha = psych::alpha(t(embed))
      pca = psych::principal(t(embed), nfactors=1, scores=FALSE)
      n.factors = sum(pca$values>1)
      if(n.factors==1)
        pca.rotation = NULL
      else
        pca.rotation = psych::principal(t(embed), nfactors=n.factors,
                                        rotate="varimax", scores=FALSE)
      cos.sim = embed %*% sum.vec  # faster
    })
  })
  items = cbind(pca$loadings[,1],
                cos.sim,
                alpha$item.stats["r.drop"],
                alpha$alpha.drop["raw_alpha"])
  names(items) = c("pc.loading",
                   "sim.sumvec",
                   "item.rest.cor",
                   "alpha.if.drop")

  if(sort)
    items = items[order(items$pc.loading, decreasing=TRUE),]

  mat = plot_similarity(data,
                        words.valid,
                        order="FPC",
                        ...)
  cos.sims = mat[lower.tri(mat)]

  reliability = list(alpha=alpha$total$raw_alpha,
                     eigen=pca$values,
                     pca=pca,
                     pca.rotation=pca.rotation,
                     items=items,
                     cos.sim.mat=mat,
                     cos.sim=cos.sims)
  class(reliability) = "reliability"
  return(reliability)
}


#' @export
print.reliability = function(x, digits=3, ...) {
  cli::cli_h1("Reliability Analysis and PCA of Dictionary")
  cn()
  Print("
    Number of items = {nrow(x$items)}
    Mean cosine similarity = {mean(x$cos.sim):.{digits}}
    Cronbach\u2019s \u03b1 = {x$alpha:.{digits}} (misleading when N of items is large)
    Variance explained by PC1 = {100*x$eigen[1]/sum(x$eigen):.1}%
    Potential principal components = {sum(x$eigen>1)} (with eigen value > 1)")
  cn()
  Print("Cosine Similarities Between Words:")
  print(summary(x$cos.sim))
  cn()
  names(x$items) = c("PC1 Loading",
                     "Item-SumVec Sim.",
                     "Item-Rest Corr.",
                     "Alpha (if dropped)")
  print_table(x$items[,1:3], digits=digits,
              title="Item Statistics:",
              note=Glue("
                PC1 Loading = the first principal component loading
                Item-SumVec Sim. = cosine similarity with the sum vector
                Item-Rest Corr. = corrected item-total correlation"))
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
#' which takes much less computation time and produces the approximate \emph{p} value
#' (comparable to the exact one).
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
#' A \code{list} object of new class \code{weat}:
#' \describe{
#'   \item{\code{words.valid}}{
#'     Valid (actually matched) words}
#'   \item{\code{words.not.found}}{
#'     Words not found}
#'   \item{\code{data.raw}}{
#'     A \code{data.table} of cosine similarities between all word pairs}
#'   \item{\code{data.mean}}{
#'     A \code{data.table} of \emph{mean} cosine similarities
#'     \emph{across} all attribute words}
#'   \item{\code{data.diff}}{
#'     A \code{data.table} of \emph{differential} mean cosine similarities
#'     \emph{between} the two attribute concepts}
#'   \item{\code{eff.label}}{
#'     Description for the difference between the two attribute concepts}
#'   \item{\code{eff.type}}{
#'     Effect type: WEAT or SC-WEAT}
#'   \item{\code{eff}}{
#'     Raw effect, standardized effect size, and p value (if \code{p.perm=TRUE})}
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
#' \code{\link{dict_expand}}
#'
#' \code{\link{dict_reliability}}
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
test_WEAT = function(
    data, T1, T2, A1, A2,
    use.pattern=FALSE,
    labels=list(),
    p.perm=TRUE,
    p.nsim=10000,
    p.side=2,
    seed=NULL,
    pooled.sd="Caliskan"
) {
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
  embed = as_embed(data)
  if(use.pattern) {
    if(!is.null(T1)) {
      message(Glue("T1 ({labels$T1}):"))
      T1 = rownames(get_wordembed(embed, pattern=T1))
    }
    if(!is.null(T2)) {
      message(Glue("T2 ({labels$T2}):"))
      T2 = rownames(get_wordembed(embed, pattern=T2))
    }
    if(!is.null(A1)) {
      message(Glue("A1 ({labels$A1}):"))
      A1 = rownames(get_wordembed(embed, pattern=A1))
    }
    if(!is.null(A2)) {
      message(Glue("A2 ({labels$A2}):"))
      A2 = rownames(get_wordembed(embed, pattern=A2))
    }
  }
  words = c(T1, T2, A1, A2)
  embed = force_normalize(embed, verbose=FALSE)
  embed = get_wordembed(embed, words)
  words.valid = rownames(embed)

  # words not found:
  not.found = setdiff(words, words.valid)
  # valid words:
  T1 = T1[T1 %in% words.valid]
  T2 = T2[T2 %in% words.valid]
  A1 = A1[A1 %in% words.valid]
  A2 = A2[A2 %in% words.valid]

  dweat = rbind(
    expand.grid(Target=labels$T1, Attrib=labels$A1, T_word=T1, A_word=A1),
    expand.grid(Target=labels$T1, Attrib=labels$A2, T_word=T1, A_word=A2),
    expand.grid(Target=labels$T2, Attrib=labels$A1, T_word=T2, A_word=A1),
    expand.grid(Target=labels$T2, Attrib=labels$A2, T_word=T2, A_word=A2))
  attr(dweat, "out.attrs") = NULL
  dweat$cos_sim = sapply(1:nrow(dweat), function(i) {
    T_word = as.character(dweat[[i, 3]])
    A_word = as.character(dweat[[i, 4]])
    sum(embed[T_word,] * embed[A_word,])
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
#' A \code{list} object of new class \code{rnd}:
#' \describe{
#'   \item{\code{words.valid}}{
#'     Valid (actually matched) words}
#'   \item{\code{words.not.found}}{
#'     Words not found}
#'   \item{\code{data.raw}}{
#'     A \code{data.table} of (absolute and relative) norm distances}
#'   \item{\code{eff.label}}{
#'     Description for the difference between the two attribute concepts}
#'   \item{\code{eff.type}}{
#'     Effect type: RND}
#'   \item{\code{eff}}{
#'     Raw effect and p value (if \code{p.perm=TRUE})}
#'   \item{\code{eff.interpretation}}{
#'     Interpretation of the RND score}
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
#' \code{\link{dict_expand}}
#'
#' \code{\link{dict_reliability}}
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
test_RND = function(
    data, T1, A1, A2,
    use.pattern=FALSE,
    labels=list(),
    p.perm=TRUE,
    p.nsim=10000,
    p.side=2,
    seed=NULL
) {
  if(!p.side %in% 1:2) stop("`p.side` should be 1 or 2.", call.=FALSE)
  if(missing(A1)) stop("Please specify `A1`.", call.=FALSE)
  if(missing(A2)) stop("Please specify `A2`.", call.=FALSE)
  if(missing(T1)) stop("Please specify `T1`.", call.=FALSE)
  if(length(labels)==0)
    labels = list(T1="Target", A1="Attrib1", A2="Attrib2")

  embed = as_embed(data)
  if(use.pattern) {
    if(!is.null(T1)) {
      message(Glue("T1 ({labels$T1}):"))
      T1 = rownames(get_wordembed(embed, pattern=T1))
    }
    if(!is.null(A1)) {
      message(Glue("A1 ({labels$A1}):"))
      A1 = rownames(get_wordembed(embed, pattern=A1))
    }
    if(!is.null(A2)) {
      message(Glue("A2 ({labels$A2}):"))
      A2 = rownames(get_wordembed(embed, pattern=A2))
    }
  }
  words = c(T1, A1, A2)
  embed = force_normalize(embed, verbose=FALSE)
  embed = get_wordembed(embed, words)
  words.valid = rownames(embed)

  # words not found:
  not.found = setdiff(words, words.valid)
  # valid words:
  T1 = T1[T1 %in% words.valid]
  A1 = A1[A1 %in% words.valid]
  A2 = A2[A2 %in% words.valid]

  # v1 = rowMeans(dt[, A1, with=FALSE])  # average vector for A1
  # v2 = rowMeans(dt[, A2, with=FALSE])  # average vector for A2
  v1 = colMeans(embed[A1,])  # average vector for A1
  v2 = colMeans(embed[A2,])  # average vector for A2
  drnd = data.table(T_word = T1)
  drnd$norm_dist_A1 = sapply(1:length(T1), function(i) {
    vm = embed[T1[i],]
    sqrt(sum((vm - v1)^2))
  })
  drnd$norm_dist_A2 = sapply(1:length(T1), function(i) {
    vm = embed[T1[i],]
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


#' Tokenize raw text for training word embeddings.
#'
#' @inheritParams data_transform
#' @param text A character vector of text,
#' or a file path on disk containing text.
#' @param tokenizer Function used to tokenize the text.
#' Defaults to \code{\link[text2vec:tokenizers]{text2vec::word_tokenizer}}.
#' @param split Separator between tokens, only used when \code{simplify=TRUE}.
#' Defaults to \code{" "}.
#' @param remove Strings (in regular expression) to be removed from the text.
#' Defaults to \code{"_|'|<br/>|<br />|e\\\\.g\\\\.|i\\\\.e\\\\."}.
#' You may turn off this by specifying \code{remove=NULL}.
#' @param encoding Text encoding (only used if \code{text} is a file).
#' Defaults to \code{"UTF-8"}.
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
#' Train static word embeddings using the
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
#'   \item{\code{"word2vec"} (default):
#'   using the \code{\link[word2vec:word2vec]{word2vec}} package}
#'   \item{\code{"glove"}:
#'   using the \code{\link[rsparse:GloVe]{rsparse}} and
#'   \code{\link[text2vec:text2vec]{text2vec}} packages}
#'   \item{\code{"fasttext"}:
#'   using the \code{\link[fastTextR:ft_train]{fastTextR}} package}
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
#'   \item{\code{"skip-gram"} (default): Skip-Gram,
#'   which predicts surrounding words given the current word}
#'   \item{\code{"cbow"}: Continuous Bag-of-Words,
#'   which predicts the current word based on the context}
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
#' A \code{wordvec} (data.table) with three variables:
#' \code{word}, \code{vec}, \code{freq}.
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


