#### Initialize ####


#' @import stringr
#' @import ggplot2
#' @import data.table
#' @importFrom stats cor
#' @importFrom utils head menu packageVersion capture.output
#' @importFrom corrplot corrplot
#' @importFrom grDevices png pdf dev.off
#' @importFrom dplyr %>% select left_join
#' @importFrom bruceR cc dtime import export Glue Print print_table
.onAttach = function(libname, pkgname) {
  ## Version Check
  new = FALSE
  inst.ver = as.character(packageVersion("PsychWordVec"))
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
  pkgs = c("data.table", "dplyr", "stringr", "ggplot2")

  suppressMessages({
    suppressWarnings({
      loaded = sapply(pkgs, require, character.only=TRUE)
    })
  })

  ## Welcome Message
  if(all(loaded)) {
    cli::cli_h1("PsychWordVec (v{inst.ver})")
    cn()
    cli::cli_alert_success("
    Packages also loaded: {.pkg data.table, dplyr, stringr, ggplot2}
    ")
    cn()
    # cli::cli_text("
    # {.href [Documentation](https://psychbruce.github.io/PsychWordVec)}
    # | Download pre-trained word vectors:
    # {.url https://psychbruce.github.io/WordVector_RData.pdf}
    # ")
    cli::cli_text("
    Documentation: {.url https://psychbruce.github.io/PsychWordVec}
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


#' Normalize all word vectors to the unit length 1.
#'
#' @description
#' L2-normalization (scaling to unit euclidean length):
#' the \emph{norm} of each vector in the vector space will be normalized to 1.
#' It is necessary for any linear operation of word vectors.
#'
#' R code:
#' \itemize{
#'   \item{Vector: \code{vec / sqrt(sum(vec^2))}}
#'   \item{Matrix: \code{mat / sqrt(rowSums(mat^2))}}
#' }
#'
#' @param x A \code{\link[PsychWordVec:as_wordvec]{wordvec}} (data.table) or
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
#' \code{\link{load_wordvec}} / \code{\link{load_embed}}
#'
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' d = normalize(demodata)
#' # the same: d = as_wordvec(demodata, normalize=TRUE)
#'
#' @export
normalize = function(x) {
  # L2-normalized (unit euclidean length)
  if(is.null(attr(x, "normalized")))
    attr(x, "normalized") = FALSE
  if(attr(x, "normalized")) return(x)
  if(is.wordvec(x))
    x$vec = lapply(x$vec, function(v) { v / sqrt(sum(v^2)) } )
  if(is.matrix(x))
    x = x / sqrt(rowSums(x^2))  # matrix is much faster!
  attr(x, "normalized") = TRUE
  # gc()  # Garbage Collection: Free the Memory
  return(x)
}


force_normalize = function(x, verbose=TRUE) {
  check_data_validity(x)
  if(attr(x, "normalized")==FALSE) {
    if(verbose)
      cli::cli_alert_warning("Results may be inaccurate if word vectors are not normalized.")
    x = normalize(x)
    if(verbose)
      cli::cli_alert_success("All word vectors now have been automatically normalized.")
  }
  return(x)
}


#' Word vectors data class: \code{wordvec} and \code{embed}.
#'
#' \code{PsychWordVec} uses two types of word vectors data:
#' \code{wordvec} (data.table, with two variables \code{word} and \code{vec})
#' and \code{embed} (matrix, with dimensions as columns and words as row names).
#' Note that matrix operation makes \code{embed} much faster than \code{wordvec}.
#' Users are suggested to reshape data to \code{embed} before using the other functions.
#'
#' @describeIn as_embed From \code{wordvec} (data.table) to \code{embed} (matrix).
#'
#' @param x Object to be reshaped. See examples.
#' @param normalize Normalize all word vectors to unit length?
#' Defaults to \code{FALSE}. See \code{\link{normalize}}.
#'
#' @return
#' A \code{wordvec} (data.table) or \code{embed} (matrix).
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{load_wordvec}} / \code{\link{load_embed}}
#'
#' \code{\link{normalize}}
#'
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' dt = head(demodata, 10)
#' str(dt)
#'
#' embed = as_embed(dt, normalize=TRUE)
#' embed
#' str(embed)
#'
#' wordvec = as_wordvec(embed, normalize=TRUE)
#' wordvec
#' str(wordvec)
#'
#' df = data.frame(token=LETTERS, D1=1:26/10000, D2=26:1/10000)
#' as_embed(df)
#' as_wordvec(df)
#'
#' dd = rbind(dt[1:5], dt[1:5])
#' dd  # duplicate words
#' unique(dd)
#'
#' dm = as_embed(dd)
#' dm  # duplicate words
#' unique(dm)
#'
#' \donttest{# more examples for extracting a subset using `x[i, j]`
#' # (3x faster than `wordvec`)
#' embed = as_embed(demodata)
#' embed[1]
#' embed[1:5]
#' embed["for"]
#' embed[pattern("^for.{0,2}$")]
#' embed[cc("for, in, on, xxx")]
#' embed[cc("for, in, on, xxx"), 5:10]
#' embed[1:5, 5:10]
#' embed[, 5:10]
#' embed[3, 4]
#' embed["that", 4]
#' }
#' @export
as_embed = function(x, normalize=FALSE) {
  if(is.embed(x) & normalize==FALSE) return(x)
  dims = attr(x, "dims")
  norm = attr(x, "normalized")
  null.dims = is.null(dims)
  null.norm = is.null(norm)
  if(is.matrix(x)) {
    # x = x
  } else if(is.numeric(x)) {
    x = t(matrix(x))
    rownames(x) = ""
  } else if(is.wordvec(x)) {
    # matrix is much faster!
    x = matrix(unlist(x[[2]]),  # vec
               nrow=nrow(x),
               byrow=TRUE,
               dimnames=list(x[[1]]))  # word or token
  } else if(is.data.frame(x)) {
    if(is.character(x[[1]])) {
      x = matrix(as.matrix(x[,-1]),
                 nrow=nrow(x),
                 dimnames=list(x[[1]]))
    } else {
      x = matrix(as.matrix(x),
                 nrow=nrow(x),
                 dimnames=list(rownames(x)))
    }
  } else {
    stop("`x` must be a matrix, data.frame, or data.table!", call.=FALSE)
  }
  if(is.null(colnames(x)))
    colnames(x) = paste0("dim", seq_len(ncol(x)))
  attr(x, "dims") = ifelse(null.dims, ncol(x), dims)
  attr(x, "normalized") = ifelse(null.norm, FALSE, norm)
  class(x) = c("embed", "matrix", "array")
  if(normalize) x = normalize(x)
  return(x)
}


#' @describeIn as_embed From \code{embed} (matrix) to \code{wordvec} (data.table).
#' @export
as_wordvec = function(x, normalize=FALSE) {
  if(is.wordvec(x) & normalize==FALSE) return(x)
  dims = attr(x, "dims")
  norm = attr(x, "normalized")
  null.dims = is.null(dims)
  null.norm = is.null(norm)
  if(is.data.frame(x) & !is.data.table(x)) {
    x = as_embed(x)
    dims = ncol(x)
    null.dims = FALSE
    x = as_wordvec(x)
  } else if(is.data.table(x)) {
    # x = x
  } else if(is.matrix(x)) {
    dims = ncol(x)
    null.dims = FALSE
    x = data.table(
      word = rownames(x),
      vec = lapply(1:nrow(x), function(i) {
        as.numeric(x[i,])
      })
    )
  } else {
    stop("`x` must be a matrix, data.frame, or data.table!", call.=FALSE)
  }
  attr(x, "dims") = ifelse(null.dims, length(x[[1, "vec"]]), dims)
  attr(x, "normalized") = ifelse(null.norm, FALSE, norm)
  class(x) = c("wordvec", "data.table", "data.frame")
  if(normalize) x = normalize(x)
  return(x)
}


#' @export
print.embed = function(x, maxn=100, ...) {
  class(x) = c("matrix", "array")
  n = nrow(x)
  ndim = ncol(x)
  dims = paste0("<", ndim, " dims>")
  norm = ifelse(attr(x, "normalized"), "(normalized)", "(NOT normalized)")
  cols = c(1, 2, ndim)
  colnames = colnames(x)[cols]
  fmt = paste0("% ", nchar(n)+1, "s")
  is.duplicate = duplicated(rownames(x))
  has.duplicate = any(is.duplicate)

  if(n > maxn) {
    rows = c(1:5, (n-4):n)
    null = t(matrix(rep("", 3)))
    rownames(null) = ""
    x = x[rows, cols]
    rowids = sprintf(fmt, c(
      paste0(rows[1:5], ":"),
      paste(rep("-", nchar(n)+1), collapse=""),
      paste0(rows[6:10], ":")
    ))
  } else {
    if(n == 1) {
      x = matrix(x[, cols],  # numeric vector
                 nrow=1,
                 byrow=TRUE,
                 dimnames=list(rownames(x),
                               colnames))
    } else {
      x = x[, cols]
    }
    rowids = sprintf(fmt, paste0(1:n, ":"))
  }
  x[, 1] = sprintf("% .4f", x[, 1])
  x[, 2] = "..."
  x[, 3] = dims
  if(n > maxn) x = rbind(x[1:5,], null, x[6:10,])
  rownames = rownames(x)  # otherwise, data.frame will print duplicate row names with extra ids
  x = as.data.frame(x)
  rownames(x) = paste(rowids, rownames)
  colnames(x)[2] = "..."
  cli::cli_text(grey("{.strong # embed (matrix)}: [{n} \u00d7 {ndim}] {norm}"))
  print(x)
  if(has.duplicate)
    cli::cli_alert_warning("
    {sum(is.duplicate)} duplicate words: use {.pkg `unique()`} to delete duplicates")
}


#' @export
print.wordvec = function(x, maxn=100, ...) {
  n = nrow(x)
  ndim = attr(x, "dims")
  dims = paste0("<", ndim, " dims>")
  norm = ifelse(attr(x, "normalized"), "(normalized)", "(NOT normalized)")
  fmt = paste0("% ", nchar(n)+1, "s")
  is.duplicate = duplicated(x, by=1)  # faster than duplicated(x[[1]])
  has.duplicate = any(is.duplicate)

  if(n > maxn) {
    rows = c(1:5, (n-4):n)
    null = as.data.frame(t(rep("", ncol(x))))
    rownames(null) = ""
    colnames(null) = names(x)
    x = x[rows,]
    rowids = sprintf(fmt, c(
      paste0(rows[1:5], ":"),
      paste(rep("-", nchar(n)+1), collapse=""),
      paste0(rows[6:10], ":")
    ))
  } else {
    rowids = sprintf(fmt, paste0(1:n, ":"))
  }
  x$vec = sapply(x$vec, function(i) {
    paste0("[", sprintf("% .4f", i[1]), ", ...", dims, "]")
  })
  x = as.data.frame(x)
  if(n > maxn) x = rbind(x[1:5,], null, x[6:10,])
  rownames(x) = rowids
  cli::cli_text(grey("{.strong # wordvec (data.table)}: [{n} \u00d7 {ncol(x)}] {norm}"))
  print(x)
  if(has.duplicate)
    cli::cli_alert_warning("
    {sum(is.duplicate)} duplicate words: use {.pkg `unique()`} to delete duplicates")
}


#' @export
str.embed = function(object, ...) {
  rn = paste(paste0("\"", head(rownames(object), 5), "\""), collapse=" ")
  cn = paste(paste0("\"", head(colnames(object), 5), "\""), collapse=" ")
  Print("
  Class \"embed\" [{nrow(object)} \u00d7 {ncol(object)}] (inherits: \"matrix\")
  - rownames(*) : {rn}{ifelse(nrow(object) > 5, ' ...', '')}
  - colnames(*) : {cn}{ifelse(ncol(object) > 5, ' ...', '')}
  - attr(*, \"dims\") = {attr(object, 'dims')}
  - attr(*, \"normalized\") = {attr(object, 'normalized')}
  ")
}


#' @export
str.wordvec = function(object, ...) {
  words = paste(paste0("\"", head(object, 5)[[1]], "\""), collapse=" ")
  Print("
  Class \"wordvec\" [{nrow(object)} \u00d7 {ncol(object)}] (inherits: \"data.table\")
  $ {names(object)[1]} : {words}{ifelse(nrow(object) > 5, ' ...', '')}
  $ {names(object)[2]} : list of {nrow(object)}
  - attr(*, \"dims\") = {attr(object, 'dims')}
  - attr(*, \"normalized\") = {attr(object, 'normalized')}
  ")
}


#' @rdname as_embed
#' @param i,j Row (\code{i}) and column (\code{j}) filter to be used in \code{embed[i, j]}.
## @param ... Not used.
#' @export
`[.embed` = function(x, i, j) {
  if(missing(i) & missing(j)) {
    return(x)
  } else {
    dims = attr(x, "dims")
    norm = attr(x, "normalized")
    class(x) = c("matrix", "array")
    i.miss = missing(i)
    j.miss = missing(j)
    rown = rownames(x)
    dimn = colnames(x)
    if(i.miss) i = seq_len(nrow(x))
    if(j.miss) j = dimn
    if(inherits(i, "pattern")) i = str_which(rown, i)
    if(is.logical(i)) i = which(i==TRUE)
    if(is.character(i)) {
      words.valid = intersect(i, rown)  # faster
      if(length(words.valid) < length(i))
        warning_not_found(setdiff(i, words.valid))
      if(length(words.valid) == 0) {
        rm(rown, dimn)
        return(NA)
      }
      i = words.valid
    }
    if(length(i) == 1) {
      if(length(j) == 1) {
        # x[1, 1]
        # x["in", "dim10"]
        v = x[i, j]
        word = ifelse(is.numeric(i), rown[i], i)
        dimj = ifelse(is.numeric(j), paste0("[", dimn[j], "]"),
                      paste0("[", j, "]"))
        names(v) = paste(word, dimj)
        rm(rown, dimn, word, dimj)
        return(v)
      } else {
        # x[1]
        # x["for"]
        # x[1, ]
        # x[1, 2:5]
        # x["for", paste0("dim", 3:10)]
        word = ifelse(is.numeric(i), rown[i], i)
        dimj = if(is.numeric(j)) dimn[j] else j
        x = matrix(if(j.miss) x[i, ] else x[i, j],  # numeric vector
                   nrow=1,
                   byrow=TRUE,
                   dimnames=list(word, dimj))
      }
    } else {
      if(length(j) == 1) {
        # x[1:5, 2]
        # x[cc("for, in, on"), "dim3"]
        v = x[i, j]
        dimj = ifelse(is.numeric(j), paste0("[dim", j, "]"),
                      paste0("[", j, "]"))
        names(v) = paste(names(v), dimj)
        rm(rown, dimn, dimj)
        return(v)
      } else {
        # x[1:5]
        # x[cc("for, in, on")]
        # x[1:5, 2:5]
        # x[cc("for, in, on"), 2:5]
        # x[cc("for, in, on"), paste0("dim", 2:5)]
        word = if(is.numeric(i)) rown[i] else i
        dimj = if(is.numeric(j)) dimn[j] else j
        x = matrix(if(j.miss) x[i, ] else x[i, j],
                   nrow=length(i),
                   dimnames=list(word, dimj))
      }
    }
    rm(rown, dimn, word, dimj)
    attr(x, "dims") = dims
    attr(x, "normalized") = norm
    return(as_embed(x))
  }
}


#' @rdname as_embed
#' @param pattern Regular expression to be used in \code{embed[pattern("...")]}.
#' @export
pattern = function(pattern) {
  class(pattern) = c("pattern", "character")
  return(pattern)
}


#' @export
rbind.embed = function(...) {
  m = do.call("rbind", lapply(list(...), function(m) {
    class(m) = c("matrix", "array")
    return(m)
  }))
  return(as_embed(m))
}


#' @export
rbind.wordvec = function(...) {
  d = rbindlist(lapply(list(...), function(d) {
    as.data.table(d)
  }))
  return(as_wordvec(d))
}


#' @export
unique.embed = function(x, ...) {
  as_embed(x[which(!duplicated(rownames(x))),])
}


#' @export
unique.wordvec = function(x, ...) {
  x[!duplicated(x[[1]]),]
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
#' \code{\link{load_wordvec}} / \code{\link{load_embed}}
#'
#' \code{\link{normalize}}
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
        vec = lapply(
          str_split(dt$x, sep, n=2, simplify=TRUE)[,2],
          function(v) { as.numeric(cc(v, sep=sep)) })
      )
      dims = unique(sapply(x$vec, length))
      ndim = length(x[[1, "vec"]])
      class(x) = c("wordvec", "data.table", "data.frame")
    } else {
      # matrix
      x = do.call("rbind", lapply(
        str_split(dt$x, sep, n=2, simplify=TRUE)[,2],
        function(v) { as.numeric(cc(v, sep=sep)) }))
      rownames(x) = str_split(dt$x, sep, n=2, simplify=TRUE)[,1]
      dims = ndim = ncol(x)
      colnames(x) = paste0("dim", 1:ndim)
      class(x) = c("embed", "matrix", "array")
    }
    if(length(dims) > 1)
      warning("The number of dimensions is not consistent between words!", call.=FALSE)
    if(verbose)
      Print("Word vectors data: {nrow(x)} vocab, {ndim} dims (time cost = {dtime(t0, 'auto')})")
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
      cli::cli_alert_success("Saved to {.pkg {file.save}} (time cost = {dtime(t1, 'mins')})")
  }
  gc()  # Garbage Collection: Free the Memory
  if(verbose)
    cli::cli_h2("Total time cost: {dtime(t0, 'mins')}")
  invisible(x)
}


#' Load word vectors data (\code{wordvec} or \code{embed}) from ".RData" file.
#'
#' @inheritParams as_embed
#' @inheritParams data_transform
#' @param file File name of .RData transformed by \code{\link{data_transform}}.
#' Can also be an .RData file containing an embedding matrix with words as row names.
#' @param as Load as
#' \code{\link[PsychWordVec:as_wordvec]{wordvec}} (data.table) or
#' \code{\link[PsychWordVec:as_embed]{embed}} (matrix).
#' Defaults to the original class of the R object in \code{file}.
#' The two wrapper functions \code{load_wordvec} and \code{load_embed}
#' automatically reshape the data to the corresponding class and
#' normalize all word vectors (for faster future use).
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
#' \code{\link{normalize}}
#'
#' \code{\link{data_transform}}
#'
#' \code{\link{data_wordvec_subset}}
#'
#' @examples
#' d = demodata[1:200]
#' save(d, file="demo.RData")
#' d = load_wordvec("demo.RData")
#' d
#' d = load_embed("demo.RData")
#' d
#' unlink("demo.RData")  # delete file for code check
#'
#' \dontrun{
#' # please first manually download the .RData file
#' # (see https://psychbruce.github.io/WordVector_RData.pdf)
#' # or transform plain text data by using `data_transform()`
#'
#' # the RData file must be on your disk
#' # the following code cannot run unless you have the file
#' library(bruceR)
#' set.wd()
#' d = load_embed("../data-raw/GloVe/glove_wiki_50d.RData")
#' d
#' }
#'
#' @export
data_wordvec_load = function(
    file,
    as=c("wordvec", "embed"),
    normalize=FALSE,
    verbose=TRUE
) {
  if(!any(as %in% c("wordvec", "embed")))
    stop("`as` should be \"wordvec\" or \"embed\".", call.=FALSE)
  t0 = Sys.time()
  check_load_validity(file)
  if(verbose) cat("Loading...")
  envir = new.env()
  load(file=file, envir=envir)
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
  if(length(as)==1) {
    if(as=="wordvec")
      x = as_wordvec(x, normalize)
    if(as=="embed")
      x = as_embed(x, normalize)
  } else {
    if(is.wordvec(x))
      x = as_wordvec(x, normalize)
    if(is.embed(x))
      x = as_embed(x, normalize)
  }
  if(verbose) {
    cat("\015")
    cli::cli_alert_success("Word vectors data: {nrow(x)} vocab, {ndim} dims (time cost = {dtime(t0)})")
    if(normalize)
      cli::cli_alert_success("All word vectors have been normalized to unit length 1.")
  }
  gc()  # Garbage Collection: Free the Memory
  return(x)
}


#' @rdname data_wordvec_load
#' @export
load_wordvec = function(file, normalize=TRUE) {
  data_wordvec_load(file, as="wordvec", normalize)
}


#' @rdname data_wordvec_load
#' @export
load_embed = function(file, normalize=TRUE) {
  data_wordvec_load(file, as="embed", normalize)
}


#### Subset ####


vocab_str_subset = function(vocab, pattern) {
  words.valid = str_subset(vocab, pattern)
  message(Glue("{length(words.valid)} words matched..."))
  return(words.valid)
}


extract_valid_subset = function(
    x, words=NULL, pattern=NULL, words.order=TRUE
) {
  if(is.wordvec(x)) vocab = x[[1]]  # word or token
  if(is.embed(x)) vocab = rownames(x)

  if(is.null(words)) {
    if(is.null(pattern))
      return(x)  # new default
    else
      words.valid = vocab_str_subset(vocab, pattern)
  } else {
    words.valid = intersect(words, vocab)  # faster
  }
  if(length(words.valid) < length(words))
    warning_not_found(setdiff(words, words.valid))

  if(length(words.valid) == 0)
    return(NULL)
  if(is.wordvec(x)) {
    if(words.order)
      return(NULL)  # not yet implemented...
    else
      return(x[word %in% words.valid])
  }
  if(is.embed(x)) {
    class(x) = c("matrix", "array")
    if(length(words.valid) == 1) {
      xs = t(x[words.valid,])
      rownames(xs) = words.valid
      return(as_embed(xs))
    } else {
      if(words.order)
        return(as_embed(x[words.valid,]))
      else
        return(as_embed(x[which(rownames(x) %in% words.valid),]))
    }
  }
}


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
#' @param words [Option 1] Character string(s).
#' @param pattern [Option 2] Regular expression (see \code{\link[stringr:str_subset]{str_subset}}).
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
#' \code{\link{load_wordvec}} / \code{\link{load_embed}}
#'
#' \code{\link{get_wordvec}}
#'
#' \code{\link{data_transform}}
#'
#' @examples
#' ## directly use `embed[i, j]` (3x faster than `wordvec`):
#' d = as_embed(demodata)
#' d[1:5]
#' d["people"]
#' d[c("China", "Japan", "Korea")]
#'
#' ## specify `x` as a `wordvec` or `embed` object:
#' subset(demodata, c("China", "Japan", "Korea"))
#' subset(d, pattern="^Chi")
#'
#' \donttest{## specify `x` and `pattern`, and save with `file.save`:
#' subset(demodata, pattern="Chin[ae]|Japan|Korea",
#'        file.save="subset.RData")
#'
#' ## load the subset:
#' d.subset = load_wordvec("subset.RData")
#' d.subset
#'
#' ## specify `x` as an .RData file and save with `file.save`:
#' data_wordvec_subset("subset.RData",
#'                     words=c("China", "Chinese"),
#'                     file.save="new.subset.RData")
#' d.new.subset = load_embed("new.subset.RData")
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
    # wordvec or embed
  } else if(is.character(x)) {
    file.load = paste(x, collapse="")
    if(!str_detect(file.load, "\\.rda$|\\.[Rr][Dd]ata$"))
      stop("`x` must be .RData!", call.=FALSE)
    x = data_wordvec_load(file.load)
  } else {
    stop("`x` must be one of them:
      - a `wordvec` or `embed` loaded by `data_wordvec_load()`
      - an .RData file transformed by `data_transform()`", call.=FALSE)
  }
  x = extract_valid_subset(x, words, pattern, words.order=FALSE)
  if(length(as)==1) {
    if(as=="wordvec")
      x = as_wordvec(x)
    if(as=="embed")
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
      cli::cli_alert_success("Saved to {.pkg {file.save}} (time cost = {dtime(t1, 'auto')})")
  }
  # gc()  # Garbage Collection: Free the Memory
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


#### Get Word Vectors ####


#' Extract word vector(s).
#'
#' Extract word vector(s), using either a list of words or a regular expression.
#'
#' @inheritParams data_wordvec_subset
#' @param data A \code{\link[PsychWordVec:as_wordvec]{wordvec}} (data.table) or
#' \code{\link[PsychWordVec:as_embed]{embed}} (matrix),
#' see \code{\link{data_wordvec_load}}.
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
#' \code{\link{data_wordvec_subset}}
#'
#' \code{\link{plot_wordvec}}
#'
#' \code{\link{plot_wordvec_tSNE}}
#'
#' @examples
#' d = as_embed(demodata, normalize=TRUE)
#'
#' get_wordvec(d, c("China", "Japan", "Korea"))
#' get_wordvec(d, cc(" China, Japan; Korea "))
#'
#' ## specify `pattern`:
#' get_wordvec(d, pattern="Chin[ae]|Japan|Korea")
#'
#' ## plot word vectors:
#' get_wordvec(d, cc("China, Japan, Korea,
#'                    Mac, Linux, Windows"),
#'             plot=TRUE, plot.dims=1:100)
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
#' dt = get_wordvec(
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
get_wordvec = function(
    data,
    words=NULL,
    pattern=NULL,
    plot=FALSE,
    plot.dims=NULL,
    plot.step=0.05,
    plot.border="white"
) {
  data = get_wordembed(data, words, pattern)
  if(is.null(data)) return(NULL)
  data = as.data.table(t(data))
  if(plot) {
    p = plot_wordvec(data, dims=plot.dims,
                     step=plot.step, border=plot.border)
    attr(data, "ggplot") = p
    print(p)
  }
  # gc()  # Garbage Collection: Free the Memory
  return(data)
}


get_wordembed = function(x, words=NULL, pattern=NULL, words.order=TRUE) {
  extract_valid_subset(as_embed(x), words, pattern, words.order)
}


#' Visualize word vectors.
#'
#' @param x Can be:
#' \itemize{
#'   \item{a \code{data.table} returned by \code{\link{get_wordvec}}}
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
#' \code{\link{get_wordvec}}
#'
#' \code{\link{plot_similarity}}
#'
#' \code{\link{plot_wordvec_tSNE}}
#'
#' @examples
#' d = as_embed(demodata, normalize=TRUE)
#'
#' plot_wordvec(d[1:10])
#'
#' \donttest{dt = get_wordvec(d, cc("king, queen, man, woman"))
#' dt[, QUEEN := king - man + woman]
#' dt[, QUEEN := QUEEN / sqrt(sum(QUEEN^2))]  # normalize
#' names(dt)[5] = "king - man + woman"
#' plot_wordvec(dt[, c(1,3,4,5,2)], dims=1:50)
#'
#' dt = get_wordvec(d, cc("boy, girl, he, she"))
#' dt[, GIRL := boy - he + she]
#' dt[, GIRL := GIRL / sqrt(sum(GIRL^2))]  # normalize
#' names(dt)[5] = "boy - he + she"
#' plot_wordvec(dt[, c(1,3,4,5,2)], dims=1:50)
#'
#' dt = get_wordvec(d, cc("
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
#' \code{\link{plot_network}}
#'
#' @examples
#' d = as_embed(demodata, normalize=TRUE)
#'
#' dt = get_wordvec(d, cc("
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
#'
#' @return
#' Normalized sum vector.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{normalize}}
#'
#' \code{\link{most_similar}}
#'
#' \code{\link{dict_expand}}
#'
#' \code{\link{dict_reliability}}
#'
#' @examples
#' sum_wordvec(normalize(demodata), ~ king - man + woman)
#'
#' @export
sum_wordvec = function(data, x=NULL, verbose=TRUE) {
  data = force_normalize(as_embed(data), verbose)  # pre-normalized

  if(is.null(x)) {
    sum.vec = colSums(data)
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

  embed.pos = get_wordembed(data, words=positive, words.order=FALSE)
  embed.neg = get_wordembed(data, words=negative, words.order=FALSE)
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


#' Compute a matrix of cosine similarity/distance of word pairs.
#'
#' @inheritParams cosine_similarity
#' @inheritParams get_wordvec
#' @inheritParams data_wordvec_subset
#' @param words1,words2 [Option 3]
#' Two sets of words for only n1 * n2 word pairs. See examples.
#'
#' @return
#' A matrix of pairwise cosine similarity/distance.
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
#' pair_similarity(demodata, c("China", "Chinese"))
#'
#' pair_similarity(demodata, pattern="^Chi")
#'
#' pair_similarity(demodata,
#'                 words1=c("China", "Chinese"),
#'                 words2=c("Japan", "Japanese"))
#'
#' @export
pair_similarity = function(
    data,
    words=NULL,
    pattern=NULL,
    words1=NULL,
    words2=NULL,
    distance=FALSE
) {
  if(is.null(words1) & is.null(words2)) {
    data = as_embed(data)
    embed = force_normalize(get_wordembed(data, words, pattern), verbose=FALSE)
    rm(data)
    if(distance)
      return(1 - tcrossprod(embed, embed))
    else
      return(tcrossprod(embed, embed))
  } else {
    data = as_embed(data)
    embed1 = force_normalize(get_wordembed(data, words1), verbose=FALSE)
    embed2 = force_normalize(get_wordembed(data, words2), verbose=FALSE)
    rm(data)
    if(distance)
      return(1 - tcrossprod(embed1, embed2))
    else
      return(tcrossprod(embed1, embed2))
  }
}


#' Tabulate cosine similarity/distance of word pairs.
#'
#' @inheritParams pair_similarity
#' @param unique Return unique word pairs (\code{TRUE})
#' or all pairs with duplicates (\code{FALSE}; default).
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
#' ## only n1 * n2 word pairs across two sets of words
#' tab_similarity(demodata,
#'                words1=cc("king, queen, King, Queen"),
#'                words2=cc("man, woman"))
#'
#' @export
tab_similarity = function(
    data,
    words=NULL,
    pattern=NULL,
    words1=NULL,
    words2=NULL,
    unique=FALSE,
    distance=FALSE
) {
  mat = pair_similarity(data, words, pattern, words1, words2, distance)
  word1 = rep(rownames(mat), each=ncol(mat))
  word2 = rep(colnames(mat), times=nrow(mat))
  dt = data.table(
    word1 = word1,
    word2 = word2,
    wordpair = paste0(word1, "-", word2),
    cos_sim = as.numeric(t(mat))
  )
  if(distance) names(dt)[4] = "cos_dist"
  if(unique) {
    if(is.null(words1) & is.null(words2))
      return(unique(dt[as.logical(lower.tri(mat))][word1!=word2], by="wordpair"))
    else
      return(unique(dt[word1!=word2], by="wordpair"))
  } else {
    # return(dt[word1!=word2])
    return(dt)
  }
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
#' @param file File name to be saved, should be png or pdf.
#' @param width,height Width and height (in inches) for the saved file.
#' Defaults to \code{10} and \code{6}.
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
#' \code{\link{plot_network}}
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
#' plot_similarity(
#'   demodata,
#'   words1=cc("man, woman, king, queen"),
#'   words2=cc("he, she, boy, girl, father, mother"),
#'   value.color="grey20"
#' )
#'
#' \donttest{w2 = cc("China, Chinese,
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
#' }
#' @export
plot_similarity = function(
    data,
    words=NULL,
    pattern=NULL,
    words1=NULL,
    words2=NULL,
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
    width=10,
    height=6,
    dpi=500,
    ...
) {
  mat = pair_similarity(data, words, pattern, words1, words2)
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
    cli::cli_alert_success("Saved to {.pkg {paste0(getwd(), '/', file)}}")
  }

  invisible(mat)
}


#' Visualize a (partial correlation) network graph of words.
#'
#' @inheritParams plot_similarity
#' @param index Use which index to perform network analysis?
#' Can be \code{"pcor"} (partial correlation, default and suggested),
#' \code{"cor"} (raw correlation),
#' \code{"glasso"} (graphical lasso-estimation of partial correlation matrix
#' using the \code{glasso} package),
#' or \code{"sim"} (pairwise cosine similarity).
#' @param alpha Significance level to be used for not showing edges. Defaults to \code{0.05}.
#' @param bonf Bonferroni correction of \emph{p} value. Defaults to \code{FALSE}.
#' @param max Maximum value for scaling edge widths and colors. Defaults to the highest value of the index.
#' Can be \code{1} if you want to compare several graphs.
#' @param node.size Node size. Defaults to 8*exp(-nNodes/80)+1.
#' @param node.group Node group(s). Can be a named list (see examples) in which each element
#' is a vector of integers identifying the numbers of the nodes that belong together, or a factor.
#' @param node.color Node color(s). Can be a character vector of colors corresponding to \code{node.group}.
#' Defaults to white (if \code{node.group} is not specified)
#' or the palette of ggplot2 (if \code{node.group} is specified).
#' @param label.text Node label of text. Defaults to original words.
#' @param label.size Node label font size. Defaults to \code{1.2}.
#' @param label.size.equal Make the font size of all labels equal. Defaults to \code{TRUE}.
#' @param label.color Node label color. Defaults to \code{"black"}.
#' @param edge.color Edge colors for positive and negative values, respectively.
#' Defaults to \code{c("#009900", "#BF0000")}.
#' @param edge.label Edge label of values. Defaults to \code{FALSE}.
#' @param edge.label.size Edge label font size. Defaults to \code{1}.
#' @param edge.label.color Edge label color. Defaults to \code{edge.color}.
#' @param edge.label.bg Edge label background color. Defaults to \code{"white"}.
#' @param ... Other parameters passed to \code{\link[qgraph:qgraph]{qgraph}}.
#'
#' @return
#' Invisibly return a \code{\link[qgraph:qgraph]{qgraph}} object,
#' which further can be plotted using \code{plot()}.
#'
#' @section Download:
#' Download pre-trained word vectors data (\code{.RData}):
#' \url{https://psychbruce.github.io/WordVector_RData.pdf}
#'
#' @seealso
#' \code{\link{plot_similarity}}
#'
#' \code{\link{plot_wordvec_tSNE}}
#'
#' @examples
#' d = as_embed(demodata, normalize=TRUE)
#'
#' words = cc("
#' man, woman,
#' he, she,
#' boy, girl,
#' father, mother,
#' mom, dad,
#' China, Japan
#' ")
#'
#' plot_network(d, words)
#'
#' p = plot_network(
#'   d, words,
#'   node.group=list(Gender=1:6, Family=7:10, Country=11:12),
#'   node.color=c("antiquewhite", "lightsalmon", "lightblue"),
#'   file="network.png")
#' plot(p)
#'
#' unlink("network.png")  # delete file for code check
#'
#' \donttest{# network analysis with centrality plot (see `qgraph` package)
#' qgraph::centralityPlot(p, include="all", scale="raw",
#'                        orderBy="Strength")
#'
#' # graphical lasso-estimation of partial correlation matrix
#' plot_network(
#'   d, words,
#'   index="glasso",
#'   # threshold=TRUE,
#'   node.group=list(Gender=1:6, Family=7:10, Country=11:12),
#'   node.color=c("antiquewhite", "lightsalmon", "lightblue"))
#' }
#' @export
plot_network = function(
    data,
    words=NULL,
    pattern=NULL,
    index=c("pcor", "cor", "glasso", "sim"),
    alpha=0.05,
    bonf=FALSE,
    max=NULL,
    node.size="auto",
    node.group=NULL,
    node.color=NULL,
    label.text=NULL,
    label.size=1.2,
    label.size.equal=TRUE,
    label.color="black",
    edge.color=c("#009900", "#BF0000"),
    edge.label=FALSE,
    edge.label.size=1,
    edge.label.color=NULL,
    edge.label.bg="white",
    file=NULL,
    width=10,
    height=6,
    dpi=500,
    ...
) {
  index = match.arg(index)
  if(index=="sim") {
    index = "cor"
    min = 0
    mat = pair_similarity(data, words, pattern)
  } else {
    min = ifelse(index=="glasso", 0, "sig")
    mat = cor(t(get_wordembed(data, words, pattern)))
  }

  if(!is.null(file)) {
    if(str_detect(file, "\\.png$"))
      png(file, width=width, height=height, units="in", res=dpi)
    if(str_detect(file, "\\.pdf$"))
      pdf(file, width=width, height=height)
  }
  p = qgraph::qgraph(
    mat, layout="spring",

    ## --- [graph] --- ##
    graph=index,  # "cor", "pcor", "glasso"
    shape="circle",
    sampleSize=attr(data, "dims"),
    minimum=min,
    maximum=max,
    alpha=alpha,
    bonf=bonf,
    # threshold="sig",  # or other adjust methods
    # details=TRUE,

    ## --- [node] --- ##
    groups=node.group,
    color=node.color,
    palette="ggplot2",
    vsize=if(node.size=="auto") 8*exp(-nrow(mat)/80)+1 else node.size,

    ## --- [label] --- ##
    labels=if(is.null(label.text)) rownames(mat) else label.text,
    label.cex=label.size,
    label.scale.equal=label.size.equal,
    label.color=label.color,

    ## --- [edge] --- ##
    posCol=edge.color[1],
    negCol=edge.color[2],
    edge.labels=edge.label,
    edge.label.cex=edge.label.size,
    edge.label.color=edge.label.color,
    edge.label.bg=edge.label.bg,

    ## --- [others] --- ##
    usePCH=TRUE,
    ...)
  if(!is.null(file)) {
    dev.off()
    cli::cli_alert_success("Saved to {.pkg {paste0(getwd(), '/', file)}}")
  }

  invisible(p)
}


#' Find the Top-N most similar words.
#'
#' Find the Top-N most similar words, which replicates the results produced
#' by the Python \code{gensim} module \code{most_similar()} function.
#' (Exact replication of \code{gensim} requires the same word vectors data,
#' not the \code{demodata} used here in examples.)
#'
#' @inheritParams get_wordvec
#' @inheritParams sum_wordvec
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
#' @param row.id Return the row number of each word? Defaults to \code{TRUE},
#' which may help determine the relative word frequency in some cases.
#'
#' @return
#' A \code{data.table} with the most similar words and their cosine similarities.
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
#' d = as_embed(demodata, normalize=TRUE)
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
    row.id=TRUE,
    verbose=TRUE
) {
  embed = force_normalize(as_embed(data), verbose)
  rm(data)
  sum.vec = sum_wordvec(embed, x)
  x = attr(sum.vec, "x.words")
  f = attr(sum.vec, "formula")
  cos.sim = embed %*% sum.vec  # much faster
  cos.sim = cos.sim[order(cos.sim, decreasing=TRUE),]
  if(keep==FALSE)
    cos.sim = cos.sim[which(!names(cos.sim) %in% x)]
  if(is.null(above)) {
    cos.sim = cos.sim[1:topn]
  } else if(is.numeric(above)) {
    cos.sim = cos.sim[cos.sim >= above]
  } else if(is.character(above)) {
    cos.sim = cos.sim[cos.sim >= cos.sim[above]]
  } else {
    stop("`above` must be a numeric value or a character string.", call.=FALSE)
  }
  if(row.id) {
    ms = data.table(
      word = names(cos.sim),
      cos_sim = cos.sim,
      row_id = which(rownames(embed) %in% names(cos.sim))
    )
  } else {
    ms = data.table(
      word = names(cos.sim),
      cos_sim = cos.sim
    )
  }
  attr(ms, "formula") = f
  if(verbose) {
    Print("<<cyan [Word Vector]>> =~ {f}")
    message("(normalized to unit length)")
  }
  rm(embed, sum.vec, cos.sim)
  # gc()  # Garbage Collection: Free the Memory
  return(ms)
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
  rm(data)
  cos_sim = NULL
  i = 1
  while(TRUE) {
    new.words = most_similar(embed, words, above=threshold,
                             row.id=FALSE, verbose=FALSE)$word
    n.new = length(new.words)
    words = unique(c(words, new.words))
    if(verbose) {
      cli::cli_h1("Iteration {i} (threshold of cosine similarity = {threshold})")
      new.ws = ifelse(n.new > 1, "words", "word")
      if(n.new > 100)
        cli::cli_alert_success("{n.new} more {new.ws} appended: ... (omitted)")
      else if(n.new > 0)
        cli::cli_alert_success("{n.new} more {new.ws} appended: {.val {new.words}}")
      else
        cli::cli_alert_success("No more word appended. Successfully convergent.")
    }
    if(n.new == 0 | i >= iteration) break
    i = i + 1
  }
  if(verbose) cli::cli_h2("Finish ({ifelse(n.new==0, 'convergent', 'NOT convergent')})")
  return(words)
}


#' Reliability analysis and PCA of a dictionary.
#'
#' Reliability analysis (Cronbach's \eqn{\alpha} and average cosine similarity) and
#' Principal Component Analysis (PCA) of a dictionary,
#' with \link[PsychWordVec:plot_similarity]{visualization of cosine similarities}
#' between words (ordered by the first principal component loading).
#' Note that Cronbach's \eqn{\alpha} can be misleading
#' when the number of items/words is large.
#'
#' @inheritParams plot_similarity
#' @param alpha Estimate the Cronbach's \eqn{\alpha}? Defaults to \code{TRUE}.
#' Note that this can be \emph{misleading} and \emph{time-consuming}
#' when the number of items/words is large.
#' @param sort Sort items by the first principal component loading (PC1)?
#' Defaults to \code{TRUE}.
#' @param plot Visualize the cosine similarities? Defaults to \code{TRUE}.
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
#' \donttest{d = as_embed(demodata, normalize=TRUE)
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
    alpha=TRUE,
    sort=TRUE,
    plot=TRUE,
    ...
) {
  embed = force_normalize(as_embed(data), verbose=TRUE)
  rm(data)
  embed = get_wordembed(embed, words, pattern)
  sum.vec = sum_wordvec(embed)
  words.valid = rownames(embed)
  suppressMessages({
    suppressWarnings({
      if(alpha) {
        alpha = psych::alpha(t(embed))
      } else {
        alpha = list()
        alpha$total$raw_alpha = NA
        alpha$item.stats["r.drop"] = NA
        alpha$alpha.drop["raw_alpha"] = NA
      }
      pca = psych::principal(t(embed), nfactors=1, scores=FALSE)
      n.factors = sum(pca$values>1)
      if(n.factors==1)
        pca.rotation = NULL
      else
        pca.rotation = psych::principal(
          t(embed), nfactors=n.factors,
          rotate="varimax", scores=FALSE)
      cos.sim = embed %*% sum.vec  # much faster
    })
  })
  items = cbind(pca$loadings[,1],
                cos.sim,
                alpha$item.stats["r.drop"],
                alpha$alpha.drop["raw_alpha"])
  items = as.data.frame(items)
  names(items) = c("pc.loading",
                   "sim.sumvec",
                   "item.rest.cor",
                   "alpha.if.drop")

  if(sort)
    items = items[order(items$pc.loading, decreasing=TRUE),]

  if(plot)
    mat = plot_similarity(embed, words.valid, order="FPC", ...)
  else
    mat = pair_similarity(embed, words.valid)
  cos.sims = mat[lower.tri(mat)]

  reliability = list(alpha=alpha$total$raw_alpha,
                     eigen=pca$values,
                     pca=pca,
                     pca.rotation=pca.rotation,
                     items=items,
                     cos.sim.mat=mat,
                     cos.sim=cos.sims)
  rm(embed, sum.vec, cos.sim)
  # gc()  # Garbage Collection: Free the Memory
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
  embed = force_normalize(as_embed(data), verbose=FALSE)
  rm(data)
  if(use.pattern) {
    if(!is.null(T1)) {
      message(Glue("T1 ({labels$T1}):"))
      T1 = vocab_str_subset(rownames(embed), T1)
    }
    if(!is.null(T2)) {
      message(Glue("T2 ({labels$T2}):"))
      T2 = vocab_str_subset(rownames(embed), T2)
    }
    if(!is.null(A1)) {
      message(Glue("A1 ({labels$A1}):"))
      A1 = vocab_str_subset(rownames(embed), A1)
    }
    if(!is.null(A2)) {
      message(Glue("A2 ({labels$A2}):"))
      A2 = vocab_str_subset(rownames(embed), A2)
    }
  }
  words = c(T1, T2, A1, A2)
  embed = get_wordembed(embed, words)
  words.valid = rownames(embed)

  # words not found:
  not.found = setdiff(words, words.valid)
  # valid words:
  T1 = intersect(T1, words.valid)
  T2 = intersect(T2, words.valid)
  A1 = intersect(A1, words.valid)
  A2 = intersect(A2, words.valid)
  if(length(intersect(T1, T2)) > 0)
    warning("`T1` and `T2` have duplicate values!", call.=FALSE)
  if(length(intersect(A1, A2)) > 0)
    warning("`A1` and `A2` have duplicate values!", call.=FALSE)

  if(!is.null(T2)) {
    dweat = rbind(
      cbind(data.table(Target=labels$T1, Attrib=labels$A1),
            tab_similarity(embed, words1=T1, words2=A1)),
      cbind(data.table(Target=labels$T1, Attrib=labels$A2),
            tab_similarity(embed, words1=T1, words2=A2)),
      cbind(data.table(Target=labels$T2, Attrib=labels$A1),
            tab_similarity(embed, words1=T2, words2=A1)),
      cbind(data.table(Target=labels$T2, Attrib=labels$A2),
            tab_similarity(embed, words1=T2, words2=A2))
    )[,-5]  # delete "wordpair" column
    names(dweat)[3:4] = c("T_word", "A_word")
    dweat$Target = factor(dweat$Target, levels=c(labels$T1, labels$T2))
    dweat$Attrib = factor(dweat$Attrib, levels=c(labels$A1, labels$A2))
    dweat$T_word = factor(dweat$T_word, levels=unique(c(T1, T2)))
    dweat$A_word = factor(dweat$A_word, levels=unique(c(A1, A2)))
  } else {
    dweat = rbind(
      cbind(data.table(Target=labels$T1, Attrib=labels$A1),
            tab_similarity(embed, words1=T1, words2=A1)),
      cbind(data.table(Target=labels$T1, Attrib=labels$A2),
            tab_similarity(embed, words1=T1, words2=A2))
    )[,-5]  # delete "wordpair" column
    names(dweat)[3:4] = c("T_word", "A_word")
    dweat$Target = factor(dweat$Target, levels=c(labels$T1))
    dweat$Attrib = factor(dweat$Attrib, levels=c(labels$A1, labels$A2))
    dweat$T_word = factor(dweat$T_word, levels=unique(c(T1)))
    dweat$A_word = factor(dweat$A_word, levels=unique(c(A1, A2)))
  }

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
  rm(embed)
  # gc()  # Garbage Collection: Free the Memory
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

  embed = force_normalize(as_embed(data), verbose=FALSE)
  rm(data)
  if(use.pattern) {
    if(!is.null(T1)) {
      message(Glue("T1 ({labels$T1}):"))
      T1 = vocab_str_subset(rownames(embed), T1)
    }
    if(!is.null(A1)) {
      message(Glue("A1 ({labels$A1}):"))
      A1 = vocab_str_subset(rownames(embed), A1)
    }
    if(!is.null(A2)) {
      message(Glue("A2 ({labels$A2}):"))
      A2 = vocab_str_subset(rownames(embed), A2)
    }
  }
  words = c(T1, A1, A2)
  embed = get_wordembed(embed, words)
  words.valid = rownames(embed)

  # words not found:
  not.found = setdiff(words, words.valid)
  # valid words:
  T1 = intersect(T1, words.valid)
  A1 = intersect(A1, words.valid)
  A2 = intersect(A2, words.valid)
  if(length(intersect(A1, A2)) > 0)
    warning("`A1` and `A2` have duplicate values!", call.=FALSE)

  # v1 = rowMeans(dt[, A1, with=FALSE])  # average vector for A1
  # v2 = rowMeans(dt[, A2, with=FALSE])  # average vector for A2
  m1 = matrix(rep(colMeans(embed[A1,]), length(T1)),
              nrow=length(T1), byrow=TRUE)
  m2 = matrix(rep(colMeans(embed[A2,]), length(T1)),
              nrow=length(T1), byrow=TRUE)
  drnd = data.table(T_word = T1)
  drnd$norm_dist_A1 = sqrt(rowSums( (embed[T1,]-m1)^2 ))
  drnd$norm_dist_A2 = sqrt(rowSums( (embed[T1,]-m2)^2 ))
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
  rm(embed)
  # gc()  # Garbage Collection: Free the Memory
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


#### Orthogonal Procrustes ####


# adapted from cds::orthprocr()
# same as psych::Procrustes() and pracma::procrustes()


#' Orthogonal Procrustes rotation for matrix alignment.
#'
#' @description
#' In order to compare word embeddings from different time periods,
#' we must ensure that the embedding matrices are aligned to
#' the same semantic space (coordinate axes).
#' The Orthogonal Procrustes solution (Schönemann, 1966) is
#' commonly used to align historical embeddings over time
#' (Hamilton et al., 2016; Li et al., 2020).
#'
#' Note that this kind of rotation \emph{does not} change the
#' relative relationships between vectors in the space,
#' and thus \emph{does not} affect semantic similarities or distances
#' within each embedding matrix.
#' But it does influence the semantic relationships between
#' different embedding matrices, and thus would be necessary
#' for some purposes such as the "semantic drift analysis"
#' (e.g., Hamilton et al., 2016; Li et al., 2020).
#'
#' This function produces the same results as by
#' \code{cds::orthprocr()},
#' \code{psych::Procrustes()}, and
#' \code{pracma::procrustes()}.
#'
#' @param M,X Two embedding matrices of the same size (rows and columns),
#' can be \code{\link[PsychWordVec:as_embed]{embed}}
#' or \code{\link[PsychWordVec:as_wordvec]{wordvec}} objects.
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
#' # Usage 1: input two matrices (can be `embed` objects)
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
  rm(M, X, ints)
  # gc()  # Garbage Collection: Free the Memory
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
  rm(M, X, MX, svdMX, R)
  return(XR)
}


