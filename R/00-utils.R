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
#' demodata
#'
#' embed = as_embed(demodata, normalize=TRUE)
#' class(embed)
#' embed
#'
#' @name demodata
NULL


#### Utility Functions ####


#' @importFrom bruceR cc
#' @export
bruceR::cc


is.wordvec = function(x) inherits(x, "wordvec")
is.embed = function(x) inherits(x, "embed")
is.valid = function(x) inherits(x, c("wordvec", "embed"))
grey = cli::make_ansi_style("grey60")


check_data_validity = function(x) {
  if(!is.valid(x))
    stop("Input must be `wordvec` or `embed`!", call.=FALSE)
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


cn = function(n=1) cat(rep("\n", times=n))


## sweater - not as Caliskan's approach
pooled_sd = function(v, g1, g2) {
  n1 = length(g1)
  n2 = length(g2)
  var1 = stats::var(v[1:n1])
  var2 = stats::var(v[n1+(1:n2)])
  pooled.sd = sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
  return(pooled.sd)
}


## adapted from combinat::permn()
arrange = function(x) {
  n = length(x)
  out = vector("list", factorial(n))
  p = ip = seqn = seq(n)
  d = rep(-1, n)
  d[1] = 0
  m = n + 1
  p = c(m, p, m)
  i = 1
  use = -c(1, n + 2)
  while(m != 1) {
    out[[i]] = x[p[use]]
    i = i + 1
    m = n
    chk = (p[ip + d + 1] > seqn)
    m = max(seqn[!chk])
    if (m < n)
      d[(m + 1):n] = -d[(m + 1):n]
    index1 = ip[m] + 1
    index2 = p[index1] = p[index1 + d[m]]
    p[index1 + d[m]] = m
    tmp = ip[index2]
    ip[index2] = ip[m]
    ip[m] = tmp
  }
  out
}


## permutation (exact & resampling) tests
p_perm = function(v, ids=NULL, test.value=NULL, nsim, side) {
  pv = numeric()

  if(is.null(ids)) {
    ## One-sample permutation test (always resampling)
    type = "pval_approx"
    absv = abs(v)
    for(i in seq_len(nsim)) {
      signs = sample(c(-1, 1), length(v), replace=TRUE)
      pv[i] = mean(signs * absv)
    }
    if(is.null(test.value)) test.value = mean(v)
  } else {
    ## Two-samples permutation test
    if(factorial(length(v)) <= nsim) {
      # Exact test
      type = "pval_exact"
      perm = arrange(v)
      pv = purrr::map_dbl(perm, ~ mean(.[ids]) - mean(.[!ids]))
    } else {
      # Resampling test
      type = "pval_approx"
      for(i in seq_len(nsim)) {
        ids.i = sample(ids, replace=FALSE)
        pv[i] = mean(v[ids.i]) - mean(v[!ids.i])
      }
    }
  }

  if(side==1)
    p = sum(pv > test.value) / length(pv)
  else if(side==2)
    p = sum(abs(pv) > abs(test.value)) / length(pv)
  else
    stop("`side` should be 1 or 2.")
  names(p) = paste0(type, "_", side, "sided")
  return(p)
}


fixed_string = function(v) {
  v = as.character(v)
  sprintf(paste0("% ", max(nchar(v)), "s"), v)
}


valid_words_info = function(x) {
  ns = x$eff.label$words
  ls = paste0(unlist(x$eff.label$labels), " (",
              names(x$eff.label$labels), ")")
  nf = length(x$words.not.found)
  nf = ifelse(nf==0, "", paste0("\n(", nf, " words not found)"))
  info = paste0(paste(paste(ns, ls, "words"), collapse="\n"), nf)
  return(info)
}


number_duplicate = function(x, sep="_") {
  v = z = c()
  for(xi in x) {
    if(sum(xi==x) > 1) {
      vi = paste0(xi, sep, sum(xi==z) + 1)
    } else {
      vi = xi
    }
    v = c(v, vi)
    z = c(z, xi)
  }
  return(v)
}

