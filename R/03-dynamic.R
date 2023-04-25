#### Setup Python Environment ####


#' Install required Python modules
#' in a new conda environment
#' and initialize the environment,
#' necessary for all \code{text_*} functions
#' designed for contextualized word embeddings.
#'
#' @details
#' Users may first need to manually install
#' \href{https://www.anaconda.com/}{Anaconda} or
#' \href{https://docs.conda.io/en/main/miniconda.html}{Miniconda}.
#'
#' The R package \code{text} (\url{https://www.r-text.org/}) enables users access to
#' \href{https://huggingface.co/models}{HuggingFace Transformers models} in R,
#' through the R package \code{reticulate} as an interface to Python
#' and the Python modules \code{torch} and \code{transformers}.
#'
#' For advanced usage, see
#' \itemize{
#'   \item{\code{\link[text:textrpp_install]{text::textrpp_install()}}}
#'   \item{\code{\link[text:textrpp_install]{text::textrpp_install_virtualenv()}}}
#'   \item{\code{\link[text:textrpp_uninstall]{text::textrpp_uninstall()}}}
#'   \item{\code{\link[text:textrpp_initialize]{text::textrpp_initialize()}}}
#' }
#'
#' @seealso
#' \code{\link{text_model_download}}
#'
#' \code{\link{text_model_remove}}
#'
#' \code{\link{text_to_vec}}
#'
#' \code{\link{text_unmask}}
#'
#' @examples
#' \dontrun{
#' text_init()
#'
#' # You may need to specify the version of Python:
#' # RStudio -> Tools -> Global/Project Options
#' # -> Python -> Select -> Conda Environments
#' # -> Choose ".../textrpp_condaenv/python.exe"
#' }
#'
#' @export
text_init = function() {
  suppressMessages({
    suppressWarnings({
      text::textrpp_install(prompt=FALSE)
    })
  })
  cn()
  cli::cli_alert_success("{.pkg Successfully installed Python modules in conda environment.}")

  error = TRUE
  try({
    suppressMessages({
      suppressWarnings({
        text::textrpp_initialize(save_profile=TRUE, prompt=FALSE)
      })
    })
    error = FALSE
  }, silent=TRUE)
  if(error)
    stop("No valid Python or conda environment.

       You may need to specify the version of Python:
         RStudio -> Tools -> Global/Project Options
         -> Python -> Select -> Conda Environments
         -> Choose \".../textrpp_condaenv/python.exe\"",
       call.=FALSE)
  cn()
  cli::cli_alert_success("{.pkg Initialized the Python modules.}")
}


text_initialized = function() {
  error = TRUE
  try({
    text::textModels()
    error = FALSE
  }, silent=TRUE)
  if(error) text_init()
}


#### Transformers Models Management ####


#' Download pre-trained language models from HuggingFace.
#'
#' Download pre-trained language models (Transformers Models,
#' such as GPT, BERT, RoBERTa, DeBERTa, DistilBERT, etc.)
#' from \href{https://huggingface.co/models}{HuggingFace} to
#' your local ".cache" folder ("C:/Users/[YourUserName]/.cache/").
#' The models will never be removed unless you run
#' \code{\link{text_model_remove}}.
#'
#' @param model Character string(s) specifying the
#' pre-trained language model(s) to be downloaded.
#' For a full list of options, see
#' \href{https://huggingface.co/models}{HuggingFace}.
#' Defaults to download nothing and check currently downloaded models.
#'
#' Example choices:
#' \itemize{
#'   \item{\code{"gpt2"} (50257 vocab, 768 dims, 12 layers)}
#'   \item{\code{"openai-gpt"} (40478 vocab, 768 dims, 12 layers)}
#'   \item{\code{"bert-base-uncased"} (30522 vocab, 768 dims, 12 layers)}
#'   \item{\code{"bert-large-uncased"} (30522 vocab, 1024 dims, 24 layers)}
#'   \item{\code{"bert-base-cased"} (28996 vocab, 768 dims, 12 layers)}
#'   \item{\code{"bert-large-cased"} (28996 vocab, 1024 dims, 24 layers)}
#'   \item{\code{"bert-base-chinese"} (21128 vocab, 768 dims, 12 layers)}
#'   \item{\code{"bert-base-multilingual-cased"} (119547 vocab, 768 dims, 12 layers)}
#'   \item{\code{"distilbert-base-uncased"} (30522 vocab, 768 dims, 6 layers)}
#'   \item{\code{"distilbert-base-cased"} (28996 vocab, 768 dims, 6 layers)}
#'   \item{\code{"distilbert-base-multilingual-cased"} (119547 vocab, 768 dims, 6 layers)}
#'   \item{\code{"albert-base-v2"} (30000 vocab, 768 dims, 12 layers)}
#'   \item{\code{"albert-large-v2"} (30000 vocab, 1024 dims, 24 layers)}
#'   \item{\code{"roberta-base"} (50265 vocab, 768 dims, 12 layers)}
#'   \item{\code{"roberta-large"} (50265 vocab, 1024 dims, 24 layers)}
#'   \item{\code{"xlm-roberta-base"} (250002 vocab, 768 dims, 12 layers)}
#'   \item{\code{"xlm-roberta-large"} (250002 vocab, 1024 dims, 24 layers)}
#'   \item{\code{"xlnet-base-cased"} (32000 vocab, 768 dims, 12 layers)}
#'   \item{\code{"xlnet-large-cased"} (32000 vocab, 1024 dims, 24 layers)}
#'   \item{\code{"microsoft/deberta-v3-base"} (128100 vocab, 768 dims, 12 layers)}
#'   \item{\code{"microsoft/deberta-v3-large"} (128100 vocab, 1024 dims, 24 layers)}
#'   \item{\code{...} (see \url{https://huggingface.co/models})}
#' }
## @param save.config Save configuration file of the model to the current path.
## Defaults to \code{TRUE}.
#'
#' @return
#' Invisibly return the names of all downloaded models.
#'
#' @seealso
#' \code{\link{text_init}}
#'
#' \code{\link{text_model_remove}}
#'
#' \code{\link{text_to_vec}}
#'
#' \code{\link{text_unmask}}
#'
#' @examples
#' \dontrun{
#' # text_init()  # initialize the environment
#'
#' text_model_download()  # check downloaded models
#' text_model_download(c(
#'   "bert-base-uncased",
#'   "bert-base-cased",
#'   "bert-base-multilingual-cased"
#' ))
#' }
#'
#' @export
text_model_download = function(model=NULL) {
  text_initialized()
  if(!is.null(model)) {
    for(m in model) {
      cli::cli_h1("Downloading model \"{m}\"")
      transformers = reticulate::import("transformers")
      cli::cli_text("Downloading configuration...")
      config = transformers$AutoConfig$from_pretrained(m)
      cli::cli_text("Downloading tokenizer...")
      tokenizer = transformers$AutoTokenizer$from_pretrained(m)
      cli::cli_text("Downloading model...")
      model = transformers$AutoModel$from_pretrained(m)
      cli::cli_alert_success("Successfully downloaded model \"{m}\"")
      gc()
    }
  }
  cli::cli_h2("Currently downloaded language models:")
  models = text::textModels()
  models[[1]] = sort(models[[1]])
  models[[2]] = sort(models[[2]])
  cli::cli_li(paste0("\"", models$Downloaded_models, "\""))
  cn()
  cli::cli_text("For raw files, see local folder {.pkg C:/Users/[YourUserName]/.cache/}")
  cli::cli_text("For a full list of options, see {.url https://huggingface.co/models}")
  invisible(models)
}


#' Remove downloaded models from the local .cache folder.
#'
#' @param model Model name. See \code{\link{text_model_download}}.
#' Defaults to automatically find all downloaded models in the .cache folder.
#'
#' @seealso
#' \code{\link{text_init}}
#'
#' \code{\link{text_model_download}}
#'
#' \code{\link{text_to_vec}}
#'
#' \code{\link{text_unmask}}
#'
#' @examples
#' \dontrun{
#' # text_init()  # initialize the environment
#'
#' text_model_remove()
#' }
#'
#' @export
text_model_remove = function(model=NULL) {
  text_initialized()
  if(is.null(model))
    model = sort(text::textModels()$Downloaded_models)
  if("NoModelsAvailable" %in% model) {
    cli::cli_alert_warning("No models available. Download using `text_model_download()`.")
  } else {
    yesno = menu(
      c("Yes", "No"),
      title=paste("\nDo you want to delete these models?\n",
                  paste(paste0("\"", model, "\""),
                        collapse="\n ")))
    if(yesno==1) for(m in model) text::textModelsRemove(m)
  }
}


#### Contextualized Word Embeddings and Language Tasks ####


#' Extract contextualized word embeddings from transformers (pre-trained language models).
#'
#' Extract hidden layers from a language model and aggregate them to
#' get token (roughly word) embeddings and text embeddings
#' (all reshaped to \code{\link[PsychWordVec:as_embed]{embed}} matrix).
#' It is a wrapper function of \code{\link[text:textEmbed]{text::textEmbed()}}.
#'
#' @param text Can be:
#' \itemize{
#'   \item{a character string or vector of text (usually sentences)}
#'   \item{a data frame with at least one character variable
#'   (for text from all character variables in a given data frame)}
#'   \item{a file path on disk containing text}
#' }
#' @param model Model name at \href{https://huggingface.co/models}{HuggingFace}.
#' See \code{\link{text_model_download}}.
#' If the model has not been downloaded, it would automatically download the model.
#' @param layers Layers to be extracted from the \code{model},
#' which are then aggregated in the function
#' \code{\link[text:textEmbedLayerAggregation]{text::textEmbedLayerAggregation()}}.
#' Defaults to \code{"all"} which extracts all layers.
#' You may extract only the layers you need (e.g., \code{11:12}).
#' Note that layer 0 is the \emph{decontextualized} input layer
#' (i.e., not comprising hidden states).
#' @param encoding Text encoding (only used if \code{text} is a file).
#' Defaults to \code{"UTF-8"}.
#' @param layer.to.token Method to aggregate hidden layers to each token.
#' Defaults to \code{"concatenate"},
#' which links together each word embedding layer to one long row.
#' Options include \code{"mean"}, \code{"min"}, \code{"max"}, and \code{"concatenate"}.
#' @param token.to.word Aggregate subword token embeddings (if whole word is out of vocabulary)
#' to whole word embeddings. Defaults to \code{TRUE}, which sums up subword token embeddings.
#' @param token.to.text Aggregate token embeddings to each text.
#' Defaults to \code{TRUE}, which averages all token embeddings.
#' If \code{FALSE}, the text embedding will be the token embedding of \code{[CLS]}
#' (the special token that is used to represent the beginning of a text sequence).
#' @param ... Other parameters passed to
#' \code{\link[text:textEmbed]{text::textEmbed()}}.
#'
#' @return
#' A \code{list} of:
#' \describe{
#'   \item{\code{token.embed}}{
#'     Token (roughly word) embeddings}
#'   \item{\code{text.embed}}{
#'     Text embeddings, aggregated from token embeddings}
#' }
#'
#' @seealso
#' \code{\link{text_init}}
#'
#' \code{\link{text_model_download}}
#'
#' \code{\link{text_model_remove}}
#'
#' \code{\link{text_unmask}}
#'
#' @examples
#' \dontrun{
#' # text_init()  # initialize the environment
#'
#' text = c("Download models from HuggingFace",
#'          "Chinese are East Asian",
#'          "Beijing is the capital of China")
#' embed = text_to_vec(text, model="bert-base-cased", layers=c(0, 12))
#' embed
#'
#' embed1 = embed$token.embed[[1]]
#' embed2 = embed$token.embed[[2]]
#' embed3 = embed$token.embed[[3]]
#'
#' View(embed1)
#' View(embed2)
#' View(embed3)
#' View(embed$text.embed)
#'
#' plot_similarity(embed1, value.color="grey")
#' plot_similarity(embed2, value.color="grey")
#' plot_similarity(embed3, value.color="grey")
#' plot_similarity(rbind(embed1, embed2, embed3))
#' }
#'
#' @export
text_to_vec = function(
    text,
    model,
    layers="all",
    layer.to.token="concatenate",
    token.to.word=TRUE,
    token.to.text=TRUE,
    encoding="UTF-8",
    ...
) {
  # Force initialization
  text_initialized()

  ## Import text if necesssary
  t0 = Sys.time()
  if(length(text) == 1) {
    if(file.exists(text)) {
      Print("Reading text from file...")
      text = readLines(text, encoding=encoding, warn=FALSE)
      cli::cli_alert_success("Raw text corpus has been loaded (time cost = {dtime(t0, 'auto')})")
    }
  }

  ## Extract token and text embeddings
  embed = text::textEmbed(
    texts=text,
    model=model,
    layers=layers,
    keep_token_embeddings=TRUE,
    aggregation_from_layers_to_tokens=layer.to.token,
    aggregation_from_tokens_to_texts="mean",
    ...)

  token.embed = lapply(embed[["tokens"]][["texts"]], as_embed)
  if(token.to.word)
    token.embed = lapply(token.embed, token_to_word)
  if(token.to.text)
    text.embed = as_embed(embed[["texts"]][["texts"]])
  else
    text.embed = do.call(
      "rbind",
      lapply(embed[["tokens"]][["texts"]], function(df) {
        as_embed(df[1,])
      }))
  rownames(text.embed) = sapply(
    embed[["tokens"]][["texts"]],
    function(data) { paste(data$tokens, collapse=" ") })

  embeds = list(token.embed=token.embed,
                text.embed=text.embed)

  return(embeds)
}


token_to_word = function(embed) {
  if(any(str_detect(rownames(embed), "##"))) {
    for(i in nrow(embed):2) {
      if(str_detect(rownames(embed)[i], "^##")) {
        embed[i-1,] = embed[i-1,] + embed[i,]
        rownames(embed)[i-1] = paste0(
          rownames(embed)[i-1],
          str_remove(rownames(embed)[i], "##"))
      }
    }
    embed = as_embed(embed[str_detect(rownames(embed), "^##", negate=TRUE),])
  }
  return(embed)
}


#' Fill in the blank mask(s) in a query (sentence).
#'
#' Predict the probably correct masked token(s) in a sequence,
#' based on the Python module \code{transformers}.
#'
#' Masked language modeling is the task of masking some of the words in a sentence
#' and predicting which words should replace those masks.
#' These models are useful when we want to get a statistical understanding of
#' the language in which the model is trained in.
#' See \url{https://huggingface.co/tasks/fill-mask} for details.
#'
#' @inheritParams text_to_vec
#' @param query A query (sentence/prompt) with masked token(s) \code{[MASK]}.
#' Multiple queries are also supported. See examples.
#' @param targets Specific target word(s) to be filled in the blank \code{[MASK]}.
#' Defaults to \code{NULL} (i.e., return \code{topn}).
#' If specified, then \code{topn} will be ignored (see examples).
#' @param topn Number of the most likely predictions to return.
#' Defaults to \code{5}. If \code{targets} is specified,
#' then it will automatically change to the length of \code{targets}.
#'
#' @return
#' A \code{data.table} of query results:
#' \describe{
#'   \item{\code{query_id} (if there are more than one \code{query})}{
#'     \code{query} ID (indicating multiple queries)}
#'   \item{\code{mask_id} (if there are more than one \code{[MASK]} in \code{query})}{
#'     \code{[MASK]} ID (position in sequence, indicating multiple masks)}
#'   \item{\code{prob}}{
#'     Probability of the predicted token in the sequence}
#'   \item{\code{token_id}}{
#'     Predicted token ID (to replace \code{[MASK]})}
#'   \item{\code{token}}{
#'     Predicted token (to replace \code{[MASK]})}
#'   \item{\code{sequence}}{
#'     Complete sentence with the predicted token}
#' }
#'
#' @seealso
#' \code{\link{text_init}}
#'
#' \code{\link{text_model_download}}
#'
#' \code{\link{text_model_remove}}
#'
#' \code{\link{text_to_vec}}
#'
#' @examples
#' \dontrun{
#' # text_init()  # initialize the environment
#'
#' model = "distilbert-base-cased"
#'
#' text_unmask("Beijing is the [MASK] of China.", model)
#'
#' # multiple [MASK]s:
#' text_unmask("Beijing is the [MASK] [MASK] of China.", model)
#'
#' # multiple queries:
#' text_unmask(c("The man worked as a [MASK].",
#'               "The woman worked as a [MASK]."),
#'             model)
#'
#' # specific targets:
#' text_unmask("The [MASK] worked as a nurse.", model,
#'             targets=c("man", "woman"))
#' }
#'
#' @export
text_unmask = function(query, model, targets=NULL, topn=5) {
  text_initialized()

  if(!is.null(targets)) {
    targets = as.character(unique(targets))
    targets = factor(targets, levels=targets)
    topn = length(targets)
  }
  topn = as.integer(topn)
  query = str_replace_all(query, "\\[mask\\]", "[MASK]")
  if(str_detect(model, "roberta-base|roberta-large"))
    query = str_replace_all(query, "\\[MASK\\]", "<mask>")
  nmask = unique(str_count(query, "\\[MASK\\]|<mask>"))
  nquery = length(query)
  if(length(nmask) > 1)
    stop("All queries should have the same number of [MASK].", call.=FALSE)

  transformers = reticulate::import("transformers")
  reticulate::py_capture_output({
    fill_mask = transformers$pipeline("fill-mask", model=model)
  })

  if(is.null(targets)) {
    res = do.call(c, lapply(query, function(q) fill_mask(q, top_k=topn)))
  } else {
    res = do.call(c, lapply(query, function(q) {
      do.call(c, lapply(targets, function(target) {
        fill_mask(q, targets=target, top_k=1L)
      }))
    }))
  }

  if(nmask > 1) res = unlist(res, recursive=FALSE)
  qm_id = data.table(
    query_id = rep(1:nquery, each=topn*nmask),
    mask_id = if(is.null(targets)) rep(1:nmask, each=topn) else rep(1:nmask, topn))
  res = cbind(qm_id, rbindlist(res))
  names(res) = c("query_id", "mask_id", "prob", "token_id", "token", "sequence")
  if(!is.null(targets)) {
    res$token = factor(str_remove(res$token, "\u2581"),
                       levels=str_remove(as.character(targets), "\u2581"))
    res = res[order(res$token)]
  }
  if(length(unique(res$query_id)) == 1) res$query_id = NULL
  if(length(unique(res$mask_id)) == 1) res$mask_id = NULL
  return(res)
}

