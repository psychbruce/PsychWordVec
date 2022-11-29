#### Setup Python Environment ####


#' Set up a Python environment for contextualized (dynamic) word embeddings,
#' necessary for all \code{text_*} functions.
#'
#' Install required Python packages in a conda environment (with defaults)
#' and initialize the installed conda environment.
#' Run once before your first use of any \code{text_*} function.
#' No need to run this in the next time.
#' Users may first need to manually install
#' \href{https://www.anaconda.com/}{Anaconda} or
#' \href{https://docs.conda.io/en/main/miniconda.html}{Miniconda}.
#'
#' The R package \code{text} (\url{https://www.r-text.org/}) enables users access to
#' \href{https://huggingface.co/models}{HuggingFace Transformers models} in R,
#' through the R package \code{reticulate} as an interface to Python
#' and the Python packages \code{torch} and \code{transformers}.
#' This function calls two functions in the \code{text} package.
#'
#' Users who have problems in running this function should directly
#' run the functions in \code{text}:
#' \itemize{
#'   \item{\code{\link[text:textrpp_install]{text::textrpp_install()}}}
#'   \item{\code{\link[text:textrpp_install]{text::textrpp_install_virtualenv()}}}
#'   \item{\code{\link[text:textrpp_uninstall]{text::textrpp_uninstall()}}}
#'   \item{\code{\link[text:textrpp_initialize]{text::textrpp_initialize()}}}
#' }
#'
#' @param install Run \code{\link[text:textrpp_install]{text::textrpp_install()}}.
#' Defaults to \code{TRUE}.
#' @param initialize Run \code{\link[text:textrpp_initialize]{text::textrpp_initialize()}}.
#' Defaults to \code{TRUE}.
#' @param save Save settings to .Rprofile for future use,
#' so that you do not have to run this again after restarting R.
#' Defaults to \code{TRUE}.
#' @param ... Other parameters passed to
#' \code{\link[text:textrpp_install]{text::textrpp_install()}}.
#'
#' @seealso
#' \code{\link{text_model_download}}
#'
#' \code{\link{text_model_remove}}
#'
#' @examples
#' \dontrun{
#' text_env_setup()
#' }
#'
#' @export
text_env_setup = function(
    install=TRUE,
    initialize=TRUE,
    save=TRUE,
    ...
) {
  if(install) {
    cli::cli_h1("Running text::textrpp_install()")
    try({ text::textrpp_install(...) })
  }
  if(initialize) {
    cli::cli_h1("Running text::textrpp_initialize()")
    text::textrpp_initialize(save_profile=save)
  }
}


#' Download pre-trained language models from HuggingFace.
#'
#' Download pre-trained language models (Transformers Models,
#' such as GPT, BERT, RoBERTa, DeBERTa, DistilBERT, XLM, etc.)
#' from \href{https://huggingface.co/models}{HuggingFace} to
#' your local ".cache" folder ("C:/Users/[UserName]/.cache/").
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
#'   \item{\code{"gpt2"}}
#'   \item{\code{"openai-gpt"}}
#'   \item{\code{"bert-base-uncased"}}
#'   \item{\code{"bert-large-uncased"}}
#'   \item{\code{"bert-base-cased"}}
#'   \item{\code{"bert-large-cased"}}
#'   \item{\code{"bert-base-chinese"}}
#'   \item{\code{"bert-base-multilingual-cased"}}
#'   \item{\code{"albert-base-v2"}}
#'   \item{\code{"albert-large-v2"}}
#'   \item{\code{"roberta-base"}}
#'   \item{\code{"roberta-large"}}
#'   \item{\code{"distilbert-base-uncased"}}
#'   \item{\code{"distilbert-base-cased"}}
#'   \item{\code{"distilbert-base-multilingual-cased"}}
#'   \item{\code{"xlm-roberta-base"}}
#'   \item{\code{"xlm-roberta-large"}}
#'   \item{\code{"xlnet-base-cased"}}
#'   \item{\code{"xlnet-large-cased"}}
#'   \item{\code{...} (see \url{https://huggingface.co/models})}
#' }
#'
#' @return
#' Invisibly return the names of all downloaded models.
#'
#' @seealso
#' \code{\link{text_env_setup}}
#'
#' \code{\link{text_model_remove}}
#'
#' @examples
#' \dontrun{
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
  if(!is.null(model)) {
    for(m in model) {
      cli::cli_h1("Downloading model \"{m}\"")
      try({ text::textEmbedRawLayers(" ", model=m) }, silent=TRUE)
      cli::cli_alert_success("Successfully downloaded model \"{m}\"")
    }
  }
  cli::cli_h2("Currently downloaded models:")
  models = text::textModels()
  models[[1]] = sort(models[[1]])
  models[[2]] = sort(models[[2]])
  cli::cli_li(paste0("\"", models$Downloaded_models, "\""))
  invisible(models)
}


#' Remove downloaded models from .cache folder.
#'
#' @param model Model name. See \code{\link{text_model_download}}.
#' Defaults to automatically find all downloaded models in the .cache folder.
#'
#' @seealso
#' \code{\link{text_env_setup}}
#'
#' \code{\link{text_model_download}}
#'
#' @examples
#' \dontrun{
#' text_model_remove()
#' }
#'
#' @export
text_model_remove = function(model=NULL) {
  if(is.null(model))
    models = sort(text::textModels()$Downloaded_models)
  if("NoModelsAvailable" %in% models) {
    cli::cli_alert_warning("No models available. Download using `text_model_download()`.")
  } else {
    yesno = utils::menu(
      c("Yes", "No"),
      title=paste("\nDo you want to delete these models?\n",
                  paste(paste0("\"", models, "\""),
                        collapse="\n ")))
    if(yesno==1) for(m in models) text::textModelsRemove(m)
  }
}


#' Extract contextualized word embeddings from transformers (pre-trained language models).
#'
#' Extract model layers and aggregate them to
#' token (roughly word) embeddings and text embeddings
#' (all reshaped to \code{\link[PsychWordVec:as_wordvec]{wordvec}} data tables).
#' A wrapper function of \code{\link[text:textEmbed]{text::textEmbed()}}.
#'
#' @param text Can be:
#' \itemize{
#'   \item{a character string or vector of text (usually sentences)}
#'   \item{a file path on disk containing text}
#'   \item{a data frame with at least one character variable
#'   (for text from all character variables in a given data frame)}
#' }
#' @param model Model name at \href{https://huggingface.co/models}{HuggingFace}.
#' See \code{\link{text_model_download}}.
#' If the model has not been downloaded, it would automatically download the model.
#' @param layers Layers to be extracted from the \code{model},
#' which are then aggregated in the function
#' \code{\link[text:textEmbedLayerAggregation]{text::textEmbedLayerAggregation()}}.
#' Defaults to \code{-2} which extracts the second to last layers.
#' You may extract only the layers you need (e.g., \code{11:12}) or
#' all layers (\code{"all"}).
#' Note that layer 0 is the \emph{decontextualized} input layer
#' (i.e., not comprising hidden states) and is normally not used.
#'
#' @return
#' A \code{list} of:
#' \describe{
#'   \item{\code{word.embed}}{
#'     Token (roughly word) embeddings}
#'   \item{\code{text.embed}}{
#'     Text embeddings, aggregated from token embeddings}
#' }
#'
#' @examples
#' \dontrun{
#' embed = text_to_vec(
#'   c("I love China.", "Beijing is the capital of China."),
#'   model="bert-base-cased"
#' )
#' embed$word.embed
#' embed$text.embed
#'
#' embed1 = embed$word.embed[[1]]
#' embed2 = embed$word.embed[[2]]
#'
#' View(embed1)
#' View(embed2)
#' View(embed$text.embed)
#' }
#'
#' @export
text_to_vec = function(
    text,
    model,
    layers=-2
) {
  embed = text::textEmbed(
    texts=text,
    model=model,
    layers=layers)
  {
    token.embed = lapply(
      embed[["tokens"]][["texts"]],
      function(data) {
        mat = as.matrix(data[-1])
        data = data.table(
          word = data$tokens,
          vec = do.call("list", lapply(
            1:nrow(mat), function(i) {
              as.numeric(mat[i,])
            }))
        )
        attr(data, "dims") = ncol(mat)
        attr(data, "normalized") = FALSE
        class(data) = c("wordvec", "data.table", "data.frame")
        return(data)
      })
  }
  {
    tmat = as.matrix(embed[["texts"]][["texts"]])
    text.embed = data.table(
      text = sapply(
        embed[["tokens"]][["texts"]],
        function(data) {
          paste(data$tokens, collapse=" ")
        }),
      vec = do.call("list", lapply(
        1:nrow(tmat), function(i) {
          as.numeric(tmat[i,])
        }))
    )
    attr(text.embed, "dims") = ncol(tmat)
    attr(text.embed, "normalized") = FALSE
    class(text.embed) = c("wordvec", "data.table", "data.frame")
  }

  embeds = list(word.embed=token.embed,
                text.embed=text.embed)

  return(embeds)
}

