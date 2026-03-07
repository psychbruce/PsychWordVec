# Tokenize raw text for training word embeddings.

Tokenize raw text for training word embeddings.

## Usage

``` r
tokenize(
  text,
  tokenizer = text2vec::word_tokenizer,
  split = " ",
  remove = "_|'|<br/>|<br />|e\\.g\\.|i\\.e\\.",
  encoding = "UTF-8",
  simplify = TRUE,
  verbose = TRUE
)
```

## Arguments

- text:

  A character vector of text, or a file path on disk containing text.

- tokenizer:

  Function used to tokenize the text. Defaults to
  [`text2vec::word_tokenizer()`](https://rdrr.io/pkg/text2vec/man/tokenizers.html).

- split:

  Separator between tokens, only used when `simplify=TRUE`. Defaults to
  `" "`.

- remove:

  Strings (in regular expression) to be removed from the text. Defaults
  to `"_|'|<br/>|<br />|e\\\\.g\\\\.|i\\\\.e\\\\."`. You may turn off
  this by specifying `remove=NULL`.

- encoding:

  Text encoding (only used if `text` is a file). Defaults to `"UTF-8"`.

- simplify:

  Return a character vector (`TRUE`) or a list of character vectors
  (`FALSE`). Defaults to `TRUE`.

- verbose:

  Print information to the console? Defaults to `TRUE`.

## Value

- `simplify=TRUE`: A tokenized character vector, with each element as a
  sentence.

- `simplify=FALSE`: A list of tokenized character vectors, with each
  element as a vector of tokens in a sentence.

## See also

[`train_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/train_wordvec.md)

## Examples

``` r
txt1 = c(
  "I love natural language processing (NLP)!",
  "I've been in this city for 10 years. I really like here!",
  "However, my computer is not among the \"Top 10\" list."
)
tokenize(txt1, simplify=FALSE)
#> ✔ Tokenized: 4 sentences (time cost = 0.3 secs)
#> [[1]]
#> [1] "I"          "love"       "natural"    "language"   "processing"
#> [6] "NLP"       
#> 
#> [[2]]
#> [1] "Ive"   "been"  "in"    "this"  "city"  "for"   "10"    "years"
#> 
#> [[3]]
#> [1] "I"      "really" "like"   "here"  
#> 
#> [[4]]
#>  [1] "However"  "my"       "computer" "is"       "not"      "among"   
#>  [7] "the"      "Top"      "10"       "list"    
#> 
tokenize(txt1) %>% cat(sep="\n----\n")
#> ✔ Tokenized: 4 sentences (time cost = 0.2 secs)
#> I love natural language processing NLP
#> ----
#> Ive been in this city for 10 years
#> ----
#> I really like here
#> ----
#> However my computer is not among the Top 10 list

txt2 = text2vec::movie_review$review[1:5]
texts = tokenize(txt2)
#> ✔ Tokenized: 81 sentences (time cost = 0.2 secs)

txt2[1]
#> [1] "With all this stuff going down at the moment with MJ i've started listening to his music, watching the odd documentary here and there, watched The Wiz and watched Moonwalker again. Maybe i just want to get a certain insight into this guy who i thought was really cool in the eighties just to maybe make up my mind whether he is guilty or innocent. Moonwalker is part biography, part feature film which i remember going to see at the cinema when it was originally released. Some of it has subtle messages about MJ's feeling towards the press and also the obvious message of drugs are bad m'kay.<br /><br />Visually impressive but of course this is all about Michael Jackson so unless you remotely like MJ in anyway then you are going to hate this and find it boring. Some may call MJ an egotist for consenting to the making of this movie BUT MJ and most of his fans would say that he made it for the fans which if true is really nice of him.<br /><br />The actual feature film bit when it finally starts is only on for 20 minutes or so excluding the Smooth Criminal sequence and Joe Pesci is convincing as a psychopathic all powerful drug lord. Why he wants MJ dead so bad is beyond me. Because MJ overheard his plans? Nah, Joe Pesci's character ranted that he wanted people to know it is he who is supplying drugs etc so i dunno, maybe he just hates MJ's music.<br /><br />Lots of cool things in this like MJ turning into a car and a robot and the whole Speed Demon sequence. Also, the director must have had the patience of a saint when it came to filming the kiddy Bad sequence as usually directors hate working with one kid let alone a whole bunch of them performing a complex dance scene.<br /><br />Bottom line, this movie is for people who like MJ on one level or another (which i think is most people). If not, then stay away. It does try and give off a wholesome message and ironically MJ's bestest buddy in this movie is a girl! Michael Jackson is truly one of the most talented people ever to grace this planet but is he guilty? Well, with all the attention i've gave this subject....hmmm well i don't know because people can be different behind closed doors, i know this for a fact. He is either an extremely nice but stupid guy or one of the most sickest liars. I hope he is not the latter."
texts[1:20]  # all sentences in txt2[1]
#>  [1] "With all this stuff going down at the moment with MJ ive started listening to his music watching the odd documentary here and there watched The Wiz and watched Moonwalker again"                                      
#>  [2] "Maybe i just want to get a certain insight into this guy who i thought was really cool in the eighties just to maybe make up my mind whether he is guilty or innocent"                                                 
#>  [3] "Moonwalker is part biography part feature film which i remember going to see at the cinema when it was originally released"                                                                                            
#>  [4] "Some of it has subtle messages about MJs feeling towards the press and also the obvious message of drugs are bad mkay"                                                                                                 
#>  [5] "Visually impressive but of course this is all about Michael Jackson so unless you remotely like MJ in anyway then you are going to hate this and find it boring"                                                       
#>  [6] "Some may call MJ an egotist for consenting to the making of this movie BUT MJ and most of his fans would say that he made it for the fans which if true is really nice of him"                                         
#>  [7] "The actual feature film bit when it finally starts is only on for 20 minutes or so excluding the Smooth Criminal sequence and Joe Pesci is convincing as a psychopathic all powerful drug lord"                        
#>  [8] "Why he wants MJ dead so bad is beyond me"                                                                                                                                                                              
#>  [9] "Because MJ overheard his plans"                                                                                                                                                                                        
#> [10] "Nah Joe Pescis character ranted that he wanted people to know it is he who is supplying drugs etc so i dunno maybe he just hates MJs music"                                                                            
#> [11] "Lots of cool things in this like MJ turning into a car and a robot and the whole Speed Demon sequence"                                                                                                                 
#> [12] "Also the director must have had the patience of a saint when it came to filming the kiddy Bad sequence as usually directors hate working with one kid let alone a whole bunch of them performing a complex dance scene"
#> [13] "Bottom line this movie is for people who like MJ on one level or another which i think is most people"                                                                                                                 
#> [14] "If not then stay away"                                                                                                                                                                                                 
#> [15] "It does try and give off a wholesome message and ironically MJs bestest buddy in this movie is a girl"                                                                                                                 
#> [16] "Michael Jackson is truly one of the most talented people ever to grace this planet but is he guilty"                                                                                                                   
#> [17] "Well with all the attention ive gave this subject"                                                                                                                                                                     
#> [18] "hmmm well i dont know because people can be different behind closed doors i know this for a fact"                                                                                                                      
#> [19] "He is either an extremely nice but stupid guy or one of the most sickest liars"                                                                                                                                        
#> [20] "I hope he is not the latter"                                                                                                                                                                                           
```
