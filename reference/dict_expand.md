# Expand a dictionary from the most similar words.

Expand a dictionary from the most similar words.

## Usage

``` r
dict_expand(data, words, threshold = 0.5, iteration = 5, verbose = TRUE)
```

## Arguments

- data:

  A
  [`wordvec`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (data.table) or
  [`embed`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (matrix), see
  [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md).

- words:

  A single word or a list of words, used to calculate the [sum
  vector](https://psychbruce.github.io/PsychWordVec/reference/sum_wordvec.md).

- threshold:

  Threshold of cosine similarity, used to find all words with
  similarities higher than this value. Defaults to `0.5`. A low
  threshold may lead to failure of convergence.

- iteration:

  Number of maximum iterations. Defaults to `5`.

- verbose:

  Print information to the console? Defaults to `TRUE`.

## Value

An expanded list (character vector) of words.

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## See also

[`sum_wordvec()`](https://psychbruce.github.io/PsychWordVec/reference/sum_wordvec.md)

[`most_similar()`](https://psychbruce.github.io/PsychWordVec/reference/most_similar.md)

[`dict_reliability()`](https://psychbruce.github.io/PsychWordVec/reference/dict_reliability.md)

## Examples

``` r
dict = dict_expand(demodata, "king")
#> ! Results may be inaccurate if word vectors are not normalized.
#> ✔ All word vectors now have been automatically normalized.
#> 
#> ── Iteration 1 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 3 more words appended: "queen", "royal", and "King"
#> 
#> ── Iteration 2 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 2 more words appended: "Queen" and "Prince"
#> 
#> ── Iteration 3 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ No more word appended. Successfully convergent.
#> 
#> ── Finish (convergent) ──
#> 
dict
#> [1] "king"   "queen"  "royal"  "King"   "Queen"  "Prince"

dict = dict_expand(demodata, cc("king, queen"))
#> ! Results may be inaccurate if word vectors are not normalized.
#> ✔ All word vectors now have been automatically normalized.
#> 
#> ── Iteration 1 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 2 more words appended: "royal" and "Queen"
#> 
#> ── Iteration 2 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 1 more word appended: "King"
#> 
#> ── Iteration 3 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 1 more word appended: "Prince"
#> 
#> ── Iteration 4 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ No more word appended. Successfully convergent.
#> 
#> ── Finish (convergent) ──
#> 
dict
#> [1] "king"   "queen"  "royal"  "Queen"  "King"   "Prince"

most_similar(demodata, dict)
#> ! Results may be inaccurate if word vectors are not normalized.
#> ✔ All word vectors now have been automatically normalized.
#> [Word Vector] =~ king + queen + royal + Queen + King + Prince
#> (normalized to unit length)
#>          word   cos_sim row_id
#>        <char>     <num>  <int>
#>  1:   Jackson 0.3615837   1066
#>  2:    Albert 0.3607588   4068
#>  3:     Crown 0.3587386   5054
#>  4:     crown 0.3399563   5174
#>  5: superstar 0.3375027   5428
#>  6:   royalty 0.3353748   6388
#>  7:     Kings 0.3330198   6472
#>  8:    legend 0.3305279   6775
#>  9:       Sir 0.3121715   7218
#> 10:      pope 0.3093004   7893

dict.cn = dict_expand(demodata, "China")
#> ! Results may be inaccurate if word vectors are not normalized.
#> ✔ All word vectors now have been automatically normalized.
#> 
#> ── Iteration 1 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 19 more words appended: "Chinese", "Beijing", "Taiwan", "Shanghai", "Shenzhen", "Guangzhou", "yuan", "India", "Japan", "Li", "Asia", "Korea", "Taiwanese", "mainland", "Russia", "Tibet", "Wang", "Chen", and "Vietnam"
#> 
#> ── Iteration 2 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 14 more words appended: "Seoul", "HK", "Singapore", "Korean", "Thailand", "Japanese", "Pyongyang", "Vietnamese", "Asian", "Tokyo", "Malaysia", "Xinhua", "Thai", and "Malaysian"
#> 
#> ── Iteration 3 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 5 more words appended: "Indonesian", "Indonesia", "Bangkok", "Philippine", and "Philippines"
#> 
#> ── Iteration 4 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ 1 more word appended: "Nepal"
#> 
#> ── Iteration 5 (threshold of cosine similarity = 0.5) ──────────────────────────
#> ✔ No more word appended. Successfully convergent.
#> 
#> ── Finish (convergent) ──
#> 
dict.cn  # too inclusive if setting threshold = 0.5
#>  [1] "China"       "Chinese"     "Beijing"     "Taiwan"      "Shanghai"   
#>  [6] "Shenzhen"    "Guangzhou"   "yuan"        "India"       "Japan"      
#> [11] "Li"          "Asia"        "Korea"       "Taiwanese"   "mainland"   
#> [16] "Russia"      "Tibet"       "Wang"        "Chen"        "Vietnam"    
#> [21] "Seoul"       "HK"          "Singapore"   "Korean"      "Thailand"   
#> [26] "Japanese"    "Pyongyang"   "Vietnamese"  "Asian"       "Tokyo"      
#> [31] "Malaysia"    "Xinhua"      "Thai"        "Malaysian"   "Indonesian" 
#> [36] "Indonesia"   "Bangkok"     "Philippine"  "Philippines" "Nepal"      

dict.cn = dict_expand(demodata,
                      cc("China, Chinese"),
                      threshold=0.6)
#> ! Results may be inaccurate if word vectors are not normalized.
#> ✔ All word vectors now have been automatically normalized.
#> 
#> ── Iteration 1 (threshold of cosine similarity = 0.6) ──────────────────────────
#> ✔ 8 more words appended: "Beijing", "Taiwan", "Taiwanese", "Shanghai", "Li", "Guangzhou", "Shenzhen", and "yuan"
#> 
#> ── Iteration 2 (threshold of cosine similarity = 0.6) ──────────────────────────
#> ✔ 4 more words appended: "Wang", "Chen", "mainland", and "HK"
#> 
#> ── Iteration 3 (threshold of cosine similarity = 0.6) ──────────────────────────
#> ✔ No more word appended. Successfully convergent.
#> 
#> ── Finish (convergent) ──
#> 
dict.cn  # adequate to represent "China"
#>  [1] "China"     "Chinese"   "Beijing"   "Taiwan"    "Taiwanese" "Shanghai" 
#>  [7] "Li"        "Guangzhou" "Shenzhen"  "yuan"      "Wang"      "Chen"     
#> [13] "mainland"  "HK"       
```
