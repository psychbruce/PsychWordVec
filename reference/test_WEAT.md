# Word Embedding Association Test (WEAT) and Single-Category WEAT.

Tabulate data (cosine similarity and standardized effect size) and
conduct the permutation test of significance for the *Word Embedding
Association Test* (WEAT) and *Single-Category Word Embedding Association
Test* (SC-WEAT).

- For WEAT, two-samples permutation test is conducted (i.e.,
  rearrangements of data).

- For SC-WEAT, one-sample permutation test is conducted (i.e.,
  rearrangements of +/- signs to data).

## Usage

``` r
test_WEAT(
  data,
  T1,
  T2,
  A1,
  A2,
  use.pattern = FALSE,
  labels = list(),
  p.perm = TRUE,
  p.nsim = 10000,
  p.side = 2,
  seed = NULL,
  pooled.sd = "Caliskan"
)
```

## Arguments

- data:

  A
  [`wordvec`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (data.table) or
  [`embed`](https://psychbruce.github.io/PsychWordVec/reference/as_embed.md)
  (matrix), see
  [`data_wordvec_load()`](https://psychbruce.github.io/PsychWordVec/reference/data_wordvec_load.md).

- T1, T2:

  Target words (a vector of words or a pattern of regular expression).
  If only `T1` is specified, it will tabulate data for single-category
  WEAT (SC-WEAT).

- A1, A2:

  Attribute words (a vector of words or a pattern of regular
  expression). Both must be specified.

- use.pattern:

  Defaults to `FALSE` (using a vector of words). If you use regular
  expression in `T1`, `T2`, `A1`, and `A2`, please specify this argument
  as `TRUE`.

- labels:

  Labels for target and attribute concepts (a named `list`), such as
  (the default)
  `list(T1="Target1", T2="Target2", A1="Attrib1", A2="Attrib2")`.

- p.perm:

  Permutation test to get exact or approximate *p* value of the overall
  effect. Defaults to `TRUE`. See also
  [`sweater::weat_exact()`](https://rdrr.io/pkg/sweater/man/weat_resampling.html).

- p.nsim:

  Number of samples for resampling in permutation test. Defaults to
  `10000`.

  If `p.nsim` is larger than the number of all possible permutations
  (rearrangements of data), it will be ignored and an exact permutation
  test will be conducted. Otherwise (in most cases for real data and
  always for SC-WEAT), a resampling test is performed, which takes much
  less computation time and produces the approximate *p* value
  (comparable to the exact one).

- p.side:

  One-sided (`1`) or two-sided (`2`) *p* value. Defaults to `2`.

  In Caliskan et al.'s (2017) article, they reported one-sided *p* value
  for WEAT. Here, I suggest reporting two-sided *p* value as a more
  conservative estimate. The users take the full responsibility for the
  choice.

  - The one-sided *p* value is calculated as the proportion of sampled
    permutations where the difference in means is greater than the test
    statistic.

  - The two-sided *p* value is calculated as the proportion of sampled
    permutations where the absolute difference is greater than the test
    statistic.

- seed:

  Random seed for reproducible results of permutation test. Defaults to
  `NULL`.

- pooled.sd:

  Method used to calculate the pooled *SD* for effect size estimate in
  WEAT.

  - Defaults to `"Caliskan"`: `sd(data.diff$cos_sim_diff)`, which is
    suggested and identical to Caliskan et al.'s (2017) original
    approach.

  - Otherwise specified, it will calculate the pooled *SD* as:
    \\\sqrt{\[(n_1 - 1) \* \sigma_1^2 + (n_2 - 1) \* \sigma_2^2\] /
    (n_1 + n_2 - 2)}\\. This is *NOT suggested* because it may
    *overestimate* the effect size, especially when there are only a few
    T1 and T2 words that have small variances.

## Value

A `list` object of new class `weat`:

- `words.valid`:

  Valid (actually matched) words

- `words.not.found`:

  Words not found

- `data.raw`:

  A `data.table` of cosine similarities between all word pairs

- `data.mean`:

  A `data.table` of *mean* cosine similarities *across* all attribute
  words

- `data.diff`:

  A `data.table` of *differential* mean cosine similarities *between*
  the two attribute concepts

- `eff.label`:

  Description for the difference between the two attribute concepts

- `eff.type`:

  Effect type: WEAT or SC-WEAT

- `eff`:

  Raw effect, standardized effect size, and p value (if `p.perm=TRUE`)

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## References

Caliskan, A., Bryson, J. J., & Narayanan, A. (2017). Semantics derived
automatically from language corpora contain human-like biases. *Science,
356*(6334), 183–186.

## See also

[tab_similarity](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md)

[`dict_expand()`](https://psychbruce.github.io/PsychWordVec/reference/dict_expand.md)

[`dict_reliability()`](https://psychbruce.github.io/PsychWordVec/reference/dict_reliability.md)

[`test_RND()`](https://psychbruce.github.io/PsychWordVec/reference/test_RND.md)

## Examples

``` r
## cc() is more convenient than c()!

weat = test_WEAT(
  demodata,
  labels=list(T1="King", T2="Queen", A1="Male", A2="Female"),
  T1=cc("king, King"),
  T2=cc("queen, Queen"),
  A1=cc("male, man, boy, brother, he, him, his, son"),
  A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
  seed=1)
weat
#> 
#> ── WEAT (Word Embedding Association Test) ──────────────────────────────────────
#> 
#> 2 King (T1) valid words
#> 2 Queen (T2) valid words
#> 8 Male (A1) valid words
#> 8 Female (A2) valid words
#>  
#> Relative semantic similarities (differences):
#> ───────────────────────────────────────────────
#>          Target cos_sim_diff std_diff closer_to
#> ───────────────────────────────────────────────
#>  "king"    King        0.104    1.587      Male
#>  "King"    King        0.090    1.598      Male
#>  "queen"  Queen       -0.176   -1.751    Female
#>  "Queen"  Queen       -0.129   -1.781    Female
#> ───────────────────────────────────────────────
#> 
#> Mean differences for single target category:
#> ─────────────────────────────────────────────
#>  Target mean_raw_diff mean_std_diff     p    
#> ─────────────────────────────────────────────
#>    King         0.097         1.592 <.001 ***
#>   Queen        -0.152        -1.766 <.001 ***
#> ─────────────────────────────────────────────
#> Permutation test: approximate p values (forced to two-sided)
#> 
#> Overall effect (raw and standardized mean differences):
#> ─────────────────────────────────────────────────────────
#>      Target       Attrib mean_diff_raw eff_size     p    
#> ─────────────────────────────────────────────────────────
#>  King/Queen  Male/Female         0.249    1.716 <.001 ***
#> ─────────────────────────────────────────────────────────
#> Permutation test: exact p value = 0.00e+00 (two-sided)
#> 

sc_weat = test_WEAT(
  demodata,
  labels=list(T1="Occupation", A1="Male", A2="Female"),
  T1=cc("
    architect, boss, leader, engineer, CEO, officer, manager,
    lawyer, scientist, doctor, psychologist, investigator,
    consultant, programmer, teacher, clerk, counselor,
    salesperson, therapist, psychotherapist, nurse"),
  A1=cc("male, man, boy, brother, he, him, his, son"),
  A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
  seed=1)
sc_weat
#> 
#> ── SC-WEAT (Single-Category Word Embedding Association Test) ───────────────────
#> 
#> 21 Occupation (T1) valid words
#> 8 Male (A1) valid words
#> 8 Female (A2) valid words
#>  
#> Relative semantic similarities (differences):
#> ──────────────────────────────────────────────────
#>                    cos_sim_diff std_diff closer_to
#> ──────────────────────────────────────────────────
#>  "architect"              0.087    1.048      Male
#>  "boss"                   0.081    1.027      Male
#>  "leader"                 0.079    0.985      Male
#>  "engineer"               0.071    0.775      Male
#>  "CEO"                    0.045    0.637      Male
#>  "officer"                0.033    0.448      Male
#>  "manager"                0.022    0.342      Male
#>  "lawyer"                 0.020    0.238      Male
#>  "scientist"              0.008    0.170      Male
#>  "doctor"                 0.013    0.169      Male
#>  "psychologist"           0.001    0.030      Male
#>  "investigator"           0.001    0.012      Male
#>  "consultant"            -0.003   -0.055    Female
#>  "programmer"            -0.006   -0.144    Female
#>  "teacher"               -0.067   -0.800    Female
#>  "clerk"                 -0.078   -1.035    Female
#>  "counselor"             -0.064   -1.196    Female
#>  "salesperson"           -0.083   -1.207    Female
#>  "therapist"             -0.069   -1.245    Female
#>  "psychotherapist"       -0.092   -1.323    Female
#>  "nurse"                 -0.162   -1.491    Female
#> ──────────────────────────────────────────────────
#> 
#> Overall effect (raw and standardized mean differences):
#> ─────────────────────────────────────────────────────────
#>      Target       Attrib mean_diff_raw eff_size     p    
#> ─────────────────────────────────────────────────────────
#>  Occupation  Male/Female        -0.008   -0.124  .599    
#> ─────────────────────────────────────────────────────────
#> Permutation test: approximate p value = 5.99e-01 (two-sided)
#> 

if (FALSE) { # \dontrun{

## the same as the first example, but using regular expression
weat = test_WEAT(
  demodata,
  labels=list(T1="King", T2="Queen", A1="Male", A2="Female"),
  use.pattern=TRUE,  # use regular expression below
  T1="^[kK]ing$",
  T2="^[qQ]ueen$",
  A1="^male$|^man$|^boy$|^brother$|^he$|^him$|^his$|^son$",
  A2="^female$|^woman$|^girl$|^sister$|^she$|^her$|^hers$|^daughter$",
  seed=1)
weat

## replicating Caliskan et al.'s (2017) results
## WEAT7 (Table 1): d = 1.06, p = .018
## (requiring installation of the `sweater` package)
Caliskan.WEAT7 = test_WEAT(
  as_wordvec(sweater::glove_math),
  labels=list(T1="Math", T2="Arts", A1="Male", A2="Female"),
  T1=cc("math, algebra, geometry, calculus, equations, computation, numbers, addition"),
  T2=cc("poetry, art, dance, literature, novel, symphony, drama, sculpture"),
  A1=cc("male, man, boy, brother, he, him, his, son"),
  A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
  p.side=1, seed=1234)
Caliskan.WEAT7
# d = 1.055, p = .0173 (= 173 counts / 10000 permutation samples)

## replicating Caliskan et al.'s (2017) supplemental results
## WEAT7 (Table S1): d = 0.97, p = .027
Caliskan.WEAT7.supp = test_WEAT(
  demodata,
  labels=list(T1="Math", T2="Arts", A1="Male", A2="Female"),
  T1=cc("math, algebra, geometry, calculus, equations, computation, numbers, addition"),
  T2=cc("poetry, art, dance, literature, novel, symphony, drama, sculpture"),
  A1=cc("male, man, boy, brother, he, him, his, son"),
  A2=cc("female, woman, girl, sister, she, her, hers, daughter"),
  p.side=1, seed=1234)
Caliskan.WEAT7.supp
# d = 0.966, p = .0221 (= 221 counts / 10000 permutation samples)
} # }
```
