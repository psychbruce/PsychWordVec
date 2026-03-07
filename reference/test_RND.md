# Relative Norm Distance (RND) analysis.

Tabulate data and conduct the permutation test of significance for the
*Relative Norm Distance* (RND; also known as *Relative Euclidean
Distance*). This is an alternative method to [Single-Category
WEAT](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md).

## Usage

``` r
test_RND(
  data,
  T1,
  A1,
  A2,
  use.pattern = FALSE,
  labels = list(),
  p.perm = TRUE,
  p.nsim = 10000,
  p.side = 2,
  seed = NULL
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

- T1:

  Target words of a single category (a vector of words or a pattern of
  regular expression).

- A1, A2:

  Attribute words (a vector of words or a pattern of regular
  expression). Both must be specified.

- use.pattern:

  Defaults to `FALSE` (using a vector of words). If you use regular
  expression in `T1`, `T2`, `A1`, and `A2`, please specify this argument
  as `TRUE`.

- labels:

  Labels for target and attribute concepts (a named `list`), such as
  (the default) `list(T1="Target", A1="Attrib1", A2="Attrib2")`.

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

## Value

A `list` object of new class `rnd`:

- `words.valid`:

  Valid (actually matched) words

- `words.not.found`:

  Words not found

- `data.raw`:

  A `data.table` of (absolute and relative) norm distances

- `eff.label`:

  Description for the difference between the two attribute concepts

- `eff.type`:

  Effect type: RND

- `eff`:

  Raw effect and p value (if `p.perm=TRUE`)

- `eff.interpretation`:

  Interpretation of the RND score

## Download

Download pre-trained word vectors data (`.RData`):
<https://psychbruce.github.io/WordVector_RData.pdf>

## References

Garg, N., Schiebinger, L., Jurafsky, D., & Zou, J. (2018). Word
embeddings quantify 100 years of gender and ethnic stereotypes.
*Proceedings of the National Academy of Sciences, 115*(16), E3635–E3644.

Bhatia, N., & Bhatia, S. (2021). Changes in gender stereotypes over
time: A computational analysis. *Psychology of Women Quarterly, 45*(1),
106–125.

## See also

[`tab_similarity()`](https://psychbruce.github.io/PsychWordVec/reference/tab_similarity.md)

[`dict_expand()`](https://psychbruce.github.io/PsychWordVec/reference/dict_expand.md)

[`dict_reliability()`](https://psychbruce.github.io/PsychWordVec/reference/dict_reliability.md)

[`test_WEAT()`](https://psychbruce.github.io/PsychWordVec/reference/test_WEAT.md)

## Examples

``` r
rnd = test_RND(
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
rnd
#> 
#> ── Relative Norm Distance (RND) ────────────────────────────────────────────────
#> 
#> 21 Occupation (T1) valid words
#> 8 Male (A1) valid words
#> 8 Female (A2) valid words
#>  
#> Relative norm distances (differences):
#> ─────────────────────────────────────────────────────────────
#>                       rnd closer_to norm_dist_A1 norm_dist_A2
#> ─────────────────────────────────────────────────────────────
#>  "architect"       -0.105      Male        1.138        1.243
#>  "boss"            -0.110      Male        1.028        1.138
#>  "leader"          -0.102      Male        1.105        1.206
#>  "engineer"        -0.093      Male        1.123        1.216
#>  "CEO"             -0.065      Male        1.235        1.300
#>  "officer"         -0.063      Male        1.098        1.161
#>  "manager"         -0.051      Male        1.171        1.222
#>  "lawyer"          -0.054      Male        1.048        1.102
#>  "scientist"       -0.040      Male        1.135        1.175
#>  "doctor"          -0.052      Male        0.965        1.017
#>  "psychologist"    -0.036      Male        1.080        1.115
#>  "investigator"    -0.035      Male        1.095        1.130
#>  "consultant"      -0.029      Male        1.172        1.202
#>  "programmer"      -0.027      Male        1.172        1.199
#>  "teacher"          0.029    Female        1.028        0.999
#>  "clerk"            0.039    Female        1.046        1.007
#>  "counselor"        0.025    Female        1.089        1.065
#>  "salesperson"      0.040    Female        1.123        1.083
#>  "therapist"        0.030    Female        1.059        1.029
#>  "psychotherapist"  0.050    Female        1.115        1.066
#>  "nurse"            0.125    Female        1.053        0.927
#> ─────────────────────────────────────────────────────────────
#> If RND < 0: Occupation is more associated with Male than Female
#> If RND > 0: Occupation is more associated with Female than Male
#> 
#> Overall effect (raw):
#> ──────────────────────────────────────────
#>      Target       Attrib rnd_sum     p    
#> ──────────────────────────────────────────
#>  Occupation  Male/Female  -0.523  .076 .  
#> ──────────────────────────────────────────
#> Permutation test: approximate p value = 7.57e-02 (two-sided)
#> 
```
