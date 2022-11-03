cn = function() cat("\n")

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

