# combinadics

N <- 63
k <- 4


# combn()
# gtools::combinations()
# combinat::combn()

#choose(7, k)

largest_ak <- function(k, N) {
  n <- k + 1
  combos <- 0
  while(combos <= N) {
    combos <- choose(n, k)
    n <- n + 1
  }
  return(n-2)
}

ak_values <- function(k, N) {
  ak_list <- vector("list", k)
  i <- 1
  while(k > 0) {
    ak <- largest_ak(k, N)
    ak_list[[i]] <- ak
    N <- N - choose(ak, k)
    k <- k - 1
    i <- i + 1
  }
  return(unlist(ak_list))
}

x <- c(7, 6, 4, 2)


combinadics_rank <- function(ak) {
  stopifnot(is.numeric(ak))
  k <- sort(1:length(ak), decreasing = TRUE)
  sum(map2_dbl(ak, k, choose))
}






combinadics_unrank <- function(k, N) {
  ak_vec <- ak_values(k, N)
  k_vec <- sort(1:k, decreasing = TRUE)
  sum(map2_dbl(ak_vec, k_vec, choose))
}


