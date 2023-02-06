# https://towardsdatascience.com/how-to-generate-random-variables-from-scratch-no-library-used-4b71eb3c8dc7
# https://www.scribbr.com/statistics/chi-square-distributions/

pseudo_uniform_good <- function(mult = 5, mod = (2^31)-1, seed = 1, size = 1) {
  U <- rep(0, size)
  x <- (seed*mult + 1)%%mod
  U[1] <- x/mod
  for(i in seq_along(2:size)) {
    x <- (x*mult + 1)%%mod
    U[i] <- x/mod
  }
  U
}

v <- pseudo_uniform_good(seed = 3, size = 1000)
hist(v)
