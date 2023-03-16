inner <- function(a, b) {
  stopifnot(dim(a) == dim(b))
  
  vec_vec_inner <- function(a, b) {
    stopifnot(length(a) == length(b))
    res <- 0
    for(i in 1:length(a)) {
      res <- res + a[i]*b[i]
    }
    res
  }
  
  rec <- function(a, b, env) {
    if( is.null(dim(a)) && is.null(dim(b)) ) {
      res <- vec_vec_inner(a, b)
      env$res <- c(env$res, res)
      return(res)
    } else {
        l <- length(dim(a))
        last_dim <- dim(a)[l]
        trash <- lapply(seq(last_dim), function(x) {
          indices <- rep(alist(,)[1], l)
          indices[l] <- x
          a <- do.call(`[`, c(list(a), indices))
          b <- do.call(`[`, c(list(b), indices))
          ret <- rec(a, b, env)
          return(0)
        })
    }
  }
  
  env <- new.env()
  env$res <- NULL
  if(is.null(dim(a))) {
    env$res <- vec_vec_inner(a, b)
  } else {
    l <- dim(a)[length(dim(a))]
    for(i in 1:l) {
      rec(a, b, env)
    }
  }
  
  if(is.null(dim(a))) {
    return(env$res)
  } else {
    return(sum(env$res)/dim(a)[length(dim(a))] )
  }
  
}

a <- c(2, 5, 6)
b <- c(4, 3, 2)
inner(a, b)
stopifnot( a%*%b == inner(a, b))

a <- array(1:36, dim = c(3, 3, 4))
b <- array( (1:36) + 3.14, dim = c(3, 3, 4))
stopifnot( a%*%b == inner(a, b))

# Outer product
a <- c(2, 5, 6, 4)
b <- c(4, 3, 7)
out <- function(a, b, f = NULL) {
  if(is.null(f)) {
    f <- `*`
  } else {
    stopifnot(is.function(f))
  }
  stopifnot(dim(a) == dim(b))
  res <- NULL
  if(is.null(dim(a))) {
    res <- array(0, dim = c(length(a), length(b)))
  } else {
    res <- array(0, dim = c(dim(a), dim(b)) )  
  }
  counter <- 1
  for(i in 1:length(b)) { # cols
    temp_b <- b[i]
    for(j in 1:length(a)) { # rows
      res[counter] <- f(a[j], temp_b)
      counter <- counter + 1
    }
  }
  
  return(res)
}

outer(a, b, `^`)
outer(a, b, \(a, b) {paste0(a, "^", b)})
out(a, b, `^`)
out(a, b, \(a, b) {paste0(a, "^", b)})
stopifnot( a%o%b == out(a, b))

a <- array(1:36, dim = c(3, 3, 4))
b <- array( (1:36) + 3.14, dim = c(3, 3, 4))
stopifnot( a%o%b == out(a, b))


# Kronecker product

# Transpose

# Matrix multiplication

# Eigenvalues and Eigenvectors

# Singular Value Decomposition and Determinants

# Least Squares Fitting and QR Decomposition

# Forming Partitioned Matrices