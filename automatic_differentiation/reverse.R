Dual <- function(x, dx=0) {
  structure(list(value=x, derivative=dx), class="Dual")
}

`+.Dual` <- function(a, b) {
  if (is.numeric(a)) a <- Dual(a)
  if (is.numeric(b)) b <- Dual(b)
  Dual(a$value + b$value, a$derivative + b$derivative)
}

`*.Dual` <- function(a, b) {
  if (is.numeric(a)) a <- Dual(a)
  if (is.numeric(b)) b <- Dual(b)
  Dual(a$value * b$value, a$value * b$derivative + b$value * a$derivative)
}

`sin.Dual` <- function(a) {
  if (is.numeric(a)) a <- Dual(a)
  Dual(sin(a$value), cos(a$value))
}

reverse_ad <- function(expr) {
  tape <- list()
  
  # check_if_symbol
  cis <- function(x) {
    return( (length(x)>1) && (!is.symbol(x)))
  }
  
  forward <- function(expr) {
    
    if (!is.call(expr)) {
      return(expr)
    }
    
    fct <- expr[[1]]
    
    if(fct == as.name("+")) {
      
      l <- expr[[2]]
      r <- expr[[3]]
      
      if(cis(l) && cis(r)) {
        
        inpv <- forward(l)
        inpv <- forward(r)
        tape <<- c(tape, list("+",
                              value = eval(l)$value,
                              deriv = 1),
                              value = eval(r)$value,
                              deriv = 1)
        
      } else if(cis(l) && !cis(r)) {
        
        inpv <- forward(l)
        tape <<- c(tape, list("+",
                              value = eval(l)$value,
                              deriv = 1),
                              value = eval(r)$value,
                              deriv = 1)
        
      } else if(!cis(l) && cis(r)) {
        
        inpv <- forward(r)
        tape <<- c(tape, list("+",
                              value = eval(l)$value,
                              deriv = 1),
                              value = eval(r)$value,
                              deriv = 1)
        
      } else if(!cis(l) && !cis(r)) {
        
        tape <<- c(tape, list("+",
                              value = eval(l)$value,
                              deriv = 1),
                              value = eval(r)$value,
                              deriv = 1)
        
      }
      
    } else if(fct == as.name("*")) {
      
      l <- expr[[2]]
      r <- expr[[3]]
      
      if(cis(l) && cis(r)) {
        
        inpv <- forward(l)
        inpv <- forward(r)
        tape <<- c(tape, list("*",
                              value = eval(l)$value,
                              deriv = 1),
                              value = eval(r)$value,
                              deriv = 1)
        
      } else if(cis(l) && !cis(r)) {
        
        inpv <- forward(l)
        tape <<- c(tape, list("*",
                              value = eval(l)$value,
                              deriv = 1),
                              value = eval(r)$value,
                              deriv = 1)
        
      } else if(!cis(l) && cis(r)) {
        
        inpv <- forward(r)
        tape <<- c(tape, list("*",
                              value = eval(l)$value,
                              deriv = 1),
                              value = eval(r)$value,
                              deriv = 1)
        
      } else if(!cis(l) && !cis(r)) {
        
        tape <<- c(tape, list("*",
                              value = eval(l)$value,
                              deriv = 1),
                              value = eval(r)$value,
                              deriv = 1)
      }
      
    } else if(fct == "sin") {
      
      inp <- expr[[2]]
      if( (length(inp) > 1) && (!is.symbol(inp)) ) {
        inpv <- forward(inp)  
        tape <<- c(tape, list("sin", 
                              value = eval(inp)$value,
                              deriv = 1))
      } else {
        tape <<- c(tape, list("sin",
                                value = eval(inp)$value,
                                deriv = eval(inp)$derivative))
      }
      
    } else {
      stop("unknown function")
    }
    
  }
  
  result <- forward(expr)
  tape <- unlist(tape)
  
  backward <- function(tape) {
    counter <- 1
    deriv <- 1
    while(counter <= length(tape)) {
      operation = tape[counter]
      if(operation == "*") {
        lv <- tape[counter + 1] |> as.numeric()
        ld <- tape[counter + 2] |> as.numeric()
        rv <- tape[counter + 3] |> as.numeric()
        rd <- tape[counter + 4] |> as.numeric()
        deriv <- deriv * (lv*rd + rv*ld)
        counter <- counter + 4
      } else if(operation == "+") {
        lv <- tape[counter + 1] |> as.numeric()
        ld <- tape[counter + 2] |> as.numeric()
        rv <- tape[counter + 3] |> as.numeric()
        rd <- tape[counter + 4] |> as.numeric()
        deriv <- deriv * (ld + rd)
        counter <- counter + 4
      } else if(operation == "sin") {
        inp <- tape[counter + 1] |> as.numeric()
        deriv <- deriv * cos(inp) 
        counter <- counter + 1
      }
      counter <- counter + 1
    } 
    return(deriv)
  }
  
  backward(tape)
}

check_equal <- function(a, b) {
  stopifnot("unqeal"=a==c(attributes(b)$gradient))
}

y <- Dual(3, 1)
expr <- quote(y*y)
result <- reverse_ad(expr)
y <- 3
check_equal(result, eval(deriv(expr, "y")))

# Test 1: addition of two variables
y <- Dual(3, 1)
expr <- quote(y + y)
result <- reverse_ad(expr)
y <- 3
check_equal(result, eval(deriv(expr, "y")))

# Test 2: multiplication of two variables
y <- Dual(3, 1)
expr <- quote(sin(y*y))
result <- reverse_ad(expr)
y <- 3
check_equal(result, eval(deriv(expr, "y")))

# Test 3: sine of a variable
y <- Dual(3, 1)
expr <- quote(sin(y)) 
result <- reverse_ad(expr)
y <- 3
check_equal(result, eval(deriv(expr, "y")))


