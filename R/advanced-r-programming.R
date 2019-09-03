factorial_loop <- function(n) {
  
  if(n == 0 ) {
    return(1)
  } 
    
  res <- 1
  
  for(i in rev(seq_len(n))) {
    
    res <- res * i
    
  }

  
  res
  
}


factorial_reduce <- function(n) {
  
  if(n == 0) {
    return(1)
  }
    
  res <- purrr::reduce(n:1, function(a, b) { a * b })
  
  
  res
}

factorial_func <- function(n, val = 1) {
  
  if(n == 0) return(val)
  
  
  val <- n * val
  n <- n - 1
  
  factorial_func(n, val)
  
}


factorial_mem <- local({
  
  memory <- list()
  
  function(n) {
    
    if(n == 0) return(1)
    
    if(!is.null(memory[[as.character(n)]])) {
      res <- memory[[as.character(n)]]
    } else {
      res <- factorial_loop(n)
      memory[[as.character(n)]] <<- res
      
    }
    
    res
  }
  
  
  
})


microbenchmark::microbenchmark(
  factorial_mem(1000), 
  factorial_func(1000), 
  factorial_loop(1000), 
  factorial_reduce(1000)
)
