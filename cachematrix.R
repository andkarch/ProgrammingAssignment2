

############################
###########PART1############                                  
## Creating a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  #Build object for the inverse and assign NULL to it
  i <- NULL
  
  #Set matrix and include m and i
  a <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  #Get matrix
  b <- function() {
    m
  }
  
  #Set inverse of matrix and assign it to i
  c <- function(inverse) {
    i <<- inverse
  }
  
  #Get inverse of matrix
  d <- function() {
    i
  }
  
  #List results
list(a = a, b = b, c = c, d = d)
}

############################
###########PART2############
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #Return matrix inverse of x
  m <- x$d()
  if( !is.null(m) ) {
    return(m)
  }
  
  #Get matrix
  data <- x$get()
  
  #Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  #Set inverse
  x$c(m)
  
  ## Return matrix
  m
}
