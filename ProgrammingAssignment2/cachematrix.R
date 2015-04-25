##Cachematrix function : This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL               ##set inverse of a matrix to null
  
  ## set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get matrix
  get <- function() x           
  ## setting inverse matrix
  setimatrix <- function(imatrix) 
    i <<- imatrix
  ## getting inverse matrix
  getimatrix <- function() i                 
  
  ## list names
  list(set = set, get = get,
       setimatrix = setimatrix,
       getimatrix = getimatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  ## imatrix value
  i <- x$getimatrix() ## gets the invmat in previous function
  
  ## if inverse matrix is already stored, cachesolve will retrieve the inverse from the cache.
  if(!is.null(i)) {
    message("Return Cached Data")
    return(i)
  }
  ## if not, calculate inverse of the matrix 
  data <- x$get()              ## get the matrix
  i <- solve(data, ...)      ## calculate the inverse
  x$setimatrix(i)             ## set the inverse of the matrix
  i                        
  
}