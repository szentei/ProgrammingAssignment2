#Functions:
#  makeCacheMatrix: creates a special "matrix" object that can cache its inverse
#  cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix

#Desc: This function creates a special "matrix" object that can cache its inverse.
#Auth: Istvan Szente
#Date: 2014-10-24
makeCacheMatrix <- function(x = matrix()) {
  
  # sets inverse matrix variable to NULL
  im <- NULL
  
  # function for setting the matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  # function for getting the matrix
  get <- function() x
  
  # function for setting the inverse of the matrix
  setim <- function(y) im <<- y
  
  # function for getting the inverse of the matrix
  getim <- function() im
  
  # returns a list of the functions
  list(set = set, get = get,
       setim = setim,
       getim = getim)
  
}

#Desc: This function computes the inverse of the special "matrix" returned by
#      makeCacheMatrix above. If the inverse has already been calculated (and
#      the matrix has not changed), then the cacheSolve retrieves the inverse
#      from the cache.
#Auth: Istvan Szente
#Date: 2014-10-24
cacheSolve <- function(x, ...) {
  
  # gets inverse matrix from the cache
  im <- x$getim()
  # if inverse is cached (not null) then returns it
  if(!is.null(im)) {
    return(im)
  }
  
  # if inverse is not cached then gets the matrix 
  # and calculates & sets & returns its inverse
  m <- x$get()
  im <- solve(m, ...)
  x$setim(im)
  im
  
}