## Put comments here that give an overall description of what your
## functions do
## R Programming - Assignment #2
## Lexical Scoping
## July 26, 2015

## The first portion of the code creates a matrix.
## makeCacheMatrix: This function creates a 
## special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     matrix_cache <- NULL
     st_mtrx <- function(Value)
       {
       x <<- Value
       matrix_cache <- NULL}
     gt_mtrx <- function()
       {
       x}
     cache_invrs <- function(solve)
     {
       matrix_cache <<- solve
     }
     gt_invrs <- function()
     {
       matrix_cache
     }
     list(st_mtrx = st_mtrx, gt_mtrx = gt_mtrx, cache_invrs = cache_invrs,
          gt_invrs = gt_invrs)
}

## cacheSolve: This function computes the inverse 
## of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Invrs <- x$getInverse()
  if(!is.null(Invrs))
  {
    message("Retrieving cached data...")
    return(Invrs)
  }
  dat <- x$getMatrix()
  Invrs <- solve(dat)
  x$cacheInverse(Invrs)
  Invrs
}


##  ASSIGNMENT EXAMPLE
##makeVector <- function(x = numeric()) {
##  m <- NULL
##  set <- function(y) {
##    x <<- y
##    m <<- NULL
##  }
##  get <- function() x
##  setmean <- function(mean) m <<- mean
##  getmean <- function() m
##  list(set = set, get = get,
##       setmean = setmean,
##       getmean = getmean)
##}

##cachemean <- function(x, ...) {
##  m <- x$getmean()
##  if(!is.null(m)) {
##    message("getting cached data")
##    return(m)
##  }
##  data <- x$get()
## m <- mean(data, ...)
##  x$setmean(m)
##  m
##}