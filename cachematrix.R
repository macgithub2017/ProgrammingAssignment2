## Code written by macgithup2017 22.03.2017
## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inver <- NULL
      set <- function(y) {
            x <<- y
            inver <<- NULL
      }
      get <- function() x
      set.inver <- function(inverse.matrix) inver <<- inverse.matrix
      get.inver <- function() inver
      list(set=set, get=get,
           set.inver = set.inver,
           get.inver = get.inver)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inver <- x$get.inver()
      if(!is.null(inver)) {
            message("getting cached data")
            return(inver)
      }
      dat <- x$get()
      inver <- solve(dat, ...)
      x$set.inver(inver)
      inver
}
