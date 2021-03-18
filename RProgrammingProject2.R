makeCacheMatrix <- function( m = matrix() ) {
     i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
        }
  
      get <- function() m
      setInverse <- function(inverse)  i <<- inverse
      getInverse <- function() i

      list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
    }
> cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      if( !is.null(m) ) {
          message("getting cached data")
          return(m)
          }
      stuff <- x$get()
      m <- solve(stuff) %*% stuff
      x$setInverse(m)
      m
    }