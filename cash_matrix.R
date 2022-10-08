createCashMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

get <- function() x
  settask <- function(solve) m <<- solve
  gettask <- function() m
  list(set = set, get = get,
       settask = settask,
       gettask = gettask)
}

cashTask <- function(x, ...) {
  o <- x$gettask()
  if (!is.null(o)) {
    message("getting cached matrix inverse")
    return(o)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$settask(o)
  m
}