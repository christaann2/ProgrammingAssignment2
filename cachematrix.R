## makeCacheMatrix does the following
## - sets the matrix
## - gets the matrix
## - sets the matrix inverse
## - gets the matrix inverse
## - stores the data in inv_matrix


makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(z) {
    x <<- z
    inv_matrix <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_matrix <<- inv
  getinv <- function() inv_matrix
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## cacheSolve returns the inverse of the matrix by doing the following:
## - gets the stored inverse matrix
## - returns the inverse matrix if it exists, else
## - calculates the inverse matrix
## - stores the information in cache
## - returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)) {
    message("getting cached matrix.")    
    return(inv_matrix)
  }
  mat_inv_data <- x$get()
  inv_matrix <- solve(mat_inv_data)
  x$setinv(inv_matrix)
  inv_matrix
        ## Return a matrix that is the inverse of 'x'
}

## sample test
## > x <- rbind(c(1, 2), c(3, 4))
## > m <- makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > cacheSolve(m)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
  
