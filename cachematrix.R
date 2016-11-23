## Return a matrix that is the inverse of 'x'

makeCacheMatrix  <- function(x = matrix()) { 
    inverse_matrix <- NULL
    set_matrix <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(solved_matrix) inverse_matrix <<- solved_matrix
    get_inverse <- function() inverse_matrix
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

cacheSolve <- function(x, ...) {
    inverse_matrix <- x$get_inverse()
    if(!is.null(inverse_matrix)) {  
        message("getting cached data")
        return(inverse_matrix)
    }
    data <- x$get_inverse()
    inverse_matrix <- solve(data, ...)
    x$set_inverse(inverse_matrix)
    return(inverse_matrix)
}
