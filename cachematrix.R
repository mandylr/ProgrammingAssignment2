## These functions work together to set and store the inverse of a matrix. 
## Once the inverse has been calculated, its value is stored in the parent environment
## using the <<- assignment allowing the functions to subsequently retrieve the calculated inverse
## without re-caclculating it. 

## makeCacheMatrix takes a matrix as and creates a list as its output. 

## The list contains functions to:
## 1. Set the value of the input matrix
## 2. Get the input matrix
## 3. Set the inverse of the input matrix
## 4. Get the inverse of the input matrix 

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    get <- function() x
    setmatrix <- function(inverse) matrix <<- inverse
    getmatrix <- function() matrix
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## cacheSolve calculates the inverse of the input matrix set in the above function. 
## First it checks to see if the inverse has already been calculated:

## If the inverse has already been calculated, cacheSolves returns the previously calculated 
## inverse and a message "getting cached data"

## If no inverse has been caculated, it calculates the inverse and assigns it to the 
## inverse "matrix" variable using the setmatrix function of the makeCacheMatrix function 

cacheSolve <- function(x, ...) {
    matrix <- x$getmatrix()
    if(!is.null(matrix)) {
        message("getting cached data")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data, ...)
    x$setmatrix(matrix)
    matrix
        ## Return a matrix that is the inverse of 'x'
}
