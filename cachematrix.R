## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m      
}


# Sample Answer
#> x<-matrix(c(1,2,3,4),2,2)
#> my_answer<-makeCacheMatrix(x)
#> my_answer$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(my_answer)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> my_answer$getInv()
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

