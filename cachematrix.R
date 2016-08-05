## Developer Ersin KALDI
## Week 3 Assignment
## This function creates a special "matrix" object that can cache its inverse
# [functions] #
# [makeCacheMatrix] creates a matrix whose inverse form can be cached
# [getinverse] --> returns the inverse of matrix
# [setinverse] --> sets the inverse of matrix
# [get] --> returns the original matrix
# [set] --> sets the original matrix
# [functions] #

makeCacheMatrix <- function(mmtx = matrix()) { 				 ## default value "matrix" assigned to argument
    matinv <- NULL                            				 ## matinv initialized
    set <- function(nmtx) {                    				 ## define the set function to assign new 
        mmtx <<- nmtx				 
        matinv <<- NULL                       				 ## matinv resets to NULL in case of a new matrix
    }
    get <- function() mmtx 							 ## get fucntion defined
    setinverse <- function(inverse) matinv <<- inverse  		 ## value of matinv assigned from parent env.
    getinverse <- function() matinv                    		 ## gets the value of matinv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)                                                                          
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(mmtx , ...) {
    matinv <- mmtx$getinverse()
    if(!is.null(matinv)) {
        message("Second call - getting cached data")
        return(matinv)
    }
    data <- mmtx$get()
    matinv <- solve(data, ...)
    mmtx$setinverse(matinv)
    matinv
}



# Run Time Example
# 
# ekMatrix <- matrix( sample(1:25), nrow=5,  ncol=5) 
#> ekMatrix
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    5   19   11   25   23
#[2,]    6   17    7    8    1
#[3,]   22   14   16    9    2
#[4,]   13    3    4   24   12
#[5,]   20   15   10   18   21

#> ekcache <- makeCacheMatrix(ekMatrix )
#> cacheSolve(ekcache)
#        [,1]        [,2]         [,3]        [,4]        [,5]
#[1,] -0.059610227  0.02006729 -0.006680428  0.01607204  0.05578402
#[2,] -0.023517492  0.09146390 -0.041464168 -0.01654557  0.03480541
#[3,]  0.094832827 -0.11323570  0.111080409 -0.03658469 -0.08814590
#[4,]  0.009130461  0.02070337 -0.002194301  0.05978035 -0.04493712
#[5,]  0.020585255 -0.04826704 -0.015035028 -0.03730746  0.05012218

# When we call the function again, it brings data from cach and it prints "second call"

#> cacheSolve(ekcache)
#Second call - getting cached data
#        [,1]        [,2]         [,3]        [,4]        [,5]
#[1,] -0.059610227  0.02006729 -0.006680428  0.01607204  0.05578402
#[2,] -0.023517492  0.09146390 -0.041464168 -0.01654557  0.03480541
#[3,]  0.094832827 -0.11323570  0.111080409 -0.03658469 -0.08814590
#[4,]  0.009130461  0.02070337 -0.002194301  0.05978035 -0.04493712
#[5,]  0.020585255 -0.04826704 -0.015035028 -0.03730746  0.05012218






