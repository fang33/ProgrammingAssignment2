## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object. The object can store a matrix class
## The matrix objectd can also hold inversion of the matrix variable

makeCacheMatrix <- function(MatrixOutput = matrix()){
     inv <- NULL
     set <- function (MatrixInput){
          if (!is.matrix(MatrixInput)) stop ("Input Must be a Matrix")
          MatrixOutput <<- MatrixInput
          inv <<- NULL
     }
     get <- function() MatrixOutput
     setInv <- function (matrixInverse) inv <<- matrixInverse
     getInv <- function () inv
     list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function input arguement is a matrix object.  Function determines if the matrix object has the inverted matrix stored
## If inverted matrix already exists, the cacheSolve function will output the inverted matrix
## If inverted matrix does not exist in the matrix object, the cacheSolve function will calculate and store the inversion

cacheSolve <- function (x, ...){
     inv <- x$getInv()
     if (!is.null(x$getInv())){
          message ("getting matrix cached data ...")
          return (inv)
     }
     matrixData <- x$get()
     inv <- solve(matrixData)
     x$setInv (inv)
     inv
}
