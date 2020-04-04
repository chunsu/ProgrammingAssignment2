## Here are two functions when used in combination can cache the inverse of a matrix, i.e.
## the inverse of the "inputMatrix" is stored and is only ever calculed once. Please also
## look up the example under each function definition line. 




    ## makeCacheMatrix returns a tagged list of four functions which provide access to the input matrix and its inverse
    ## when input argument is a square matrix which can be inverted.
	makeCacheMatrix <- function(inputMatrix = matrix(, nrow=0, ncol=0)) {
        ## Example:
        ## matrix_1 <- makeCacheMatrix(matrix(sample.int(100,36), nrow=6, ncol=6))


        #check the input is a square matrix which can be inverted   
        if ( !is.matrix(inputMatrix) ) {
            stop( "argument inputMatrix is not a matrix" )
        } 

        if ( !nrow(inputMatrix) == ncol(inputMatrix) ) {
            stop( "matrix inputMatrix is not a squre matrix" )
        } 

        if ( det(inputMatrix) == 0 ) {
            stop( "a singular matrix cannot be inversed!" )
        } 


        cachedInverseMatrix <- NULL

        # This assigns the input argument, IN, to the variable inputMatrix in the enclosing scope.
        # It also sets cachedInverseMatrix to be null, indicating that the inverse of inputMatrix hasn't been calculated. 
        set <- function(IN) {
                inputMatrix <<- IN
                cachedInverseMatrix <<- NULL
            }

        get <- function() inputMatrix

        setInverse <- function(invert) cachedInverseMatrix <<- invert

        getInverse <- function() cachedInverseMatrix

        return(list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse))

    } 



    ## cacheSolve returns a matrix that is the inverse using the 'makeCacheMatrix' 
    ## function such that the inverse is calculated only once. 
    cacheSolve <- function(matrixCacher,...) {
        ## Inputs
        ## matrixCacher - a list returned by makeCacheMatrix
        ##
        ## Example:
        ## matrix_2 <- makeCacheMatrix(matrix(c(4,6,3,6),nrow=2,ncol=2))
        ## cacheSolve(matrix_2) # This calculates and returns the inverse
        ## cacheSolve(matrix_2) # This returns the inverse from the cache



        # Check matlib is installed and if so load it
        ip <- as.list(installed.packages()[,'Package'])
        if ( !any(ip == 'matlib') ) {
            message("Error:please install matlib package")
        } else {
            library(matlib)
        }

        ## Return a matrix that is the inverse of 'matrixCacher'    
        invertedMatrix <- matrixCacher$getInverse()

        if( !is.null(invertedMatrix) ) {
                message("getting cached data")
                return(invertedMatrix)
        }

        data <- matrixCacher$get()
        invertedMatrix <- inv(data, ...)
        matrixCacher$setInverse(invertedMatrix)


        return(invertedMatrix)
}


