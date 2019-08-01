
"Together this pair of functions will (1) save a matrix to the cache
with a set of functions to be performed on such matrix (makeCacheMatrix),
and (2) calculate the inverse of such matrix if one has not been stored
to the cached or, if the inverse has been stored to the cache, will
retreive the inverse from the cache without having to calculate it again,
saving processing time."


"arg-an invertible matrix (a non-invertible matrix will throw an error
    if called as an argument to cacheSolve)
returns-a list of functions with respect to the matrix passed as an
    argument which cacheSolve will call to find the inverse of the matrix
stores-the matrix in the cache and a corresponding inv variable initially
    set to null
note-to use this function in combination with cacheSolve, assign
    makeCacheMatrix(the subject matrix) to a new variable (ex: 'mcmvar') which will
    be called as the argument to cacheSolve
to call any function in the list on the matrix, write mcmvar$'func'"

makeCacheMatrix <- function(x = matrix()) {#arg is the subject matrix
    inv<-NULL#sets the matrx's inverse variable to null, as it is not yet calculated
    setmat<-function(y){
        x<<-y
        inv<<-inv
    }
    setmat(x) #this assigns the matrix and null inv to the cache, though setmat can be called again to assign a new matrix to the function variable (mcmvar)
    getmat<-function()x #can be used to call the matrix from the cache as needed
    setinv<-function(newinv) inv<<-newinv #will be used by cacheSolve function to save newly caluclated inv to cache
    getinv<-function()inv#will be used by cacheSolve function to load inv from cache if exists 
    list(setmat=setmat,getmat=getmat,setinv=setinv,getinv=getinv) #list of functions returned by function
}


"arg-a variable (mcmvar) containing makeCacheMatrix with an invertible matrix
    assigned as an argument (see instructions to makeCacheMatrix)
 returns-if the inverse has already been calculated and stored to the
    cache, such inverse without having to calculate it. if the
    inverse has not been calculated, the function will do so and save
    it to cache for future reference
 stores-the inverse to the cache
"

cacheSolve <- function(x, ...) {#pass mcmvar as an argument
    inv <- x$getinv() #gets inv value  from cache
    if(!is.null(inv)) {#tests if there is already an inv value calculated
        message("getting cached data") #if so, message console and
        return(inv)#return the cached inverse
    }
    else{#if the inv variable is null because it has not yet been calculated
        data <- x$getmat()#retreive the matrix from the cache
        newinv <- solve(data, ...)#solve for the matrix inverse
        x$setinv(newinv)#save the inverse to the cache for future reference
        return(newinv)#return the inverse
    }
}
