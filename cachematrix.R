makeCacheMatrix <- function(x = matrix()) {
## 
##
## initialize inver locally to NULL - will use this when cacheinverse function
## is called 

        inver <- NULL  

## Ok, now we set four methods.  Functions which allow us to manipulate 
## the object created when we call makeMatrix on a Matrix.
## set allows us to give a new matrix to the object.  We can overwrite 
## the orginal matrix that was used when makeMatrix was first called
## get returns the current matrix
## setinverse lets us assign an inverse matrix to the Object rather than
## have it calculate getinverse allows us to call the current inverse
## in the object.
## if we have a matrix m1 and call makeMatrix we create a new object m1_cache 
## by >m1_cache <- makeMatrix(m1)
## we can then call any of these functions by using m1_cashe$getinverse()
## if we have a new matrix m2 we can overwrite the current matrix in m1_cache 
## by writing >m1_cache$set(m2) and so on


## set swaps in new matrix for whatever we used the first time this
## function was evoked.  Replaces the current matrix with a new one
## sets parent enviornment  x to what has been passed in and inver is set
## to null in parent enviornment
## when we set the matrix the inverse is no longer correct, so it is set 
## to null until we call cacheinverse

        set <- function(y) {  
                x <<- y  
                inver <<- NULL  

        }
        
## get returns current matrix
        
        get <- function() x 
        
## setinverse assignes a new value to inver, again in parent enviornment
## - it is set not calculated.  

        setinverse <- function(inverse) inver <<- inverse
        
## getinverse just returns current inverse
        
        getinverse <- function() inver

## This is a list used by the function to tell what is available in the object
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## We use this function to actually get the inverse of the matrix for the
## object the first time
## I honestly don't like this.  I if I was making an object that controlled 
## the inverse of a matrix I would make the call to the function solve in 
## the object. I.E In the makeCache I would check and see if the inverse was 
## set when getinverse was called.  
## If it hadn't been set I would make the call to the function solve then. 
## If it had been set I would return it.  Basically the stuff below we be 
## in the function above  But I am following the example from class

## when cacheinverse is called the name of of the object created by makeCache 
## above 
## is passed in as an argument and is assigned to x

cacheSolve <- function(x, ...) {

## this is like calling m1_cashe$getinverse() described above.
        
        inver <- x$getinverse()
        
## as long as the inverse has already been created it prints this message
## and returns inver
        
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        
## If the call to x$getinverse() is null 
## (i.e this function has never been called)
## this next part called to calculate the inverse of the matrix that is going 
## to be cached. The function get in the makeCache object is called , 
## like m1_cashe$get() 
        
        data <- x$get()

##calls solve to  get the inverse        

        inver <- solve(data, ...)

## and then sets it using the setinverse funtion from the function above

        x$setinverse(inver)
        
## and returns it        
        
        inver
}
