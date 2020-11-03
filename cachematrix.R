# Caching the Inverse of a Matrix:
## La inversión de matrices suele ser un cálculo costoso y puede haber algún beneficio de
## almacenar en caché la inversa de una matriz en lugar de calcularla repetidamente 
## (también hay alternativas a la inversión de matrices que no discutiremos aquí). 
## Su tarea es escribir un par de funciones que almacenan en caché la inversa de una matriz.
## Esta función crea un objeto "matriz" especial que puede almacenar en caché su inverso.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Esta función calcula la inversa de la "matriz" especial devuelta por
## makeCacheMatrix anterior. Si la inversa ya se ha calculado (y la matriz
## no ha cambiado), cacheSolve debería recuperar la inversa de la caché.
cacheSolve <- function(x, ...) {
  ## DEvuelve la matriz que insersa de "x"
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
