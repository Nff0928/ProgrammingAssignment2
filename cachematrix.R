  ## Programación R, Tarea 2: Caché de la Inversa de una Matriz
  
  ## La inversión de matrices suele ser un cálculo costoso y puede resultar ventajoso 
  ## almacenar en caché la inversa de una matriz en lugar de calcularla repetidamente.
  ## El siguiente par de funciones almacenan en caché la inversa de una matriz.
  
  ## makeCacheMatrix: Esta función crea un objeto "matriz" especial que puede 
  ## almacenar en caché su inversa.
  
  makeCacheMatrix <- function(x = matrix()) {
    ## Inicializar la variable para almacenar la inversa
    inverse <- NULL
    
    ## Función para establecer la matriz
    set <- function(y) {
      x <<- y
      inverse <<- NULL  ## Limpiar la inversa almacenada cuando cambia la matriz
    }
    
    ## Función para obtener la matriz
    get <- function() x
    
    ## Función para establecer la inversa
    setInverse <- function(inv) inverse <<- inv
    
    ## Función para obtener la inversa
    getInverse <- function() inverse
    
    ## Devolver una lista con las funciones
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
  
  ## cacheSolve: Esta función calcula la inversa de la "matriz" especial devuelta 
  ## por makeCacheMatrix. Si la inversa ya ha sido calculada (y la matriz no ha 
  ## cambiado), entonces cacheSolve recupera la inversa de la caché.
  
  cacheSolve <- function(x, ...) {
    ## Intentar obtener la inversa del caché
    inverse <- x$getInverse()
    
    ## Si la inversa ya existe en el caché, devolverla
    if(!is.null(inverse)) {
      message("obteniendo datos almacenados en caché")
      return(inverse)
    }
    
    ## Si no existe en el caché, calcularla
    matrix_data <- x$get()
    inverse <- solve(matrix_data, ...)
    
    ## Almacenar la inversa calculada en el caché
    x$setInverse(inverse)
    
    ## Devolver la inversa
    inverse
  }
