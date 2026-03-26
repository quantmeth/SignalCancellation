#' rotation
#'
#' @param vect a vector.
#'
#' @returns a vector.
#' @export
#' 
#' @author André Achim
rotation <- function(vect){
  n <- length(vect)
  rang <- ((1:n) %% n)+1
  return(vect[rang])
}