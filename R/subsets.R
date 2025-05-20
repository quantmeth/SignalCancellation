subsets <- function(lst, mi = 1, ma = length(lst)) {
  n <- length(lst)
  if (n < 2) {
    return(list(lst))
  }
  
  lst <- sort(lst)
  SS <- unlist(
    lapply(mi:ma, function(k) {
      combn(lst, k, simplify = FALSE)
    }), 
    recursive = FALSE
  )

  return(SS)
}
