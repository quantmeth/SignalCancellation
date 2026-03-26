encodeMelange <- function(AS, melange) {
  nm <- length(melange)
  p <- AS$nv^(0:(nm-1))
  code <- sum(p * melange)
  return(code)
}