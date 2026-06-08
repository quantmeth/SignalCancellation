Stouffer <- function(probs){
  z <- qnorm(probs)
  pr <- pnorm(sum(z)/sqrt(length(z)));
  return(pr)
}