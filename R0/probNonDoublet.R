probNonDoublet <- function(AS, melange) {
  # retourne une probabilité nette (ajustée selon le nombre de corrélations
  # dont le maximum en valeurs absolues est utilisé ici)
  
  var <- setdiff(AS$pertinent, melange)
  mc <- max(abs(AS$R[var, melange])) # corrélation maximale des deux variables de mélange avec toutes les autres pertinentes
  x2 <- mc^2 * (AS$N - 1)
  pr <- 1 - pchisq(x2, df = 1)^(2 * length(var))
  
  return(pr)
}
