ordonneTrio <- function(AS, brG, trio, ...) {
  # tri <- ordonneTrio(AS, brG, trio, ...)
  # fonction créée le 2 mars 2025
  # trio est un trio de rangs de GRAPPES et non de variables, sauf si un
  # quelconque 4e argument est fourni
  # met en 3e l'élément du trio qui semble entre les deux autres pour
  # faciliter l'annulation
  
  ordre <- matrix(c(1, 2, 3, 1, 3, 2, 2, 3, 1), nrow = 3, byrow = TRUE)
  
  if (length(list(...)) > 0) {
    tri <- asGrappeDe(AS, brG, trio)
    if (length(tri) < length(trio)) {  # si une variable pas dans une grappe
      tri <- trio
      return(tri)
    }
  } else {
    tri <- trio
  }
  
  co <- abs(AS$VG[[brG]]$CorFct[tri, tri])
  co <- co + t(co)
  c <- co[c(4, 7, 8)]  # les trois corrélations de trois grappes
  f <- which.min(c)
  tri <- trio[ordre[f,]]
  
  return(tri)
}