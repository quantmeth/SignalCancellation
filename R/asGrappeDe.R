asGrappeDe <- function(AS, brG, var) {
  # gr <- asGrappeDe(AS, brG, var)
  # retourne dans gr le(s) rang(s) de grappes qui conti(enn)ent une des
  # variables de var
  
  gr <- c()
  Gr <- AS$VG[[brG]]$Gr
  
  for (v in var) {
    for (g in seq_along(Gr)) {
      if (v %in% Gr[[g]]) {
        gr <- c(gr, g)
      }
    }
  }
  
  gr <- unique(gr)  # Élimine les doublons
  
  return(gr)
}