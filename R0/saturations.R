saturations <- function(AS, brG, grp, facteurs = NULL) {
  # AS la structure en ??volution, brG le rang de AS$VG qui contient la liste des grappes
  # grp est un triplet ou quatrain coplanaire
  # facteurs les rangs de ses deux grappes ?? garder comme facteurs
  # Si facteurs est absent, ce sont les rangs des deux grappes donn??s en positif
  # Calcule les saturations des v variables des grappes autres que les deux de fct
  # Dans une matrice satur(v+1,3) avec [0 fct] en rang??e 1 et [o var]' en colonne 1
  
  if (is.null(facteurs)) {
    facteurs <- which(grp > 0)
    grp <- abs(grp)
  }
  
  if (length(facteurs) != 2) {
    stop("Le param??tre 'facteurs' doit ??tre de longueur 2 OU le param??tre 'grp' ne doit avoir que 2 rangs positifs")
  }
  
  fct <- grp[facteurs]
  gb <- setdiff(grp, fct)  # grappes bifactorielles (2 grappes pour un quatrain)
  gb <- gb[gb != 0]
  
  bifac <- AS$VG[[brG]]$Gr[[gb[1]]]
  if (length(gb) > 1) {
    bifac <- c(bifac, AS$VG[[brG]]$Gr[[gb[2]]])
  }
  
  nv <- length(bifac)
  satur <- matrix(0, nrow = nv + 1, ncol = 3)
  satur[1, 2:3] <- facteurs
  satur[2:(nv + 1), 1] <- bifac
  
  pred <- expand.grid(AS$VG[[brG]]$Gr[[fct[2]]],AS$VG[[brG]]$Gr[[fct[1]]])[,2:1] 
  #pred <- croise(AS$VG[[brG]]$Gr[[fct[1]]], AS$VG[[brG]]$Gr[[fct[2]]])
  np <- nrow(pred)
  
  for (k in seq_len(nv)) {
    sat <- matrix(0, nrow = np, ncol = 2)
    
    for (p in seq_len(np)) {
      combine <- unlist(c(pred[p, ], bifac[k]))  # la grappe soup??onn??e ?? rendre bifactorielle est en dernier
      A <- asTuples(AS, combine) # POC : Ne donne pas le m??me r??sultats
      cx <- which(combine == bifac[k])  # rang de cible
      p1 <- combine[combine == pred[p, 1]]  # rang du premier pr??dicteur
      p2 <- combine[combine == pred[p, 2]]  # rang du deuxi??me pr??dicteur
      sdx <- setdiff(1:3, cx)
      
      st <- c(A$tmp$Poids, 1)
      st <- st / st[cx]
      
      if (cx < 3) {
        st[, 3 - cx] <- -st[, 3 - cx]
      }
      
      s1 <- AS$VG[[brG]]$Fct[p1, fct[1]]  # Saturation pred 1 sur son facteur
      s2 <- AS$VG[[brG]]$Fct[p2, fct[2]]  # Saturation pred 2 sur son facteur
      
      if (s1 + s2 == 0) {
        stop("Erreur : s1 ou s2 n'a pas ??t?? extrait correctement")
      }
      
      sat[p, ] <- st[sdx] * c(s1, s2) # POC : Est-ce toujours un vecteur
    }
    
    satur[k + 1, 2:3] <- colMeans(sat)
  }
  
  return(satur)
}