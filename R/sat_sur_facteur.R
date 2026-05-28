sat_sur_facteur <- function(R, N, AB, seuil = 0.05) {
  # Estime les saturations factorielles de toutes les variables à partir
  # d'une paire (A, B) supposée exclusive au même facteur.
  #
  # Arguments :
  #   R     : matrice de corrélations (nv × nv)
  #   N     : taille d'échantillon
  #   AB    : vecteur de 2 indices — A = AB[1], B = AB[2]
  #   seuil : seuil de significativité pour le filtre des dénominateurs
  #
  # Retourne : list(sat, a_hat, b_hat, a_vec, b_vec, r_crit, n_valid_a, n_valid_b, details)
  #   sat    : vecteur de longueur nv — saturations estimées pour toutes les variables
  #            (NA pour A et B si on veut les distinguer — ici on y met a_hat et b_hat)
  #   details : data.frame des valeurs intermédiaires pour les variables hors AB
  ia     <- AB[1]
  ib     <- AB[2]
  nv     <- nrow(R)
  autres <- setdiff(seq_len(nv), AB)

  # Valeur critique de |r| — calculée une seule fois pour ce N
  t_crit <- qt(1 - seuil / 2, df = N - 2)
  r_crit <- t_crit / sqrt(t_crit^2 + N - 2)
  r_AB <- R[ia, ib]
  r_AX <- R[ia, autres]
  r_BX <- R[ib, autres]

  sig_AX <- abs(r_AX) >= r_crit
  sig_BX <- abs(r_BX) >= r_crit

  # Condition de signe : r(A,B)*r(A,X)*r(B,X) > 0 garantit a²>0 et b²>0
  signe_ok <- r_AB * r_AX * r_BX > 0
  # a² = r(A,B)*r(A,X)/r(B,X)  — valide si r(B,X) significatif et signe OK
  ok_a2  <- sig_BX & signe_ok
  a2_est <- ifelse(ok_a2, r_AB * r_AX / r_BX, NA_real_)

  # b² = r(A,B)*r(B,X)/r(A,X)  — valide si r(A,X) significatif et signe OK
  ok_b2  <- sig_AX & signe_ok
  b2_est <- ifelse(ok_b2, r_AB * r_BX / r_AX, NA_real_)
  
  # Saturations individuelles — convention : sign(a) = sign(r_AB), b ≥ 0
  a_vec <- sign(r_AB) * sqrt(a2_est[!is.na(a2_est)])
  b_vec <- sqrt(b2_est[!is.na(b2_est)])

  a_hat <- if (length(a_vec) > 0) mean(a_vec) else NA_real_
  b_hat <- if (length(b_vec) > 0) mean(b_vec) else NA_real_

  # Repli 1 : r_AB = a*b — si un seul est manquant, l'autre se déduit exactement
  if ( is.na(a_hat) && !is.na(b_hat) && b_hat != 0) a_hat <- r_AB / b_hat
  if (!is.na(a_hat) &&  is.na(b_hat) && a_hat != 0) b_hat <- r_AB / a_hat
  # Repli 2 : les deux manquent — annulation du signal sur la paire (ia, ib)
  # Reproduit la logique de optim_paire_initiale sans ses browser()
  if (is.na(a_hat) || is.na(b_hat)) {
    GS_tmp  <- chol(R)
    autres2 <- seq_len(nrow(R))[-c(ia, ib)]
    col2    <- GS_tmp[, c(ia, ib)]
    TEM2    <- GS_tmp[, autres2]
    Rinv2   <- solve(t(TEM2) %*% TEM2)
    out2    <- optim(1, crit_R2, gr = NULL, col2, TEM2, Rinv2, method = "BFGS")
    p       <- out2$par
    if (is.finite(p) && r_AB * p > 0) {
      a_hat <- sqrt(r_AB * p)
      b_hat <- r_AB / a_hat
    } else {
      a_hat <- b_hat <- sqrt(max(r_AB, 0))
    }
  }

  # x² = r(A,X)*r(B,X) / (a_hat*b_hat)  [remplace r(A,B) par le produit des moyennes]
  # signe de x : sign(r(A,X)) * sign(a_hat)
  # x estimé à 0 si :
  #   - aucune corrélation significative, OU
  #   - une seule significative mais r(A,X) et r(B,X) de signes opposés
  #     (contredit l'hypothèse d'un signal de X sur le facteur)
  #   - a_hat * b_hat == 0 (pas de facteur commun extractible — évite division par zéro)
  zero_x <- (!sig_AX & !sig_BX) | (xor(sig_AX, sig_BX) & r_AX * r_BX < 0)
  if (isTRUE(a_hat * b_hat == 0)) {
    x_est <- rep(0, length(autres))
  } else {
    x2_est <- r_AX * r_BX / (a_hat * b_hat)
    x_est  <- ifelse(zero_x, 0,
                     sqrt(pmax(x2_est, 0)) * sign(r_AX) * sign(a_hat))
  }

  # Vecteur complet des saturations (toutes variables)
  sat        <- numeric(nv)
  sat[ia]    <- a_hat
  sat[ib]    <- b_hat
  sat[autres] <- x_est

  details <- data.frame(
    var    = autres,
    r_AX   = round(r_AX,  4),
    r_BX   = round(r_BX,  4),
    sig_AX = sig_AX,
    sig_BX = sig_BX,
    ok_a2  = ok_a2,
    ok_b2  = ok_b2,
    a2_est = round(a2_est, 4),
    b2_est = round(b2_est, 4),
    x_est  = round(x_est, 4),
    x_zero = zero_x
  )

  list(
    sat       = sat,
    a_hat     = a_hat,
    b_hat     = b_hat,
    a_vec     = a_vec,
    b_vec     = b_vec,
    r_crit    = r_crit,
    n_valid_a = sum(!is.na(a2_est)),
    n_valid_b = sum(!is.na(b2_est)),
    details   = details
  )
}
