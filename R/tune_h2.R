tune_h2 <- function(AS, tuple,
                    mults  = seq(0.95, 1.05, by = 0.01),
                    seuil  = 0.001,
                    zone   = c(0.03, 0.05),
                    dirs_seulement = TRUE,
                    h2_base = NULL) {
  # Descente par coordonnées discrètes sur les h2, déclenchée seulement
  # quand la prob de Stouffer tombe dans la zone d'ambiguïté [zone[1], zone[2]].
  # Par défaut, ajuste uniquement les h2 des variables-directions (±5%).
  #
  # Retourne : list(h2, h2_base, ratio, prob, details)
  #   h2       : h2 ajustés (= h2_base si aucun ajustement utile)
  #   h2_base  : estimés initiaux de fareg
  #   ratio    : h2 / h2_base (vecteur des multiplicateurs retenus)
  #   prob     : prob de Stouffer finale
  #   details  : matrice de détails d'optim_base au résultat final

  k <- length(tuple)
  if (is.null(h2_base)) h2_base <- fareg(AS$R, k)$h2
  h2_cur  <- h2_base

  res_cur  <- optim_base(AS, tuple, h2 = h2_cur)
  prob_cur <- res_cur$prob

  retour_neutre <- list(h2 = h2_cur, h2_base = h2_base,
                        ratio = rep(1, AS$nv), prob = prob_cur,
                        prob_base = prob_cur, ajuste = FALSE,
                        details = res_cur$details)
  if (prob_cur > zone[2]) {
    message(sprintf(
      "Prob initiale = %.4f > %.2f : conclusion d\u00E9j\u00E0 positive, ajustement inutile.",
      prob_cur, zone[2]))
    return(invisible(retour_neutre))
  }
  if (prob_cur < zone[1]) {
    message(sprintf(
      "Prob initiale = %.4f < %.2f : trop loin du seuil, ajustement peu utile.",
      prob_cur, zone[1]))
    return(invisible(retour_neutre))
  }

  cat(sprintf("Prob initiale : %.4f  [zone d'ambig\u00EFt\u00E9 : %.2f - %.2f]\n",
              prob_cur, zone[1], zone[2]))

  prob_init  <- prob_cur   # conservé pour prob_base dans le retour
  vars_ajust <- if (dirs_seulement) tuple else seq_len(AS$nv)

  iter <- 0
  repeat {
    iter      <- iter + 1
    best_gain <- 0
    best_j    <- NA
    best_m    <- NA
    best_prob <- prob_cur
    best_res  <- res_cur

    for (j in vars_ajust) {
      for (m in mults) {
        h2_trial    <- h2_cur
        h2_trial[j] <- h2_base[j] * m
        if (isTRUE(all.equal(h2_trial, h2_cur))) next
        r    <- optim_base(AS, tuple, h2 = h2_trial)
        gain <- r$prob - prob_cur
        if (gain > best_gain) {
          best_gain <- gain
          best_j    <- j
          best_m    <- m
          best_prob <- r$prob
          best_res  <- r
        }
      }
    }

    if (best_gain < seuil || is.na(best_j)) break

    h2_cur[best_j] <- h2_base[best_j] * best_m
    prob_cur       <- best_prob
    res_cur        <- best_res
  }

  # Rapport
  modif <- which(round(h2_cur / h2_base, 8) != 1)
  if (length(modif) == 0) {
    cat("Aucun ajustement utile trouv\u00E9 dans la grille.\n")
  } else {
    cat(sprintf("Prob finale : %.4f\n", prob_cur))
    cat("Ajustements appliqu\u00E9s :\n")
    for (j in modif)
      cat(sprintf("  var %d : h2 %.4f \u2192 %.4f  (\u00D7%.2f)\n",
                  j, h2_base[j], h2_cur[j], h2_cur[j] / h2_base[j]))
    if (prob_cur >= 0.05)
      cat(sprintf("  \u2192 prob \u2265 .05 : le mod\u00E8le  k=%d dimensions devient acceptable.\n", k))
    else
      cat(sprintf("  \u2192 prob reste < .05 : le mod\u00E8le \u00E0 k=%d reste rejet\u00E9 malgr\u00E9 les ajustements.\n", k))
  }

  list(h2       = h2_cur,
       h2_base  = h2_base,
       ratio    = h2_cur / h2_base,
       prob     = prob_cur,
       prob_base = prob_init,     # prob avant tout ajustement
       ajuste   = length(which(round(h2_cur / h2_base, 8) != 1)) > 0,
       details  = res_cur$details)
}
