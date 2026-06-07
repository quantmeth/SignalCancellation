prob_Rr <- function(R, N, no) {
  dl  <- N - no - 1
  RR  <- 1 - R * R
  res <- rep(NA_real_, length(R))

  manq <- is.na(RR)
  neg  <- !manq & RR < 0
  ok   <- !manq & RR >= 0

  if (any(manq)) {
    warning("prob_Rr: ", sum(manq), " corr\u00E9lation(s) NA re\u00E7ue(s) - p-valeur(s) fix\u00E9e(s) \u00E0 NA.")
  }

  res[ok] <- 2 * pt(-abs(R[ok] * sqrt(dl / RR[ok])), dl)
  if (any(neg)) {
    cat("prob_Rr: \u007Cr\u007C > 1 pour", sum(neg), "valeur(s) :",
        round(R[neg], 3), "\n")
    res[neg] <- 99.999  # artefact d'estimation — non significatif mais visible
  }

  # res reste NA là où R est NA
  return(res)
}
