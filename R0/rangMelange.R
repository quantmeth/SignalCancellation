rangMelange <- function(AS, melange) {
  # Vérifier si 'Tuples' existe dans AS
  # if (!"Tuples" %in% names(AS)) {
  #   return(NULL)
  # }
  
  # Encoder le mélange
  code <- encodeMelange(AS, melange)
  nm <- length(melange)
  
  # Vérifier si la taille de Tuples est suffisante
  if (length(AS$Tuples) < nm) {
    #AS$Tuples[[nm]] <- list(melange = 0) why is that?
    return(NULL)
  } else {
    return(which(AS$Tuples[[nm]]$melange == code))
  }
}