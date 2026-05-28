#' Title
#'
#' @param AS A SCFA object.
#' @param AB A pair c(i,j) to compare.
#'
#' @returns Alist.
#' @export
#' @importFrom stringr str_split str_locate
#' 
#' @examples
#' R <- Rnest::tabachnick_fidell2019
#' tf <- scfa(R, n = 175)
#' # Check for the pair 1 and 5
#' tf15 <- factor_test(AS = tf$AS, c(1,5))
factor_test <- function(AS, AB){
  
  out <- factor_exclusion(AS, AB)
  sortie <- rapport_exclusion(out)
  list(residual.cov = sortie$CR,
       partial.cor = sortie$R)
  
  #sortie$CR
  # tri inf = cov residual moyenne
  # tri up = prob
  
  #sortie$R
  #correlation partielle moyenne
}