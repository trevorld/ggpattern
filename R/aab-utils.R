#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Combine two aes() mappings
#'
#' @param a1,a2 the two aes mappings or lists
#' @return combined aes mapping
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
augment_aes <- function(a1, a2) {
  a3 <- utils::modifyList(a1, a2, keep.null = TRUE)
  do.call(aes, a3)
}
