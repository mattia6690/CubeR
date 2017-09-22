#' @title Convert RGB to Luminance
#' @description Convert RGB image to Luminance. Reduces the three R, G and B
#' matrices to a one dimensional matrix for weighted intensities
#' @param img RGB image; class: [array]; dim: [nrow, ncol, 3]
#' @return gray: grayscale image; class: [matrix]; dim: [nrow, ncol, 1]
#' @export

rgb2luminance <- function(img){

  # Compute luminance from RGB using matrix multiplication
  gray<- img[,,1]*0.299 + img[,,2]*0.587 + img[,,3]*0.114

  return(gray)

}
