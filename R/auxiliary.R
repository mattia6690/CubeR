#' @title Convert RGB to Greascale
#' @description Convert RGB image [array] to grayscale (luminance) [matrix]
#' @param img RGB image; class: [array]; dim: [nrow, ncol, 3]
#' @return gray: grayscale image; class: [matrix]; dim: [nrow, ncol, 1]
#' @export

rgb2gray <- function(img){

  # Compute luminance from RGB using matrix multiplication
  gray<- img[,,1]*0.299 + img[,,2]*0.587 + img[,,3]*0.114

  return(gray)

}
