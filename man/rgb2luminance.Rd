% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary.R
\name{rgb2luminance}
\alias{rgb2luminance}
\title{Convert RGB to Luminance}
\usage{
rgb2luminance(img)
}
\arguments{
\item{img}{RGB image; class: [array]; dim: [nrow, ncol, 3]}
}
\value{
gray: grayscale image; class: [matrix]; dim: [nrow, ncol, 1]
}
\description{
Convert RGB image to Luminance. Reduces the three R, G and B
matrices to a one dimensional matrix for weighted intensities
}
