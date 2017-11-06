#' @title Get a raster from the coverage
#' @description Calculate raster layer(s) of the coverage
#' @param coverage name of the coverage [character]
#' @param coord_sys coordinate system [character]
#' @param slice_E image slicing coordinates in x-direction [character]
#' @param slice_N image slicing coordinates in y-direction [character]
#' @param date an available timestamp [character]
#' @param ref_Id EPSG code of the coordinate system [character]
#' @param res_eff factor to scale raster resolution [numeric]
#' @param format image format in WCPS query [character]
#' @param bands coverage bands to calculate raster [character]
#' @param pixel_url Web Coverage Service (WCS) for processing the query [character].
#' This URL can be built with the *createWCS_URLs* function
#' @import urltools
#' @import httr
#' @import raster
#' @import tiff
#' @import png
#' @import jpeg
#' @import sp
#' @export
image_from_coverage <- function(coverage, coord_sys, slice_E, slice_N, date, ref_Id=NULL, res_eff=NULL, format="Tiff", bands=NULL,
                                pixel_url=NULL){

  if(is.null(pixel_url)) pixel_url<-createWCS_URLs(type="Pixel")
  if(is.null(ref_Id))    ref_Id<-coverage_get_coordinate_reference(coverage = coverage)

  bands_len <- length(bands)
  rasters <- list()

  for(i in 1:bands_len){

    query <- paste0('for c in (', coverage, ') return encode (c.', bands[i],
                    '[',
                    coord_sys[1], '(', slice_E[1], ':', slice_E[2], ')', ',',
                    coord_sys[2], '(', slice_N[1], ':', slice_N[2], ')', ',',
                    coord_sys[3], '("', date, '")',
                    '],',
                    '"', format,'"',')')

    query_encode  <- urltools::url_encode(query)
    request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")

    res <- GET(request)
    bin <- content(res, "raw")
    to_img  <- get(paste0("read",format))
    img     <- to_img(bin,as.is = T)

    ras_ext <- extent(c(as.numeric(slice_E), as.numeric(slice_N)))
    ras     <-raster(img)
    proj4string(ras) <- CRS(paste0("+init=epsg:",ref_Id))
    extent(ras)      <- ras_ext

    if(!is.null(res_eff)){

      if(res_eff == 1){

        print(ras)
        rasters[[i]] <- ras

      } else {

        ras_aggregate <- aggregate(ras, fact=res_eff, expand = FALSE)
        print(ras_aggregate)
        rasters[[i]] <- ras_aggregate

      }

    }

  }

  return(rasters)

}
