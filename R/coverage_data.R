#' @title Get a raster from coverage
#' @description This function provides the possibility to interact directly with the data cubes. It gives the option to write the images to
#' memory or on the hard drive for further computation.
#' @param coverage name of the coverage [character]
#' @param slice_E image slicing coordinates in x-direction [character]
#' @param slice_N image slicing coordinates in y-direction [character]
#' @param date an available timestamp [character]
#' @param res_eff factor to scale raster resolution [numeric]
#' @param format image format in WCPS query [character]
#' @param bands character; coverage bands to calculate raster. Can contain one or more bands from the same coverage.
#' If NULL all bands are used
#' @param filename If the raster image should be saved please digit a path and a filename. [character]
#' @param url Web Coverage Service (WCS) DescribeCoverage url [character].
#' If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom httr GET content
#' @importFrom tiff readTIFF
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom raster raster extent aggregate stack writeRaster
#' @importFrom urltools url_encode
#' @export
image_from_coverage <- function(coverage, slice_E, slice_N, date,
                                res_eff=1, format="image/tiff", bands=NULL,filename=NULL,
                                url=NULL){




  if(is.null(query_url)) query_url<-createWCS_URLs(url=url,type="Query")

  ref_Id<-coverage_get_coordinate_reference(coverage=coverage)
  coord_sys<-coverage_get_coordsys(coverage=coverage)

  gbands<-coverage_get_bands(coverage)
  if(is.null(bands)) bands<- gbands
  if(any(!is.element(gbands,bands))) stop("One or more bands you selected are not present in the selected coverage")

  bands_len <- length(bands)
  rasters <- list()

  for(i in 1:bands_len){

    query <- paste0('for c in (', coverage, ') return encode (c.', bands[i],
                    '[',
                    coord_sys[1], '(', slice_E[1], ':', slice_E[2], ')', ',',
                    coord_sys[2], '(', slice_N[1], ':', slice_N[2], ')', ',',
                    coord_sys[3], '("', date, '")',
                    '],"',format,'"',')')

    query_encode  <- urltools::url_encode(query)
    request       <- paste(query_url, query_encode, collapse = NULL, sep="")

    res     <- GET(request)
    bin     <- content(res, "raw")

    format2<-strsplit(format,"/")[[1]][2]
    to_img  <- get(paste0("read",toupper(format2)))
    img     <- tryCatch({to_img(bin)},error=function(e){message("This format cannot be read by the function")})

    ras_ext <- extent(c(as.numeric(slice_E), as.numeric(slice_N)))
    ras     <- raster(img)
    proj4string(ras) <- paste0("+init=epsg:",ref_Id)
    extent(ras)      <- ras_ext

    if(res_eff == 1){

      rasters[[i]] <- ras

    } else {

      ras_aggregate <- aggregate(ras, fact=res_eff, expand = FALSE)
      print(ras_aggregate)
      rasters[[i]] <- ras_aggregate
    }
  }

  if(!is.null(filename)){

    rasters<-stack(rasters)
    writeRaster(rasters,filename)

  }else{return(rasters)}
}
