#' @title Calculate Normalized Difference Pixel History
#' @description Calculate normalized difference pixel history between band1 and band2
#' @param coverage character; name of the coverage
#' @param coords character; coordinates of the location of interest
#' @param band1 character; coverage band
#' @param band2 character; coverage band
#' @param date character(s); date with one or date range range with two dates. If NULL all the available time range is returned.
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @param format character; Output format. Not all available are implemented for each datacube. Please see `getFormat` for all the available formats
#' @importFrom httr GET content
#' @importFrom stringr str_replace_all
#' @importFrom urltools url_encode
#' @importFrom magrittr "%>%"
#' @importFrom dplyr distinct
#' @export

norm_diff_pixel <- function(coverage, coords, band1, band2, date = NULL,
                           url=NULL,format="text/csv"){

  # Build queries
  desc_url <-createWCS_URLs(type="Meta",url=url)
  query_url<-createWCS_URLs(type="Query",url=url)

  # Catch Exceptions
  if(is.null(band1) | is.null(band2)) stop("Can't calculate normalized difference for selected coverage, reason: No band selected!")

  # Use buildin functions
  coord_sys <- coverage_get_coordsys(coverage=coverage,url=url)
  times     <- coverage_get_temporal_extent(coverage,url=url)

  # Do the querying
  csys1<-paste0(coord_sys[1],'(',round(coords[1]),')')
  csys2<-paste0(coord_sys[2],'(',round(coords[2]),')')

  if(is.null(date))   date2<-c(times[1],times[2])
  if(length(date)==1) date2<-rep(date,2)
  if(length(date)>1)  date2<-c(date[1],date[2])

  csys3 <-paste0(coord_sys[3],'("',date2[1],'":"', date2[2],'")',sep="")
  paster<-paste(csys1,csys2,csys3,sep=",")
  paster<-paste0('[',paster,']')

  query <- paste0('for c in (', coverage, ') return encode((',
                  '( (int) c.',band1, paster,
                  '-',
                  'c.',band2, paster,')',
                  '/',
                  '( (int) c.',band1, paster,
                  '+',
                  'c.', band2, paster, ')),'
                  ,'"',format,'")')

  query_encode  <- urltools::url_encode(query)
  request       <- paste(query_url, query_encode, collapse = NULL, sep="")


  # Return the response
  res <- GET(request)
  bin <- suppressMessages(
    content(res,"text") %>%
    str_replace_all(.,"\\{","") %>%
    str_replace_all(.,"\\}","") %>%
    strsplit(.,",") %>% unlist() %>%
    as.numeric())

  res <- bin
  out <- cbind.data.frame(times, res)
  out <- dplyr::distinct(out)
  return(out)

}

#' @title Calculate Normalized Difference Raster Layer
#' @description Calculate a raster layer of normalized difference between band_1 and band_2
#' @param coverage character; name of the coverage
#' @param slice_E character; image slicing coordinates in x-direction
#' @param slice_N character; image slicing coordinates in y-direction
#' @param date character; an available timestamp
#' @param band1 character; coverage band
#' @param band2 character; coverage band
#' @param res_eff numeric; factor to scale raster resolution
#' @param format character; Output format. Not all available are implemented for each datacube. Please see `getFormat` for all the available formats
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom httr GET content
#' @importFrom raster raster extent aggregate
#' @importFrom urltools url_encode
#' @export

norm_diff_raster <- function(coverage, slice_E, slice_N, date, band1, band2,
                             res_eff=1,format="image/tiff", url = NULL){

  # Build urls
  query_url<-createWCS_URLs(type="Query",url=url)

  # Catch Exceptions
  if(length(date)>1) stop("No multiple dates supported")

  # Use build-in functions
  coord_sys <-coverage_get_coordsys(coverage=coverage,url = url)
  ref_Id    <-coverage_get_coordinate_reference(coverage=coverage,url = url)

  # Build request
  query<- paste0('for c in (', coverage, ') ', 'return encode ((unsigned char)(127.5*(1+( (int) ',
                 'c.',band1,'[',
                 coord_sys[1],'(',slice_E[1], ':',slice_E[2], '),',
                 coord_sys[2],'(',slice_N[1], ':',slice_N[2], '),',
                 coord_sys[3],'("',date,'")]',
                 '-',
                 'c.',band2,'[',
                 coord_sys[1],'(',slice_E[1], ':',slice_E[2], '),',
                 coord_sys[2],'(',slice_N[1], ':',slice_N[2], '),',
                 coord_sys[3],'("',date,'")]',
                 ')/( (int) ',
                 'c.',band1,'[',
                 coord_sys[1],'(',slice_E[1], ':',slice_E[2], '),',
                 coord_sys[2],'(',slice_N[1], ':',slice_N[2], '),',
                 coord_sys[3],'("',date,'")]',
                 '+',
                 'c.',band2,'[',
                 coord_sys[1],'(',slice_E[1], ':',slice_E[2], '),',
                 coord_sys[2],'(',slice_N[1], ':',slice_N[2], '),',
                 coord_sys[3],'("',date,'")]',
                 '))),"',format,'")')


  query_encode  <- urltools::url_encode(query)
  request       <- paste(query_url, query_encode, collapse = NULL, sep="")

  # Handle request
  res <- GET(request)
  bin <- content(res, "raw")

  format2<- strsplit(format,"/")[[1]][2]
  to_img <- get(paste0("read",toupper(format)))
  img    <- suppressWarnings(to_img(bin))

  ras_ext <- extent(c(as.numeric(slice_E), as.numeric(slice_N)))
  ras     <- raster(img)
  proj4string(ras) <- paste0("+init=epsg:",ref_Id)
  extent(ras) <- ras_ext

  # Optional resampling
  if(res_eff == 1){

    return(ras)

  } else {

    ras_aggregate <- aggregate(ras, fact=res_eff, expand = FALSE)
    return(ras_aggregate)

  }
}

