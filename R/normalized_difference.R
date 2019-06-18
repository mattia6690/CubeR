#' @title Calculate Normalized Difference Pixel History
#' @description Calculate normalized difference pixel history between band1 and band2
#' @param coverage name of the coverage [character]
#' @param coords coordinates of the location of interest [character]
#' @param band1 coverage band [character]
#' @param band2 coverage band [character]
#' @param date date range in format (Ymd) [character]
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character].
#' This URL can be built with the *createWCS_URLs* function
#' @param query_url Web Coverage Service (WCS) for processing the query [character].
#' This URL can be built with the *createWCS_URLs* function
#' @param plot handler if a plot is returned or a vector containing timestamp and value
#' @import httr
#' @import stringr
#' @import ggplot2
#' @importFrom urltools url_encode
#' @importFrom magrittr "%>%"
#' @importFrom dplyr distinct
#' @export

norm_diff_pixel <- function(coverage, coords, band1, band2, date = NULL,
                           desc_url=NULL, query_url=NULL, plot = TRUE){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")
  if(is.null(query_url)) query_url<-createWCS_URLs(type="Query")

  coord_sys<-coverage_get_coordsys(coverage=coverage)

  if(is.null(band1) | is.null(band2)) stop("Can't calculate normalized difference for selected coverage, reason: No band selected!")

  times<-coverage_get_timestamps(desc_url,coverage)

  csys1<-paste0(coord_sys[1],'(',round(coords[1]),')')
  csys2<-paste0(coord_sys[2],'(',round(coords[2]),')')

  if(is.null(date))   date2<-c(min(times),max(times))
  if(length(date)==1) date2<-rep(date,2)
  if(length(date)>1)  date2<-c(min(date),max(date))

  csys3<-paste0(coord_sys[3],'("',date2[1],'":"', date2[2],'")',sep="")
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
                  ,'"text/csv")')

  query_encode  <- urltools::url_encode(query)
  request       <- paste(query_url, query_encode, collapse = NULL, sep="")

  res <- GET(request)
  bin <- suppressMessages(
    content(res,"text") %>%
    str_replace_all(.,"\\{","") %>%
    str_replace_all(.,"\\}","") %>%
    str_split(.,",") %>% unlist() %>%
    as.numeric())

  res <- bin
  out <- cbind.data.frame(times, res)
  out <- dplyr::distinct(out)

  if(plot == TRUE){

    p<-ggplot(out,aes(as.Date(times),res))+geom_point()+geom_line()+
      ggtitle(paste("Normalized Difference of", band1, ",", band2,". Coverage:",coverage))+
      ylab("Normalized Difference")+ xlab("Date")

    return(p)

  } else return(out)
}

#' @title Calculate Normalized Difference Raster Layer
#' @description Calculate a raster layer of normalized difference between band_1 and band_2
#' @param coverage name of the coverage [character]
#' @param slice_E image slicing coordinates in x-direction [character]
#' @param slice_N image slicing coordinates in y-direction [character]
#' @param date an available timestamp [character]
#' @param band1 coverage band [character]
#' @param band2 coverage band [character]
#' @param res_eff factor to scale raster resolution [numeric]
#' @param format image format in WCPS query [character]
#' @param query_url Web Coverage Service (WCS) for processing the query [character].
#' This URL can be built with the *createWCS_URLs* function
#' @import httr
#' @import raster
#' @import sp
#' @importFrom urltools url_encode
#' @export

norm_diff_raster <- function(coverage, slice_E, slice_N, date, band1, band2,
                             res_eff=1, format="TIFF", query_url = NULL){

  if(is.null(query_url)) query_url<-createWCS_URLs(type="Query")

  if(length(date)>1) stop("No multiple dates supported")

  coord_sys <-coverage_get_coordsys(coverage=coverage)
  ref_Id    <-coverage_get_coordinate_reference(coverage=coverage)

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
                 '))),', '"image/',tolower(format),'")')


  query_encode  <- urltools::url_encode(query)
  request       <- paste(query_url, query_encode, collapse = NULL, sep="")

  res <- GET(request)
  bin <- content(res, "raw")

  to_img <- get(paste0("read",toupper(format)))
  img <- suppressWarnings(to_img(bin))

  ras_ext <- extent(c(as.numeric(slice_E), as.numeric(slice_N)))
  ras <- raster(img)

  proj4string(ras) <- CRS(paste0("+init=epsg:",ref_Id))
  extent(ras) <- ras_ext

  if(res_eff == 1){

    return(ras)

  } else {

    ras_aggregate <- aggregate(ras, fact=res_eff, expand = FALSE)
    return(ras_aggregate)

  }
}

