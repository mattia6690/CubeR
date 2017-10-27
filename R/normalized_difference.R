#' @title Calculate Normalized Difference Pixel History
#' @description Calculate normalized difference pixel history between band1 and band2
#' @param coverage name of the coverage [character]
#' @param coord_sys coordinate system [character]
#' @param coords coordinates of the location of interest [character]
#' @param band1 coverage band [character]
#' @param band2 coverage band [character]
#' @param date date range in format (Ymd) [character]
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character].
#' This URL can be built with the *createWCS_URLs* function
#' @param pixel_url Web Coverage Service (WCS) for processing the query [character].
#' This URL can be built with the *createWCS_URLs* function
#' @param plot handler if a plot is returned or a vector containing timestamp and value
#' @import magrittr
#' @import urltools
#' @import httr
#' @import stringr
#' @export

norm_diff_hist <- function(coverage, coord_sys, coords, band1, band2, date = NULL,
                           desc_url=NULL,pixel_url=NULL, plot = TRUE){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")
  if(is.null(pixel_url)) pixel_url<-createWCS_URLs(type="Pixel")
  if(is.null(band1) | is.null(band2)){

    p <- plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    p <- text(1,1, "Can't calculate normalized difference for selected coverage, reason: No band selected!", cex = 2)

  } else {

    times<-coverage_get_timestamps(desc_url,coverage)

    if(is.null(date)){

      times <- times %>% as.Date()

      query <- paste0('for c in (', coverage, ') return encode (',
                      '( (int) c.',
                      band1,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2], '(',coords[2],')]', '- c.',
                      band2,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2], '(',coords[2],')])','/',
                      '( (int) c.',
                      band1,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2], '(',coords[2],')]', '+ c.',
                      band2,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2], '(',coords[2],')]', '),',
                      '"csv"',')')
    } else {

      times <- times[times >= date[1] & times <= date[2]] %>% as.Date()

      query <- paste0('for c in (', coverage, ') return encode (( (int) c.',band1,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2],
                      '(',coords[2],'),', coord_sys[3], '("',date[1],'":"', date[2],'")',']', '- c.',band2,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2],
                      '(',coords[2],'),', coord_sys[3], '("',date[1],'":"', date[2],'")',']',')','/', '( (int) c.',band1,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2],
                      '(',coords[2],'),', coord_sys[3], '("',date[1],'":"', date[2],'")',']', '+', 'c.',band2,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2],
                      '(',coords[2],'),', coord_sys[3], '("',date[1],'":"', date[2],'")',']', '),', '"csv"',')')

    }


    query_encode  <- urltools::url_encode(query)
    request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")

    print(request)

    res <- GET(request)
    bin <- content(res,"text") %>%
      str_replace_all(.,"\\{","") %>%
      str_replace_all(.,"\\}","") %>%
      str_split(.,",") %>% unlist() %>%
      as.numeric()

    res <- bin

    out <- cbind(times, res)
  }

  if(plot == TRUE){
    p<-plot(times,res,type="o", lwd = 2, xlab="Date", ylab="Normalized difference", ylim = c(-1,1), cex.axis = 1.2, cex.lab = 1.2)
    p <-legend("topright", inset = .02,legend=paste0(band1," - ",band2), pch=15)
    p<-title(paste0("Normalized Difference between ", band1, " and ", band2))

    return(p)

  } else {

    return(out)

  }



}

#' @title Calculate Normalized Difference Raster Layer
#' @description Calculate a raster layer of normalized difference between band_1 and band_2
#' @param coverage name of the coverage [character]
#' @param coord_sys coordinate system [character]
#' @param slice_E image slicing coordinates in x-direction [character]
#' @param slice_N image slicing coordinates in y-direction [character]
#' @param date an available timestamp [character]
#' @param ref_Id EPSG code of the coordinate system [character]
#' @param res_eff factor to scale raster resolution [numeric]
#' @param format image format in WCPS query [character]
#' @param band_1 coverage band [character]
#' @param band_2 coverage band [character]
#' @import urltools
#' @import httr
#' @import raster
#' @import sp
#' @export

norm_diff_raster <- function(coverage, coord_sys, slice_E, slice_N, date, ref_Id, res_eff, format,
                             band_1, band_2, pixel_url = NULL){

  if(is.null(pixel_url)) pixel_url<-createWCS_URLs(type="Pixel")

  query = paste0('for c in (', coverage, ') ', 'return encode ((unsigned char)(127.5*(1+( (int) ',
                 'c.',band_1,'[',
                 coord_sys[1],'(',slice_E[1], ':',slice_E[2], '),',
                 coord_sys[2],'(',slice_N[1], ':',slice_N[2], '),',
                 coord_sys[3],'("',date,'")]',
                 '-',
                 'c.',band_2,'[',
                 coord_sys[1],'(',slice_E[1], ':',slice_E[2], '),',
                 coord_sys[2],'(',slice_N[1], ':',slice_N[2], '),',
                 coord_sys[3],'("',date,'")]',
                 ')/( (int) ',
                 'c.',band_1,'[',
                 coord_sys[1],'(',slice_E[1], ':',slice_E[2], '),',
                 coord_sys[2],'(',slice_N[1], ':',slice_N[2], '),',
                 coord_sys[3],'("',date,'")]',
                 '+',
                 'c.',band_2,'[',
                 coord_sys[1],'(',slice_E[1], ':',slice_E[2], '),',
                 coord_sys[2],'(',slice_N[1], ':',slice_N[2], '),',
                 coord_sys[3],'("',date,'")]',
                 '))),', '"', format, '")')

  query_encode  <- urltools::url_encode(query)
  request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")

  print(request)

  res <- GET(request)
  bin <- content(res, "raw")

  to_img <- get(paste0("read",toupper(format)))

  img <- to_img(bin)
  img <- (img*2) - 1

  ras_ext <- extent(c(as.numeric(slice_E), as.numeric(slice_N)))

  ras = raster(img)

  proj4string(ras) <- CRS(paste0("+init=epsg:",ref_Id))
  extent(ras) <- ras_ext

  if(res_eff == 1){

    print(ras)
    return(ras)

  } else {

    ras_aggregate <- aggregate(ras, fact=res_eff, expand = FALSE)

    print(ras_aggregate)

    return(ras_aggregate)

  }

}

