#' @title Calculate Pixel Buffer History
#' @description Calculate pixel buffer history of a coverage band
#' @param coverage name of the coverage [character]
#' @param coords coordinates of the location of interest [character]
#' @param band coverage band [character]
#' @param buffer A buffer Zone around the coordinates of the Point [numeric]
#' @param date date range in format (Ymd) [character]
#' @param filename filename for the NETCDF Output.
#' If none is provided the Object will be deleted after temporal storage in the tempdir [character]
#' @param query_url Web Coverage Service (WCS) for processing the query [character].
#' This URL can be built with the *createWCS_URLs* function
#' @param plot do you want a generic plot to be returned [boolean]
#' @import ncdf4
#' @import httr
#' @import stringr
#' @import graphics
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom magrittr "%>%"
#' @importFrom urltools url_encode
#' @importFrom grDevices gray.colors
#' @export

geocoded_pixel_buffer <- function(coverage, coords, band, buffer, date = NULL,
                                  filename="request",query_url=NULL,plot=F){

  if(is.null(query_url)) query_url<-createWCS_URLs(type="Query")

  coord_sys<-coverage_get_coordsys(coverage=coverage)

  bounding_box <- round(c(as.numeric(coords[1]) - as.numeric(buffer),
                          as.numeric(coords[1]) + as.numeric(buffer),
                          as.numeric(coords[2]) - as.numeric(buffer),
                          as.numeric(coords[2]) + as.numeric(buffer)))

  if(is.null(date)){

    query <- paste0('for c in (', coverage, ') return encode (c.', band,
                    '[',
                    coord_sys[1], '(', bounding_box[1]-1, ':', bounding_box[2], '),',
                    coord_sys[2], '(', bounding_box[3]-1, ':', bounding_box[4], ')',
                    '], "netcdf")')

  } else {

    query <- paste0('for c in (', coverage, ') return encode (c.', band,
                    '[',
                    coord_sys[1], '(', bounding_box[1]-1, ':', bounding_box[2], '),',
                    coord_sys[2], '(', bounding_box[3]-1, ':', bounding_box[4], '),',
                    coord_sys[3], '("', date[1], '":"', date[2],'")',
                    '], "netcdf")')

  }

  query_encode  <- urltools::url_encode(query)
  request       <- paste(query_url, query_encode, collapse = NULL, sep="")

  res <- GET(request)
  bin <- content(res, "raw")

  writeBin(bin, paste0(tempdir(),filename))
  ncdf <- nc_open(paste0(tempdir(),filename))

  dates <- ncvar_get(ncdf, coord_sys[3])
  dates <- dates/86400
  dates <- as.Date(dates, origin = "1970-01-01")

  values <- ncvar_get(ncdf, band)

  chip_center_x <- round(ncdf$dim$E$len/2)
  chip_center_y <- round(ncdf$dim$N$len/2)

  nc_close(ncdf)
  if(filename!="request") file.remove(paste0(tempdir(),filename))

  if(plot==TRUE){

    # Setup Plot
    cols <- round(sqrt(length(dates)))
    rows <- cols + 1
    par(mar = c(0,1,1,1), oma = c(3,3,1,1))
    layout(matrix(c(rep(1,rows),c(seq(2,rows*cols+1, by = 1))), nrow = rows, byrow = TRUE))

    # Define Parameters
    start_date <- floor_date(dates[1], unit = "month")
    end_date <- ceiling_date(dates[length(dates)], unit = "month")

    max <- range(values[, chip_center_x, chip_center_y])[2] %>% as.character() %>% nchar()

    y_low <- 0
    y_up  <- round(range(values[, chip_center_x, chip_center_y])[2], -max+1)

    # Draw the Plot
    p <- plot(dates, values[, chip_center_x, chip_center_y], type = "l", lwd = 2, axes = FALSE)
    axis(side = 3,
         at = seq(start_date, end_date, by= "month"),
         labels = seq(start_date, end_date, by= "month"),
         cex.axis = 0.8,
         padj = 1)
    axis(side = 2,
         at = seq(y_low,y_up, by = y_up/5),
         labels = seq(y_low,y_up, by = y_up/5))

    par(pty = "s")

    # Add to Plot
    for(j in 1:length(dates)){
      image(values[j,,], col = gray.colors(10), axes = F, pty = "s")
      title(main = dates[j], cex.main = 0.8)

      if(is.element(j, seq(1,rows*cols,by = rows))){
        axis(side = 2, at = c(par("usr")[3],par("usr")[2]), labels = c(bounding_box[3],bounding_box[4]),
             las = 1, hadj = 1, padj = 0, cex.axis = 0.8)

      }

      if (is.element(j+rows, seq(rows^2-rows+1,rows^2,by = 1))){
        axis(side = 1, at = c(par("usr")[1],par("usr")[4]), labels = c(bounding_box[1],bounding_box[2]),
             las = 2, padj = 0, hadj = 1, cex.axis = 0.8)
      }
    }

    return(p)

  }else return(list(Request=request,BB=bounding_box,TimeStamps=dates,Values=values))
}
