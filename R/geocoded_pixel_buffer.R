#' @title Calculate Pixel Buffer History
#' @description Calculate pixel buffer history of a coverage band. This functionality is limited to Netcdf files
#' @param coverage character; name of the coverage
#' @param coords character; coordinates of the location of interest
#' @param band character; coverage band
#' @param buffer numeric; A buffer Zone around the coordinates of the Poi
#' @param date character; date range in format (Ymd). If NULL all timestamps are used
#' @param filename character; filename for the NETCDF Output. If none is provided the Object will be deleted after temporal storage in the tempdir
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @param plot boolean; do you want a generic plot to be returned?
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom httr GET content
#' @import graphics
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom magrittr "%>%"
#' @importFrom urltools url_encode
#' @importFrom grDevices gray.colors
#' @export

geocoded_pixel_buffer <- function(coverage, coords, band=NULL, buffer, date = NULL,
                                  filename="request",url=NULL,plot=F){

  # Build urls
  query_url<-createWCS_URLs(type="Query", url=url)

  # Use buildin functions
  coord_sys<-coverage_get_coordsys(coverage=coverage,url = url)
  bands.avail <- coverage_get_bands(coverage,url=url)

  if(is.null(band))   band<-bands.avail[1]
  if(length(band>1))  stop("In this function only one band is allowed")
  # Define Boundingbox
  bounding_box <- round(c(as.numeric(coords[1]) - as.numeric(buffer),
                          as.numeric(coords[1]) + as.numeric(buffer),
                          as.numeric(coords[2]) - as.numeric(buffer),
                          as.numeric(coords[2]) + as.numeric(buffer)))

  # X/Y/Date Queries
  meta <- getMetadata(coverage,url=url)

  x <-  filter(meta,Type=="X")
  query.x <- paste0(x$Axis,'(',bounding_box[1]-1, ':', bounding_box[2],')')
  y <-  filter(meta,Type=="Y")
  query.y <- paste0(',',y$Axis,'(',bounding_box[3]-1, ':', bounding_box[4],')')


  if(!isTRUE(NoTemp)) {
    d <- filter(lj,Type=="Time")
    dateS   <- as.Date(date)
    query.d <- paste0(',',d$Axis,'("',dateS[1],'":"', dateS[2],'")')

    timestamps  <- as.Date(coverage_get_timestamps(coverage))
    timestampsW <- timestamps>=dateS[1] & timestamps<=dateS[2]
    timestamps2 <- as.character(timestamps[timestampsW])

  } else {

    query.d<-NULL
    timestamps2<-NA

  }

  # Band Queries
  if(length(bands.avail)>1){query.b<-paste0("c.",bands)} else {query.b<-"c"}

  # Bind queries
  queries <- paste0('for c in ( ',coverage,' ) return encode( ',query.b,
                   '[',query.x,query.y,query.d,'],"img/tiff")')


  query_encode  <- urltools::url_encode(queries)
  request       <- paste(query_url, query_encode, collapse = NULL, sep="")

  # Handle queries
  res <- GET(request)
  bin <- content(res, "raw")

  # Handle netcdf return
  writeBin(bin, paste0(tempdir(),filename))
  ncdf <- nc_open(paste0(tempdir(),filename))

  dates <- ncdf$dim[["DATE"]]$vals
  dates <- dates/86400
  dates <- as.Date(dates, origin = "1970-01-01")

  values <- ncvar_get(ncdf, band)

  chip_center_x <- round(ncdf$dim$E$len/2)
  chip_center_y <- round(ncdf$dim$N$len/2)

  nc_close(ncdf)
  if(filename!="request") file.remove(paste0(tempdir(),filename))

  # Do the plotting
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
