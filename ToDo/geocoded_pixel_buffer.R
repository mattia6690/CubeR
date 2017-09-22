# Pixel history of "band" (coverage band) for a given point determined "coords"
# Additionally pixels around the specified point are shown in a range determined by "buffer"
geocoded_pixel_buffer <- function(coverage, coord_sys, coords, band, buffer, date = NULL){

  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  #                                                                                                                #
  # coord_sys: coordinate system; class: [character];                                                              #
  # format: (X,Y,Time)                                                                                             #
  # example: c("E", "N", "DATE")                                                                                   #
  #                                                                                                                #
  # coords: coordinates of the location of interest; class: [character]                                            #
  # format: c(X,Y)                                                                                                 #
  # example:  c("610000", "5150000")                                                                               #
  #                                                                                                                #
  # band: coverage band; class: [character]                                                                        #
  # example: band = "B8A"                                                                                          #
  #                                                                                                                #
  # buffer: desired buffer to display pixels around given coordinates; class: [character]                          #
  # example: buffer = "20"                                                                                         #
  # note: buffer is measured in meters. Therefore, the minimum buffer threshold has to be the coverages resolution #
  #                                                                                                                #
  # date: date range; class: [character, Date]                                                                     #
  # format: "%yyyy-%mm-%dd"                                                                                        #
  # example: c("2015-12-31", "2016-12-31")                                                                         #
  # default: NULL; If date is per default NULL, the pixel history for all available timestamps is computed         #
  #                                                                                                                #
  # NOTE: This function writes a temporary file to the following directory:                                        #
  #       <your-current-working-directory>/tmp/                                                                    #
  #                                                                                                                #
  #       If the tmp folder does not exist, it will be created automatically.                                      #
  #       The file will immediately be removed when the function has finished reading its content.                 #
  #                                                                                                                #
  #       This procedure is currently used to read netCDF-files to R.                                              #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################

  # Calculate bounding box for the geocoded pixel-buffer
  bounding_box <- round(c(as.numeric(coords[1]) - as.numeric(buffer),
                          as.numeric(coords[1]) + as.numeric(buffer),
                          as.numeric(coords[2]) - as.numeric(buffer),
                          as.numeric(coords[2]) + as.numeric(buffer)))

  # Check if a daterange is desired or if full extent is required
  if(is.null(date)){

    # Construct query

    query <- paste0('for c in (', coverage, ') return encode (c.', band,
                    '[',
                    coord_sys[1], '(', bounding_box[1]-1, ':', bounding_box[2], '),',
                    coord_sys[2], '(', bounding_box[3]-1, ':', bounding_box[4], ')',
                    '], "netcdf")')

  } else {

    # Construct query

    query <- paste0('for c in (', coverage, ') return encode (c.', band,
                    '[',
                    coord_sys[1], '(', bounding_box[1]-1, ':', bounding_box[2], '),',
                    coord_sys[2], '(', bounding_box[3]-1, ':', bounding_box[4], '),',
                    coord_sys[3], '("', date[1], '":"', date[2],'")',
                    '], "netcdf")')

  }


  # Create request
  query_encode  <- urltools::url_encode(query)
  request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")

  # print request
  print(request)

  # get content
  res <- GET(request)
  bin <- content(res, "raw")

  # write binary data to temporary file
  writeBin(bin, paste0("tmp/",res$cookies$value))

  # open file connection
  ncdf <- nc_open(paste0("tmp/",res$cookies$value))

  # get timeseries dates
  dates <- ncvar_get(ncdf, coord_sys[3])

  # convert julian date in seconds to Date
  dates <- dates/86400
  dates <- as.Date(dates, origin = "1970-01-01")

  # get timeseries values
  values <- ncvar_get(ncdf, band)

  # calculate center pixel position
  chip_center_x <- round(ncdf$dim$E$len/2)
  chip_center_y <- round(ncdf$dim$N$len/2)

  # close file connection
  nc_close(ncdf)

  # remove temporary file
  file.remove(paste0("tmp/",res$cookies$value))

  # define plot grid
  cols <- round(sqrt(length(dates)))
  rows <- cols + 1
  par(mar = c(0,1,1,1), oma = c(3,3,1,1))
  layout(matrix(c(rep(1,rows),c(seq(2,rows*cols+1, by = 1))), nrow = rows, byrow = TRUE))

  # define x-limits
  start_date <- floor_date(dates[1], unit = "month")
  end_date <- ceiling_date(dates[length(dates)], unit = "month")

  # check magnitudes of values to be plotted
  max <- range(values[, chip_center_x, chip_center_y])[2] %>% as.character() %>% nchar()

  # define y-limits
  y_low <- 0
  y_up <- round(range(values[, chip_center_x, chip_center_y])[2], -max+1)

  # plot pixel history timeseries
  p <- plot(dates, values[, chip_center_x, chip_center_y], type = "l", lwd = 2, axes = FALSE)

  # add axes
  axis(side = 3, at = seq(start_date, end_date, by= "month"), labels = seq(start_date, end_date, by= "month"),
       cex.axis = 0.8, padj = 1)
  axis(side = 2, at = seq(y_low,y_up, by = y_up/5), labels = seq(y_low,y_up, by = y_up/5))

  # set shape of default plot region to a square (to correctly display pixels)
  par(pty = "s")

  # plot geocoded pixel-buffers
  for(j in 1:length(dates)){
    image(values[j,,], col = gray.colors(10), axes = F, pty = "s")
    title(main = dates[j], cex.main = 0.8)

    # add axes on the outermost images

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

  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # p: pixel history plus geocoded pixel-buffer plot;              #
  #    class: [NULL]; graphics object                              #
  # -------------------------------------------------------------- #
  ##################################################################

}
