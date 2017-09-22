# Calculate and visualize normalized difference pixel history between input band1 and input band2
norm_diff_hist <- function(coverage, coord_sys, coords, band1, band2, date = NULL){

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
  # band1: Input band1; class: [character]                                                                         #
  # example: band1 = "B8A"                                                                                         #
  #                                                                                                                #
  # band2: Input band2; class: [character]                                                                         #
  # example: band2 = "B04"                                                                                         #
  #                                                                                                                #
  # date: date range; class: [character, Date]                                                                     #
  # format: "%yyyy-%mm-%dd"                                                                                        #
  # example: c("2015-12-31", "2016-12-31")                                                                         #
  # default: NULL; If date is per default NULL, the pixel history for all available timestamps is computed         #
  #                                                                                                                #
  # NOTE: Normalized Difference is calculated according to:                                                        #
  #                                                                                                                #
  #       ND = (band1 - band2)/(band1 + band2)                                                                     #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################

  # Error Handling if a input band is missing
  if(is.null(band1) | is.null(band2)){

    p <- plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    p <- text(1,1, "Can't calculate normalized difference for selected coverage, reason: No band selected!", cex = 2)

  } else {

    # Get all available timestamps
    times<-coverage_get_timestamps(desc_url,coverage)

    # Check if a daterange is desired or if full extent is required
    if(is.null(date)){

      # Convert to Date
      times <- times %>% as.Date()

      # Construct query
      query <- paste0('for c in (', coverage, ') return encode (',
                      '( (int) c.',
                      band1,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2], '(',coords[2],')]', '- c.',
                      band2,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2], '(',coords[2],')])','/',
                      '( (int) c.',
                      band1,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2], '(',coords[2],')]', '+ c.',
                      band2,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2], '(',coords[2],')]', '),',
                      '"csv"',')')
    } else {

      # Restrict all dates (times) to the desired daterange
      times <- times[times >= date[1] & times <= date[2]] %>% as.Date()

      # Construct query
      query <- paste0('for c in (', coverage, ') return encode (( (int) c.',band1,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2],
                      '(',coords[2],'),', coord_sys[3], '("',date[1],'":"', date[2],'")',']', '- c.',band2,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2],
                      '(',coords[2],'),', coord_sys[3], '("',date[1],'":"', date[2],'")',']',')','/', '( (int) c.',band1,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2],
                      '(',coords[2],'),', coord_sys[3], '("',date[1],'":"', date[2],'")',']', '+', 'c.',band2,'[', coord_sys[1],'(',coords[1],'),',coord_sys[2],
                      '(',coords[2],'),', coord_sys[3], '("',date[1],'":"', date[2],'")',']', '),', '"csv"',')')

    }

    # Create request
    query_encode  <- urltools::url_encode(query)
    request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")

    # print request
    print(request)

    #### JSON ########################################################
    # JSON response resulted in a Error if NANs were in the timeseries
    # res <- jsonlite::fromJSON(request)
    ##################################################################

    #### CSV #########################################################

    # Get content
    res <- GET(request)
    bin <- content(res,"text") %>% str_replace_all(.,"\\{","") %>%
      str_replace_all(.,"\\}","") %>% str_split(.,",") %>% unlist() %>%
      as.numeric()

    res <- bin

    ##################################################################

    # plot normalized difference pixel history
    p<-plot(times,res,type="o", lwd = 2, xlab="Date", ylab="Normalized difference", ylim = c(-1,1), cex.axis = 1.2, cex.lab = 1.2)
    p <-legend("topright", inset = .02,legend=paste0(band1," - ",band2), pch=15)
    p<-title(paste0("Normalized Difference between ", band1, " and ", band2))

  }

  return(p)

  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # p: normalized difference pixel history plot; class: [NULL];    #
  #    graphics object                                             #
  # -------------------------------------------------------------- #
  ##################################################################

}

# Calculate a raster layer of normalized difference between band_1 and band_2 of the coverage that
# can be visualized in leaflet() using addRasterImage(raster)
norm_diff_raster <- function(coverage, coord_sys, slice_E, slice_N, date, ref_Id, res_eff, format,
                             band_1, band_2){

  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  #                                                                                                                #
  # coord_sys: coordinate system; class: [character];                                                              #
  # format: (X,Y,Time)                                                                                             #
  # example: c("E", "N", "DATE")                                                                                   #
  #                                                                                                                #
  # slice_E: image slicing coordinates in x-direction ; class: [character]                                         #
  # format: c(xmin, xmax)                                                                                          #
  # example: c("600000","610000")                                                                                  #
  #                                                                                                                #
  # slice_N: image slicing coordinates in y-direction ; class: [character]                                         #
  # format: c(ymin, ymax)                                                                                          #
  # example: c("5190000","5200000")                                                                                #
  #                                                                                                                #
  # date: an available timestamp; class: [character]                                                               #
  # format: "%yyyy-%mm-%ddT00:00:00.000Z"                                                                          #
  # example: c("2015-12-31T00:00:00.000Z")                                                                         #
  # note: to get all available timestamps of a coverage, use function coverage_get_timestamps(desc_url, coverage)  #
  #                                                                                                                #
  # ref_Id: EPSG code of the coordinate system; class: [character]                                                 #
  # example: ref_Id = "32632"                                                                                      #
  #                                                                                                                #
  # res_eff: factor to scale raster resolution; class: [numeric]; has to be > 1                                    #
  # example: res_eff = 5                                                                                           #
  # note: A value of 1 returns original image resolution.                                                          #
  #       High values allow faster visualization since number of pixels decreases.                                 #
  #                                                                                                                #
  # format: image output format; class: [character]; choices: c("tiff", "png", "jpeg")                             #
  # example: format = "tiff"                                                                                       #
  #                                                                                                                #
  # band_1: Input band1; class: [character]                                                                        #
  # example: band_1 = "B8A"                                                                                        #
  #                                                                                                                #
  # band_2: Input band2; class: [character]                                                                        #
  # example: band_2 = "B04"                                                                                        #
  #                                                                                                                #
  # NOTE: Normalized Difference is calculated according to:                                                        #
  #                                                                                                                #
  #       ND = (band_1 - band_2)/(band_1 + band_2)                                                                 #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################

  # Construct query

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


  # Create request
  query_encode  <- urltools::url_encode(query)
  request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")

  # print request
  print(request)

  # get content
  res <- GET(request)
  bin <- content(res, "raw")

  # read response to image depending on selected format
  to_img <- get(paste0("read",toupper(format)))

  # get original values
  img <- to_img(bin)
  img <- (img*2) - 1

  # define raster extent
  ras_ext <- extent(c(as.numeric(slice_E), as.numeric(slice_N)))

  # create raster
  ras = raster(img)

  # set reference coordinate system and extent
  proj4string(ras) <- CRS(paste0("+init=epsg:",ref_Id))
  extent(ras) <- ras_ext

  # check if scaling ("res_eff" > 1) is desired or not
  if(res_eff == 1){

    # print raster metadata
    print(ras)
    return(ras)

  } else {

    # reduce raster dimension for speeding up computation
    ras_aggregate <- aggregate(ras, fact=res_eff, expand = FALSE)

    # print raster metadata
    print(ras_aggregate)

    return(ras_aggregate)

  }

  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # ras:  raster; class: [Raster Layer];                           #
  #       Raster Layer with original image resolution              #
  #                                                                #
  #                             OR                                 #
  #                                                                #
  # ras_aggregate: raster; class: [Raster Layer];                  #
  #                Raster Layer with scaled resolution ("res_eff") #
  # -------------------------------------------------------------- #
  ##################################################################

}

