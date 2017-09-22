# Calculate raster layer(s) of the coverage that can be visualized in leaflet() using addRasterImage(raster)
image_from_coverage <- function(coverage, coord_sys, slice_E, slice_N, date, ref_Id, res_eff, format, bands){

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
  # bands: coverage bands to visualize image; class: [character]                                                   #
  # example: bands = c("AOT", "B02","B03", "B04", ..., "B8A", ... )                                                #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################

  # Get number of input bands
  bands_len <- length(bands)

  # list of rasters to compute
  rasters <- list()

  # Initiate loop to create a query for every input band
  for(i in 1:bands_len){

    #### Construct the query ####

    ##### Use WCS to create channel selection with RANGESUBSET parameter #####
    # This is currently a very slow query, since the RANGESUBSET parameter
    # does not work (or really slowly) when selecting more bands

    # Allthough this would be the query using RANGESUBSET

    # bands_subset <- paste(bands, collapse = ",")
    #
    # query <- paste0(coverage, '&SUBSET=', coord_sys[1],'(', slice_E[1], ',', slice_E[2], ')',
    #                 '&SUBSET=', coord_sys[2],'(', slice_N[1], ',', slice_N[2], ')', '&SUBSET=', coord_sys[3],
    #                 '("', date, '")', '&RANGESUBSET=' , bands_subset ,'&FORMAT=image/', format)
    #
    # query_encode  <- urltools::url_encode(query)
    # request       <- paste(tiff_url, query_encode, collapse = NULL, sep="")

    ##########################################################################

    # Use WCPS to create band selection

    query <- paste0('for c in (', coverage, ') return encode (c.', bands[i],
                    '[',
                    coord_sys[1], '(', slice_E[1], ':', slice_E[2], ')', ',',
                    coord_sys[2], '(', slice_N[1], ':', slice_N[2], ')', ',',
                    coord_sys[3], '("', date, '")',
                    '],',
                    '"', format,'"',')')

    # Create the Request
    query_encode  <- urltools::url_encode(query)
    request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")

    # print request
    print(request)

    # get content
    res <- GET(request)
    bin <- content(res, "raw")

    # read response to image depending on selected format
    to_img <- get(paste0("read",toupper(format)))
    img <- to_img(bin)

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

      # append to rasters list
      rasters[[i]] <- ras

    } else {

      # reduce raster dimension for speeding up computation
      ras_aggregate <- aggregate(ras, fact=res_eff, expand = FALSE)

      # print raster metadata
      print(ras_aggregate)

      # append to rasters list
      rasters[[i]] <- ras_aggregate

    }

  }

  return(rasters)

  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # rasters: list of raster layers; class: [list];                 #
  #          Length of the list is equal to the number of input    #
  #          bands. Order of raster layers corresponds to the order#
  #          of the input bands.                                   #
  # -------------------------------------------------------------- #
  ##################################################################

}
