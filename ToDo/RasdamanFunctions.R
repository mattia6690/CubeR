####################################################
# RasdamanFunctions.R                              #
# ------------------------------------------------ #
# Functions for querying raster cubes produced by  #
# the RASter DAta MANager (RASDAMAN).              #
#                                                  #
# Written by: Frisinghelli Daniel                  #
# Domain: EURAC                                    # 
# Institute: Earth Observation                     #
# Workgroup: Technical Environment                 #
# ------------------------------------------------ #
####################################################

###########################################################
# ------------------------------------------------------- #
# These functions can be used to access Rasdaman database #
# using WCS (Web Coverage Service) and WCPS (Web Coverage #
# Processing Service) standard OGC Web services.          #
# ------------------------------------------------------- #
# Both meta-data about the coverage and data from the co- #
# verage can be queried.                                  #
# ------------------------------------------------------- #
# At the moment a stable web-application created with     #
# shiny (by RStudio) for Sentinel 2 data is operational   # 
# at: http://10.8.244.145:3838/SOS4R/R/Rasdaman/          #
#                                                         # 
# At the moment, this url can only be reached in the in-  #
# ternal Net of the institute for Earth Observation.      #
# ------------------------------------------------------- #
# For using the code directly in R without the shiny app, #
# depending packages are listed below and will automatic- #
# ally be installed.                                      #
# ------------------------------------------------------- #
# Every function is explained in its functionality, input #
# and output.                                             #
# ------------------------------------------------------- #
# ENJOY!                                                  #
# ------------------------------------------------------- #
###########################################################

##### INITITALIZE PACKAGES #########################################################################################

# Function to check if a library ('mypgk') is available, otherwise it will be installed
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
{install.packages(mypkg, repos = "http://cran.r-project.org/")}; library(mypkg, character.only=TRUE)}

# Initialize packages
loadandinstall("magrittr")
loadandinstall("httr")
loadandinstall("stringr")
loadandinstall("lubridate")
loadandinstall("urltools")
loadandinstall("raster")
loadandinstall("rgdal")
loadandinstall("xml2")
loadandinstall("png")
loadandinstall("tiff")
loadandinstall("jpeg")
loadandinstall("ncdf4")
loadandinstall("grid")
loadandinstall("leaflet")
#loadandinstall("jsonlite")
loadandinstall("shiny")

####################################################################################################################

# ---------------------------------------------------------------------------------------------------------------- #

##### FUNCTIONS FOR QUERYING METADATA ##############################################################################

# Get coverage coordinate system
coverage_get_coordsys <- function(desc_url, coverage){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # desc_url: WCS DescribeCoverage url of Rasdaman database; class: [character]                                    #
  # example: "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID="#
  #                                                                                                                #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # The request returns an xml which describes the coverage metadata
  desc_xml = xml2::read_xml(paste0(desc_url,coverage))
  
  # Find xml node where the coordinate system is defined
  coord_sys = xml2::xml_find_all(desc_xml, ".//wcs:CoverageDescription") %>% xml_children(.) %>% .[5] %>% 
    xml_children(.) %>% xml_children(.) %>% .[2] %>% xml_text() %>% str_split(., " ") %>%  unlist()
  
  return(coord_sys)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # coord_sys: coordinate system of the coverage; class:[character]#
  # format: coord_sys = c(x, y, time)                              #
  # example: coord_sys = c("E","N", "DATE")                        #
  # -------------------------------------------------------------- #
  ##################################################################
  
}

# Get coverage coordinate system EPSG identifier
coverage_get_coordinate_reference <- function(desc_url, coord_url, coverage){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # desc_url: WCS DescribeCoverage url of Rasdaman database; class: [character]                                    #
  # example: "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID="#
  #                                                                                                                #
  # coord_url: url for retrieving EPSG code; class: [character]                                                    #
  # exmaple: "http://10.8.244.147:8080/def/crs/EPSG/0/"                                                            #
  #                                                                                                                #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # The request returns an xml which describes the coverage metadata 
  d_xml <- xml2::read_xml(paste0(desc_url,coverage))
  
  # Find xml node of spatial (EPSG) and temporal (UNIX) coordinate references
  sys_Id <- xml_children(d_xml) %>% xml_children(.) %>% xml_children(.) %>% .[1] %>% xml_attr(., "srsName") %>% 
    str_split(., "=") %>% unlist 
  
  # Get EPSG code
  if(length(sys_Id) > 1){
    sys_Id <- sys_Id  %>% .[2] %>% str_split(.,"/") %>% unlist %>% .[8] %>% str_split(.,"&") %>% unlist %>% 
      .[1]
  } else {
    sys_Id <- sys_Id  %>% .[1] %>% str_split(.,"/") %>% unlist %>% .[8] %>% str_split(.,"&") %>% unlist %>% 
      .[1]
  }
  
  return(sys_Id)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # sys_Id: EPSG code of coordinate system; class:[character]      #
  # example: sys_Id = "32632"                                      #
  # -------------------------------------------------------------- #
  ##################################################################
  
}

# Get coverage temporal extent
coverage_get_temporal_extent <- function(desc_url, coverage){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # desc_url: WCS DescribeCoverage url of Rasdaman database; class: [character]                                    #
  # example: "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID="#
  #                                                                                                                #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # The request returns an xml which describes the coverage metadata 
  t_xml = xml2::read_xml(paste0(desc_url,coverage))
  
  # Find xml node of the bounding box 
  t_extent = xml2::xml_find_all(t_xml, ".//wcs:CoverageDescription") %>% xml_children(.) %>% .[1] %>% 
    xml_children(.) %>% xml_children(.)
  
  # Get temporal extent (first and last date)
  tmp_ext = c(str_split(xml_text(t_extent[1]), " ") %>% unlist() %>% .[3] %>% str_replace_all(., "\"", ""),
              str_split(xml_text(t_extent[2]), " ") %>% unlist() %>% .[3] %>% str_replace_all(., "\"", "")
  )
  
  return(tmp_ext)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # tmp_ext: temporal extent of coverage; class:[character]        #
  # format: tmp_ext = c(date_min, date_max)                        #
  # example: tmp_ext = c("2015-06-27T00:00:00.000Z",               #
  #                      "2017-09-11T00:00:00.000Z")               #
  # -------------------------------------------------------------- #
  ##################################################################
}

# Get coverage bounding box 
coverage_get_bounding_box <- function(desc_url, coverage){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # desc_url: WCS DescribeCoverage url of Rasdaman database; class: [character]                                    #
  # example: "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID="#
  #                                                                                                                #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # The request returns an xml which describes the coverage metadata 
  s_xml = xml2::read_xml(paste0(desc_url,coverage))
  
  # Find xml node of the bounding box 
  s_extent = xml2::xml_find_all(s_xml, ".//wcs:CoverageDescription") %>% xml_children(.) %>% .[1] %>% 
    xml_children(.) %>% xml_children(.)
  
  # Get bounding box values
  s_extent_xmin = str_split(xml_text(s_extent[1]), " ") %>% unlist() %>% .[1]
  s_extent_xmax = str_split(xml_text(s_extent[2]), " ") %>% unlist() %>% .[1]
  s_extent_ymin = str_split(xml_text(s_extent[1]), " ") %>% unlist() %>% .[2]
  s_extent_ymax = str_split(xml_text(s_extent[2]), " ") %>% unlist() %>% .[2]
  
  # Bounding Box
  BB <- c(s_extent_xmin, s_extent_xmax, s_extent_ymin, s_extent_ymax)
  
  return(BB)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # BB: bounding box of the coverage; class:[character]            #
  # format: BB = c(xmin, xmax, ymin, ymax)                         #
  # example: BB = c("600000","709800", "5090220", "5200020")       #
  # -------------------------------------------------------------- #
  ##################################################################
  
}

# Get coverage's available timestamps (images)
coverage_get_timestamps <- function(desc_url, coverage){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # desc_url: WCS DescribeCoverage url of Rasdaman database; class: [character]                                    #
  # example: "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID="#
  #                                                                                                                #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # The request returns an xml which describes the coverage metadata 
  i_xml = xml2::read_xml(paste0(desc_url,coverage))
  
  # Find xml node where the timestamps of the available images of the coverage are defined
  av_img_times = xml2::xml_find_all(i_xml, ".//wcs:CoverageDescription") %>% xml_children(.) %>% .[5] %>%
    xml_children(.) %>% xml_children(.) %>% .[6] %>% xml_children(.) %>% xml_children(.) %>% .[2] %>%
    xml_text(.) %>% str_replace_all(., "\"", "") %>% str_split(.," ") %>% unlist()
  
  return(av_img_times)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # av_img_times: timestamps of available images; class:[character]#
  # format: av_img_times = seq(date_min, date_max, by = timestep)  #
  # example: av_img_times = c(date_min,                            #
  #                                     ...             ,          #
  #                           "2015-07-04T00:00:00.000Z",          #
  #                           "2015-07-24T00:00:00.000Z",          #
  #                           "2015-08-03T00:00:00.000Z",          #
  #                                     ...             ,          #   
  #                           date_max)                            #
  # -------------------------------------------------------------- #
  ##################################################################
  
}

# Get coverage bands
coverage_get_bands <- function(desc_url, coverage){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # desc_url: WCS DescribeCoverage url of Rasdaman database; class: [character]                                    #
  # example: "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID="#
  #                                                                                                                #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # The request returns an xml which describes the coverage metadata 
  b_xml = xml2::read_xml(paste0(desc_url,coverage))
  
  # Find xml node where the bands of the coverage are listed
  bands = xml2::xml_find_all(b_xml, ".//wcs:CoverageDescription") %>% xml_children(.) %>% .[6] %>%
    xml_children() %>% xml_find_all(.,"./swe:field") %>% xml_attr(.,"name")
  
  return(bands)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # bands: bands of the coverage; class:[character]                #
  # example: bands = c("AOT", "B02","B03", "B04", ..., "B8A", ... )#
  # -------------------------------------------------------------- #
  ##################################################################
  
}

# Get coverage resolution
coverage_get_resolution <- function(desc_url, coverage){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # desc_url: WCS DescribeCoverage url of Rasdaman database; class: [character]                                    #
  # example: "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID="#
  #                                                                                                                #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # The request returns an xml which describes the coverage metadata 
  r_xml = xml2::read_xml(paste0(desc_url,coverage))
  
  # Find xml node where the resolution of the coverage is defined, return absolute value
  resolution = xml2::xml_find_all(r_xml, ".//wcs:CoverageDescription") %>% xml_children(.) %>% .[5] %>% 
    xml_children() %>%  xml_children() %>% .[4] %>% xml_children() %>%  xml_children() %>% .[1] %>% xml_text() %>% 
    str_split(.," ") %>% unlist() %>% .[1] %>% as.numeric() %>% abs()
  
  return(resolution)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # resolution: resolution of the coverage; class:[numeric]        #
  # example: resolution = 20 [meters]                              #
  # -------------------------------------------------------------- #
  ##################################################################
  
}

####################################################################################################################

# ---------------------------------------------------------------------------------------------------------------- #

##### FUNCTIONS TO QUERY AND VISUALIZE COVERAGE DATA ###############################################################

# Convert RGB image [array] to grayscale (luminance) [matrix] 
rgb2gray <- function(img){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # img: RGB image; class: [array]; dim: [nrow, ncol, 3]                                                           #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # Compute luminance from RGB using matrix multiplication
  gray<- img[,,1]*0.299 + img[,,2]*0.587 + img[,,3]*0.114
  
  return(gray)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # gray: grayscale image; class: [matrix]; dim: [nrow, ncol, 1]   #
  # -------------------------------------------------------------- #
  ##################################################################
  
}

# Pixel history of "bands" (coverage bands) for a given point determined "coords"
pixel_history <- function(coverage, coord_sys, bands, coords, date=NULL){
  
  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  #                                                                                                                #
  # coord_sys: coordinate system; class: [character];                                                              #
  # format: (X,Y,Time)                                                                                             #
  # example: c("E", "N", "DATE")                                                                                   #
  #                                                                                                                #
  # bands: coverage bands to calculate and visualize pixel history; class: [character]                             #
  # example: bands = c("AOT", "B02","B03", "B04", ..., "B8A", ... )                                                #
  #                                                                                                                #  
  # coords: coordinates of the location of interest; class: [character]                                            #
  # format: c(X,Y)                                                                                                 #
  # example:  c("610000", "5150000")                                                                               #
  #                                                                                                                #
  # date: date range; class: [character, Date]                                                                     #
  # format: "%yyyy-%mm-%dd"                                                                                        #
  # example: c("2015-12-31", "2016-12-31")                                                                         #
  # default: NULL; If date is per default NULL, the pixel history for all available timestamps is computed         #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################
  
  # Get all available timestamps
  times<-coverage_get_timestamps(desc_url,coverage)
  
  # Check if a daterange is desired or if full extent is required
  if(is.null(date)){
    
    # convert to Date
    times <- times %>% as.Date()
    
    # dates for the query
    start_date_mod <- times[1]
    end_date_mod <- times[length(times)]
    
  } else {
    
    # dates for the query
    start_date_mod = date[1]
    end_date_mod = date[2]
    
    # Restrict all dates (times) to the desired daterange
    times <- times[times >= start_date_mod & times <= end_date_mod] %>% as.Date()
    
  }
  
  # Get number of input bands
  bands_len <- length(bands)
  
  # Error Handling if no band is selected
  if(bands_len == 0){
    
    p <- plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    p <- text(1,1, "Can't visualize pixel history for selected coverage, reason: No band selected!", cex = 2)
    
  } else {
    
    # List of pixel histories for each band
    responses = list()
    
    # List of maximum values of pixel history for each band
    max_res = list()
    
    # Initiate loop to create a query for every input band
    for(k in 1:bands_len){
      
      # Construct the Query
      query <-str_c('for c in ( ',coverage,' ) return encode( c.',bands[k],'[',coord_sys[1],'(',coords[1],'),',coord_sys[2],'(',coords[2],
                    '),', coord_sys[3], '("', start_date_mod,'":"', end_date_mod,'")],"csv")')
      
      
      # Create the Request
      query_encode  <- urltools::url_encode(query)
      request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")
      r             <- GET(request)
      
      # Get content
      res <- content(r, "text")
      
      # parse text content to vector
      num <- str_replace_all(res,"\\{","") %>% str_replace_all(.,"\\}","") %>% str_replace_all(.,"\"","") %>% 
        str_split(.,",") %>% unlist() %>% str_split(.," ") %>% unlist() %>% as.numeric()
      
      # write to list
      responses[[k]] = num
      
      # Get maximum value
      max_res[[k]] = max(num)
    }
    
    # Get absolute maximum value for plotting
    abs_max <- max(na.omit(unlist(max_res)))
    
    # plot first entry in the list "responses" to create a plot environment to add other entries to
    p <- plot(times,responses[[1]], type="o", col = rainbow(1)[1], lwd = 2, xlab="Date", ylab="Single channels", ylim = c(0,abs_max),
              cex.axis = 1.2, cex.lab = 1.2)
    
    # Check how many bands to plot
    if(length(responses) == 1){
      
      # Add legend
      p<-legend("topright", inset = .02,legend=c(bands), col = c(rainbow(length(responses))), pch=15)
      
    } else{
      
      # Initiate loop to plot all input bands
      for(l in 2:length(responses)){
        p <- lines(times, responses[[l]], type = "o", col = rainbow(l)[l], lwd = 2)
        
      }
      
      # Add legend
      p<-legend("topright", inset = .02,legend=c(bands), col = c(rainbow(length(responses))), pch=15)
      
    }
    
  }
  
  return(p)
  
  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # p: pixel history plot; class: [NULL]; graphics object          #
  # -------------------------------------------------------------- #
  ##################################################################
  
}

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

####################################################################################################################
