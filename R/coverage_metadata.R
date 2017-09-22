#' @title Get coordinate system
#' @description Get the coordinate system of a coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url
#' @param coverage Name of a coverage
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
