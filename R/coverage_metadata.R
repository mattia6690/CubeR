#' @title Returns the Capabilities
#' @description This function Returns the Capabilities of a DataCube containing all coverages available
#' @param url This central URL Leads to the 'ows' page of the Datacube
#' @importFrom xml2 read_xml xml_text xml_find_all
#' @export

getCapability <-function(url=NULL){

  if(is.null(url)){
    url = "http://10.8.244.147:8080/rasdaman/ows"
  }

  urlp<-paste0(url,"?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCapabilities")
  xml_cov1 <- xml2::read_xml(urlp)
  xml_cov1 <- xml_text(xml2::xml_find_all(xml_cov1, ".//wcs:CoverageId"))

  return(xml_cov1)
}

#' @title WCS URL creator
#' @description This function provides the possibility to construct the url necessary to address different layers of
#' Metadata. Every Metadata Information in  the WCS OGS format has to be addressed with a differen URL Query.
#' @param url This central URL Leads to the 'ows' page of the Datacube. If empy the Standard Rasdaman page is used.
#' @param type characer; Mandatory input for the URL that should be created with the function.
#' For now the types "Meta", "Pixel","Tiff","Coords","Time" can be created.
#' @importFrom stringr str_split str_replace_all
#' @export

createWCS_URLs<-function(url=NULL,type){

  if(is.null(url))url = "http://10.8.244.147:8080/rasdaman/ows"

  urlsmall<-str_split(url,"/")[[1]]
  urlsmall<-paste(urlsmall[1:3],"/",collapse = "") %>% str_replace_all(.," ","")

  if(type=="Meta")  url2<-paste0(url,"?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID=")
  if(type=="Query") url2<-paste0(url,"?SERVICE=WCS&VERSION=2.0.1&REQUEST=ProcessCoverages&QUERY=")

  return(url2)
}

#' @title Get coordinate system
#' @description Get the coordinate system of a coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character].
#' This URL can be built with the *createWCS_URLs* funtion
#' @param coverage Name of a coverage [character]
#' @importFrom magrittr "%>%"
#' @importFrom xml2 read_xml xml_find_all xml_children xml_text
#' @importFrom stringr str_split
#' @export

coverage_get_coordsys <- function(desc_url = NULL, coverage){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")

  desc_xml = xml2::read_xml(paste0(desc_url,coverage))

  coord_sys = xml2::xml_find_all(desc_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[5] %>%
    xml_children(.) %>%
    xml_children(.) %>% .[2] %>%
    xml_text() %>% str_split(., " ") %>%
    unlist()

  return(coord_sys)

}

#' @title Get EPSG identifier
#' @description Function to extract the EPSG identifier from a WCS WCPS coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character].
#' This URL can be built with the *createWCS_URLs* funtion
#' @param coverage Name of a coverage [character]
#' @import xml2
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split
#' @export

coverage_get_coordinate_reference <- function(desc_url=NULL, coverage){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")

  d_xml <- xml2::read_xml(paste0(desc_url,coverage))

  sys_Id <- xml_children(d_xml) %>%
    xml_children(.) %>% xml_children(.) %>% .[1] %>%
    xml_attr(., "srsName") %>%
    str_split(., "=") %>% unlist

  if(length(sys_Id) > 1){
    sys_Id <- sys_Id  %>% .[2] %>%
      str_split(.,"/") %>% unlist %>% .[8] %>%
      str_split(.,"&") %>% unlist %>% .[1]
  } else {
    sys_Id <- sys_Id  %>% .[1] %>%
      str_split(.,"/") %>% unlist %>% .[8] %>%
      str_split(.,"&") %>% unlist %>% .[1]
  }

  return(sys_Id)

}

#' @title Get Temporal Extent
#' @description Get the temporal extent from a WCS WCPS Coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' This URL can be built with the *createWCS_URLs* funtion
#' @param coverage Name of a coverage [character]
#' @import xml2
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split str_replace_all
#' @export

coverage_get_temporal_extent <- function(desc_url=NULL, coverage){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")

  t_xml = xml2::read_xml(paste0(desc_url,coverage))

  t_extent = xml2::xml_find_all(t_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[1] %>%
    xml_children(.) %>% xml_children(.)

  tmp_ext = c(str_split(xml_text(t_extent[1]), " ") %>% unlist() %>% .[3] %>% str_replace_all(., "\"", ""),
              str_split(xml_text(t_extent[2]), " ") %>% unlist() %>% .[3] %>% str_replace_all(., "\"", "")
  )

  return(tmp_ext)

}

#' @title Get Bounding Box
#' @description Get a bounding Box of a WCS, WCPS coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' This URL can be built with the *createWCS_URLs* funtion
#' @param coverage Name of a coverage [character]
#' @import xml2
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split
#' @export

coverage_get_bounding_box <- function(desc_url=NULL, coverage){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")

  s_xml = xml2::read_xml(paste0(desc_url,coverage))

  s_extent = xml2::xml_find_all(s_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[1] %>%
    xml_children(.) %>% xml_children(.)

  s_extent_xmin = str_split(xml_text(s_extent[1]), " ") %>% unlist() %>% .[1]
  s_extent_xmax = str_split(xml_text(s_extent[2]), " ") %>% unlist() %>% .[1]
  s_extent_ymin = str_split(xml_text(s_extent[1]), " ") %>% unlist() %>% .[2]
  s_extent_ymax = str_split(xml_text(s_extent[2]), " ") %>% unlist() %>% .[2]

  BB <- c(s_extent_xmin, s_extent_xmax, s_extent_ymin, s_extent_ymax)

  return(BB)

}

#' @title Get Timestamps
#' @description Get the available timestamps of WCS, WCPS coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' This URL can be built with the *createWCS_URLs* funtion
#' @param coverage Name of a coverage [character]
#' @import xml2
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split
#' @export

coverage_get_timestamps <- function(desc_url=NULL, coverage){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")

  i_xml <- read_xml(paste0(desc_url,coverage))

  av_img_times <- xml_find_all(i_xml, ".//wcs:CoverageDescription") %>%
    xml2::xml_children(.) %>% .[5] %>%
    xml_children(.) %>% xml_children(.) %>% .[6] %>%
    xml_children(.) %>% xml_children(.) %>% .[2] %>%
    xml_text(.) %>% str_replace_all(., "\"", "") %>%
    str_split(.," ") %>% unlist()

  return(av_img_times)

}

#' @title Get Bands
#' @description Get the available bands of one WCS, WCPS coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' This URL can be built with the *createWCS_URLs* funtion
#' @param coverage Name of a coverage [character]
#' @import xml2
#' @importFrom magrittr "%>%"
#' @export

coverage_get_bands <- function(desc_url=NULL, coverage){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")

  b_xml<- read_xml(paste0(desc_url,coverage))

  bands<- xml_find_all(b_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[6] %>%
    xml_children() %>%
    xml_find_all(.,"./swe:field") %>% xml_attr(.,"name")

  return(bands)

}

#' @title Get Resolution
#' @description Get the resolution of one WCS, WCPS coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' This URL can be built with the *createWCS_URLs* funtion
#' @param coverage Name of a coverage [character]
#' @import xml2
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split
#' @export

coverage_get_resolution <- function(desc_url=NULL, coverage){

  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")

  r_xml = xml2::read_xml(paste0(desc_url,coverage))

  resolution = xml2::xml_find_all(r_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[5] %>%
    xml_children() %>%  xml_children() %>% .[4] %>%
    xml_children() %>%  xml_children() %>% .[1] %>%
    xml_text() %>% str_split(.," ") %>% unlist() %>% .[1] %>%
    as.numeric() %>% abs()

  return(resolution)

}
