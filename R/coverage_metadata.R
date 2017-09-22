#' @title Get coordinate system
#' @description Get the coordinate system of a coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' @param coverage Name of a coverage [character]
#' @import magrittr
#' @import xml2
#' @export

coverage_get_coordsys <- function(desc_url, coverage){

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
#' @description Function to extract the EPSG identifier from a WCS coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' @param coord_url character; Url for retrieving EPSG code [character]
#' @param coverage Name of a coverage [character]
#' @import magrittr
#' @import xml2
#' @importFrom stringr str_split
#' @export

coverage_get_coordinate_reference <- function(desc_url, coord_url, coverage){

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

#' @title Temporal Extent
#' @description Get the temporal extent from a WCS WCPS Coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' @param coverage Name of a coverage [character]
#' @import magrittr
#' @import xml2
#' @importFrom stringr str_split str_replace_all
#' @export

coverage_get_temporal_extent <- function(desc_url, coverage){


  t_xml = xml2::read_xml(paste0(desc_url,coverage))

  t_extent = xml2::xml_find_all(t_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[1] %>%
    xml_children(.) %>% xml_children(.)

  tmp_ext = c(str_split(xml_text(t_extent[1]), " ") %>% unlist() %>% .[3] %>% str_replace_all(., "\"", ""),
              str_split(xml_text(t_extent[2]), " ") %>% unlist() %>% .[3] %>% str_replace_all(., "\"", "")
  )

  return(tmp_ext)

}

#' @title Bounding Box
#' @description Get a bounding Box of a coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' @param coverage Name of a coverage [character]
#' @import magrittr
#' @import xml2
#' @importFrom stringr str_split
#' @export

coverage_get_bounding_box <- function(desc_url, coverage){

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
#' @description Get coverage's available timestamps (images)
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' @param coverage Name of a coverage [character]
#' @import magrittr
#' @import xml2
#' @importFrom stringr str_split
#' @export

coverage_get_timestamps <- function(desc_url, coverage){

  i_xml = xml2::read_xml(paste0(desc_url,coverage))

  av_img_times = xml2::xml_find_all(i_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[5] %>%
    xml_children(.) %>% xml_children(.) %>% .[6] %>%
    xml_children(.) %>% xml_children(.) %>% .[2] %>%
    xml_text(.) %>% str_replace_all(., "\"", "") %>%
    str_split(.," ") %>% unlist()

  return(av_img_times)

}

#' @title Get Bands
#' @description Get the available bands of one coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' @param coverage Name of a coverage [character]
#' @import magrittr
#' @import xml2
#' @export

coverage_get_bands <- function(desc_url, coverage){

  b_xml = xml2::read_xml(paste0(desc_url,coverage))

  bands = xml2::xml_find_all(b_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[6] %>%
    xml_children() %>%
    xml_find_all(.,"./swe:field") %>% xml_attr(.,"name")

  return(bands)

}

#' @title Get Resolution
#' @description Get the resolution of one coverage
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character]
#' @param coverage Name of a coverage [character]
#' @import magrittr
#' @import xml2
#' @importFrom stringr str_split
#' @export

coverage_get_resolution <- function(desc_url, coverage){

  r_xml = xml2::read_xml(paste0(desc_url,coverage))

  resolution = xml2::xml_find_all(r_xml, ".//wcs:CoverageDescription") %>%
    xml_children(.) %>% .[5] %>%
    xml_children() %>%  xml_children() %>% .[4] %>%
    xml_children() %>%  xml_children() %>% .[1] %>%
    xml_text() %>% str_split(.," ") %>% unlist() %>% .[1] %>%
    as.numeric() %>% abs()

  return(resolution)

}
