#' @title WCS URL creator
#' @description This function provides the possibility to construct the url necessary to address different layers of
#' Metadata. Every Metadata Information in  the WCS OGS format has to be addressed with a differen URL Query.
#' @param type character; Mandatory input for the URL that should be created with the function.
#' For now the types "Meta", "Query","Capabilities" can be created.
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @export

createWCS_URLs<-function(type, url = NULL){

  if(is.null(url)) url = "http://saocompute.eurac.edu/rasdaman/ows"

  urlsmall<-strsplit(url,"/")[[1]]
  urlsmall<-paste0(urlsmall[1:3],"/",collapse = "")

  if(type=="Meta")  url2<-paste0(url,"?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID=")
  if(type=="Query") url2<-paste0(url,"?SERVICE=WCS&VERSION=2.0.1&REQUEST=ProcessCoverages&QUERY=")
  if(type=="Capability")  url2<-paste0(url,"?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCapabilities")

  return(url2)

}

#' @title Returns the Service Provider of the Rasdaman Environment
#' @description This function returns the contact information to the Rasdaman service provider
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom XML xmlParse xmlToList
#' @importFrom dplyr filter
#' @importFrom tibble enframe
#' @importFrom stringr str_replace_all
#' @export
getProvider <- function(url=NULL){

  urlp<- createWCS_URLs(type="Capability",url=url)
  str<- "ServiceProvider."
  xml_cov1 <- xmlParse(urlp)
  xml_cov2 <- enframe(unlist(xmlToList(xml_cov1)))
  xml_cov3 <- filter(xml_cov2,grepl(str,name))
  xml_cov3$name <- str_replace(xml_cov3$name,str,"")

  return(xml_cov3)

}

#' @title Returns the Coverages
#' @description This function Returns the Capabilities of a DataCube containing all coverages available
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom XML xmlParse xmlToList
#' @importFrom dplyr filter
#' @importFrom tibble enframe
#' @export
getCoverages <-function(url=NULL){

  urlp     <- createWCS_URLs(type="Capability",url=url)
  xml_cov1 <- xmlParse(urlp)
  xml_cov2 <- enframe(unlist(xmlToList(xml_cov1)))
  xml_cov3 <- filter(xml_cov2,grepl("CoverageId",name))[["value"]]

  return(xml_cov3)

}

#' @title Find Capabilities
#' @description This function searches for available Coverages within the
#' @param str character; string to search for among coverages
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @export
findCoverages <-function(str,url=url){

  capas<-getCoverages()

  gr <- grep("NDVI",capas,value = T)
  ind<- grep("NDVI",capas)

  bind<-cbind(gr,ind)
  colnames(bind)<-c("Coverage","ID")
  return(bind)

}



#' @title Rasdaman support formats
#' @description This function returns the formats supported by the Rasdaman Version
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom XML xmlParse xmlToList
#' @importFrom dplyr filter
#' @importFrom tibble enframe
#' @export
getFormats <- function(url=NULL){

  urlp<- createWCS_URLs(type="Capability",url=url)
  xml_cov1 <- xmlParse(urlp)
  xml_cov2 <- enframe(unlist(xmlToList(xml_cov1)))
  xml_cov3 <- filter(xml_cov2,grepl("ServiceMetadata.",name))[["value"]]

  return(xml_cov3)

}


#' @title Set possible coverage Metadata axis names
#' @description Scrape the data in the whole Rasdaman Environment
#' @param addcols Add dimension columns and the respective name. If NULL the standard SAO columns are used
#' @param keepsao boolean; You want to keep the ones from SAO adjacent to the ones you defined?
#' @importFrom tibble as_tibble
#' @export
setMetaCols<-function(addcols=NULL,keepsao=F){

  if(is.null(addcols)){

    saoTcols<- cbind("Time",c("DATE","Date","Time","TIME","time","ansi","date"))
    saoXcols<- cbind("X",c("X","Lon","Long","E"))
    saoYcols<- cbind("Y",c("Y","Lat","N"))
    bind<-rbind(saoTcols,saoXcols,saoYcols)
    colnames(bind)<-c("Type","Axis")

  }

  if(!is.null(addcols)){

    if(ncol(addcols)!=2) stop("Please provide a matrix with two columns indicating the Type and Axis")
    if(keepsao==T) {
      bind<-rbind.data.frame(bind,addcols)
    } else {bind<-addcols}
  }

  colnames(bind)<-c("Type","Axis")
  bind<- as_tibble(bind)

  return(bind)
}

#' @title Parse coverage metadata
#' @description This function parses the metadata of a coverage and returns a tibble
#' containing both values and attributes. Other *coverage_get* functions are built on top of this function
#' @param coverage character; Name of a coverage
#' @param returnxml You want the Coverage Metadata XML return?
#' @param url character; Web Coverage Service (WCS) DescribeCoverage url
#' If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @note used in the getMetadata function
#' @importFrom XML xmlParse xmlToList
#' @importFrom tibble as_tibble enframe
#' @importFrom tidyr separate
#' @export
parseMetadata<-function(coverage,returnxml=F,url=NULL){

  desc  <- createWCS_URLs(type="Meta",url=url)
  xml   <- xmlParse(paste0(desc,coverage))

  if(isTRUE(returnxml)) return(xml)

  enf  <- enframe(unlist(xmlToList(xml,addAttributes = T)))
  enf2 <- tidyr::separate(enf,name,into=c("Level","Attributes"),sep=".attrs",fill="right")

  max.xml.lng<- lapply(enf2$Level,function(x){length(strsplit(x,"\\.")[[1]])})
  max.xml.lng<- max(unlist(max.xml.lng))

  lp<-lapply(enf2$Level,function(x,l=max.xml.lng){

    split     <- str_split(x,"\\.")[[1]]
    split.l   <- length(split)
    split.diff<- (l-split.l)

    if(split.diff>0) {
      r<-rep(NA,split.diff)
      return(c(split,r))
    } else {return(split)}
  })

  dc<-do.call(rbind,lp)
  colnames(dc) <-paste0("L",c(1:ncol(dc)))

  ret<-as_tibble(cbind(dc,Attributes=enf2$Attributes,Value=enf2$value))
  return(ret)

}


#' @title Get coverage metadata
#' @description This function retieves the actual Metadata from the
#' containing both values and attributes. Other *coverage_get* functions are built on top of this function
#' @param coverage character; Name of a coverage
#' @param Metacols tibble; 2 column tibble with all available columns and their dimension
#' If NULL the standard SAO columns will be used
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @note used in the getMetadata function
#' @importFrom dplyr filter left_join
#' @importFrom tibble as_tibble
#' @export
getMetadata <- function(coverage, Metacols=NULL, url=NULL){

  if(is.null(Metacols)) Metacols<- setMetaCols()

  # Get the Axes
  getMeta <- parseMetadata(coverage,url = url)
  labels  <- dplyr::filter(getMeta,L4=="axisLabels")[["Value"]]
  labels  <- strsplit(labels," ")[[1]]

  # Get the bounding Boxes
  bbox1<- dplyr::filter(getMeta,L2=="boundedBy" & L4=="lowerCorner")[["Value"]]
  bbox2<- dplyr::filter(getMeta,L2=="boundedBy" & L4=="upperCorner")[["Value"]]
  lc<-strsplit(bbox1," ")[[1]]
  uc<-strsplit(bbox2," ")[[1]]


  # Return the units
  units<- dplyr::filter(getMeta,L2=="boundedBy" & Attributes==".uomLabels")[["Value"]]
  units1 <- strsplit(units," ")[[1]]

  # Return the EPSG code
  srs<- dplyr::filter(getMeta,L2=="boundedBy" & Attributes==".srsName")[["Value"]]
  srs.split<-strsplit(srs,"&")[[1]]
  epsg <- grep(srs.split,pattern = "EPSG",value = T)
  epsg1<- strsplit(epsg,"/")[[1]]
  epsg2<- epsg1[length(epsg1)]

  # Return the Offset (resolution)
  offset1<- dplyr::filter(getMeta,L2=="domainSet" &
                            (L4=="offsetVector" | L6=="offsetVector" ) &
                            is.na(Attributes))[["Value"]]
  offset2<- lapply(offset1,function(x){
    split<-strsplit(x," ")[[1]]
    split2<-split[which(split!="0")]
  })
  offset3<-unlist(offset2)

  # Bind all of that
  bind<-cbind(coverage,labels,lc,uc,offset3,units1,epsg2)
  colnames(bind)<- c("Coverage","Axis","Start","End","Resolution","Units","EPSG")
  bind<-as_tibble(bind)
  bind2<-suppressWarnings(dplyr::left_join(bind,Metacols,by="Axis"))
  return(bind2)

}


#' @title Get coordinate system
#' @description Get the coordinate system of a coverage
#' @param coverage character; Name of a coverage
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @export
coverage_get_coordsys <- function(coverage, url = NULL){

  md   <- getMetadata(coverage,url=url)
  axes <- as.character(unique(md$Axis))
  return(axes)

}

#' @title Get EPSG identifier
#' @description Function to extract the EPSG identifier from a WCS WCPS coverage
#' @param coverage character; Name of a coverage
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @export

coverage_get_coordinate_reference <- function(coverage, url=NULL){

  md   <- getMetadata(coverage,url=url)
  epsg <- as.character(unique(md$EPSG))
  return(epsg)

}

#' @title Get Temporal Extent
#' @description Get the temporal extent from a WCS WCPS Coverage
#' @param coverage character; Name of a coverage
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom dplyr filter
#' @importFrom stringr str_replace_all
#' @export

coverage_get_temporal_extent <- function(coverage, url=NULL){

  md      <- getMetadata(coverage,url=url)

  if(!any(md$Type=="Time")) return(NA)

  time    <- filter(md,Type=="Time")
  stamps  <- c(as.character(time$Start),as.character(time$End))
  stamps2 <- str_replace_all(stamps,'\"',"")
  return(stamps2)

}

#' @title Get Bounding Box
#' @description Get a bounding Box of a WCS, WCPS coverage
#' @param coverage character; Name of a coverage
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom dplyr filter arrange
#' @export

coverage_get_bounding_box <- function(coverage, url=NULL){

  md      <- getMetadata(coverage,url=url)
  ext     <- filter(md,Type=="X" | Type=="Y")
  ext     <- arrange(ext,Type)
  stamps  <- c(as.character(ext$Start),as.character(ext$End))
  stamps2 <- stamps[c(1,3,2,4)]
  return(stamps2)

}

#' @title Get Timestamps
#' @description Get the available timestamps of WCS coverages
#' @param coverage character; Name of a coverage
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @param timetype boolean; return the type of storage of time dimension in coverage (regular, irregular, none)?
#' @param verbose boolean; Want to return the processed coverage?
#' @importFrom dplyr filter
#' @importFrom stringr str_split str_replace_all
#' @importFrom lubridate as_datetime
#' @export

coverage_get_timestamps <- function(coverage, url=NULL, timetype=FALSE, verbose=FALSE){

  md   <- parseMetadata(coverage,url=url)
  iscoeff<-filter(md,L3=="coefficients" | L4=="coefficients" | L5=="coefficients" | L6=="coefficients")

  if(nrow(iscoeff)>1) stop("Multiple possible TimeStamps available. Operation stopped")
  if(nrow(iscoeff)==1) {

    vals <-iscoeff[["Value"]]
    replacer<-str_replace_all(vals,'\"',"")
    splitter<-strsplit(replacer," ")[[1]]
    timetype <- "Irregular Time Dimension"

  }

  if(nrow(iscoeff)==0) {

    md1<-getMetadata(coverage)
    time<-filter(md1,Type=="Time")

    if(nrow(time)==0) {
      splitter <- NA
      timetype <- "No Time Dimension"
    } else {

      stamps  <- c(as.character(time$Start),as.character(time$End))
      stamps2 <- str_replace_all(stamps,'\"',"")

      diff<-as.numeric(as.character(time$Resolution))

      dates    <- seq(as_datetime(stamps[1]),as_datetime(stamps[2]),by=diff)
      splitter <- paste0(str_replace_all(as.character(dates)," ","T"),".000Z")
      timetype <- "Regular Time Dimension"

    }
  }

  if(isTRUE(verbose)) print(coverage)
  if(!isTRUE(timetype)) {return(splitter)} else {return(timetype)}

}

#' @title Get Bands
#' @description Get the available bands of one WCS, WCPS coverage
#' @param coverage character; Name of a coverage
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom dplyr filter
#' @export

coverage_get_bands <- function(coverage, url=NULL){

  getMeta <- parseMetadata(coverage,url=url)
  bands<- dplyr::filter(getMeta,L2=="rangeType" & Attributes==".name")[["Value"]]
  return(bands)

}

#' @title Get Resolution
#' @description Get the resolution of one WCS, WCPS coverage
#' @param coverage character; Name of a coverage
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @importFrom dplyr filter
#' @export
coverage_get_resolution <- function(coverage, url=NULL){

  md      <- getMetadata(coverage,url=url)
  ext     <- filter(md,Type=="X" | Type=="Y")
  stamps  <- as.character(ext$Resolution)
  return(stamps)

}

#' @title Parse the whole Rasdaman Environment
#' @description Scrape the data in the whole Rasdaman Environment
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @export
parseEnvironment <- function(url=NULL){

  caps<-getCoverages(url=url)
  lp<-lapply(caps,function(c){

    csys <- paste(coverage_get_coordsys(c,url=url),collapse=" /--/ ")
    cref <- paste(coverage_get_coordinate_reference(c,url=url),collapse=" /--/ ")
    ctex <- paste(coverage_get_temporal_extent(c,url=url),collapse=" /--/ ")
    ctmp <- coverage_get_timestamps(c,url=url)
    cbbx <- paste(coverage_get_bounding_box(c,url=url),collapse=" /--/ ")
    cbnd <- paste(coverage_get_bands(c,url=url),collapse=" /--/ ")
    cres <- paste(coverage_get_resolution(c,url=url),collapse=" /--/ ")

    cb <- cbind(c,csys,cref,ctex,ctmp,cbbx,cbnd,cres)
    colnames(cb)<-c("Coverage",
                    "Coordinate System",
                    "Coordinate Reference",
                    "Temporal Extent",
                    "Time Stamps",
                    "BoundingBox",
                    "Bands",
                    "Resolution")
    return(cb)

  })
  rb<-do.call(rbind,lp)
  return(rb)
}



