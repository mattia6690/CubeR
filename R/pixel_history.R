#' @title Pixel History
#' @description Returns a pixel history of one or multiple bands from one coverage.
#' @param coverage character; name of the coverage
#' @param coords character; coordinates of the location of interest in the native coverage projection. If NULL the middle coordinate will be taken
#' @param bands character; coverage bands to calculate and visualize pixel history
#' @param date character; date range in format (Ymd)
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @param encoding character; Encoding for the response. "UTF-8" as standard
#' @importFrom httr GET content
#' @importFrom urltools url_encode
#' @importFrom dplyr filter left_join bind_cols
#' @importFrom tibble as_tibble
#' @export

pixel_history <- function(coverage, coords=NULL, bands=NULL, date = NULL,
                          url=NULL,encoding="UTF-8"){

  # Options
  options(scipen=100)

  # Build URL
  query_url<-createWCS_URLs(type="Query",url=url)

  # Use buildin functions
  coord_sys   <- coverage_get_coordsys(coverage,url=url)
  bands.avail <- coverage_get_bands(coverage,url=url)

  if(is.null(bands)) bands <- bands.avail
  if(is.null(date))  date  <- coverage_get_temporal_extent(coverage,url=url)
  NoTemp<- all(is.na(date))
  if(is.null(coords)) {

    cbb   <- as.numeric(coverage_get_bounding_box(coverage,url=url))
    cbb1  <- mean(c(cbb[1],cbb[2]))
    cbb2  <- mean(c(cbb[3],cbb[4]))
    coords<- as.character(c(cbb1,cbb2))

  }


  # X/Y/Date Queries
  c1 <- bind_cols(Type=c("X","Y"),coords=coords)
  lj <- left_join(getMetadata(coverage,url=url),c1,by="Type")

  x <-  filter(lj,Type=="X")
  query.x <- paste0(x$Axis,'(',x$coords,')')
  y <-  filter(lj,Type=="Y")
  query.y <- paste0(',',y$Axis,'(',y$coords,')')


  if(!isTRUE(NoTemp)) {
    d <- filter(lj,Type=="Time")
    dateS   <- as.Date(date)
    query.d <- paste0(',',d$Axis,'("',dateS[1],'":"', dateS[2],'")')

    timestamps  <- as.Date(coverage_get_timestamps(coverage,url=url))
    timestampsW <- timestamps>=dateS[1] & timestamps<=dateS[2]
    timestamps2 <- as.character(timestamps[timestampsW])

  } else {

    query.d<-NULL
    timestamps2<-NA

    }

  # Band Queries

  if(length(bands.avail)>1){query.b<-paste0("c.",bands)} else {query.b<-"c"}

  # Bind queries
  queries<- paste0('for c in ( ',coverage,' ) return encode( ',query.b,
                   '[',query.x,query.y,query.d,'],"text/csv")')


  l1<-lapply(c(1:length(queries)),function(x){

    encode  <- urltools::url_encode(queries[x])
    request <- paste(query_url, encode, collapse = NULL, sep="")

    get   <- GET(request)
    con   <- content(get, "text",encoding = encoding)

    if(grepl("Exception",con)) {
      num<- "Exception occurred : Check Input"
    } else {
      num <- strsplit(con,",")[[1]]
    }

    bind<-cbind(coverage,timestamps2,bands[x],num)
    return(bind)

  })

  return<-do.call(rbind,l1)
  return<-as_tibble(return)
  return<-setNames(return,c("Coverage","TimeStamp","Band","Value"))
  return<-arrange(return, Coverage, TimeStamp,Band)
  return(return)

}
