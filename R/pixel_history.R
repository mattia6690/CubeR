#' @title Pixel History
#' @description Returns a pixel history of one or multiple bands from one coverage.
#' @param coverage character; name of the coverage
#' @param coord_sys character; coordinate system
#' @param bands character; coverage bands to calculate and visualize pixel history
#' @param coords character; coordinates of the location of interest in c(Lat,Lon)
#' @param date character; date range in format (Ymd)
#' @param url character; Web Coverage Service (WCS) Url. If NULL then it is directing to the SAO homepage ("http://saocompute.eurac.edu/rasdaman/ows")
#' @param plot boolean; Should the data be plotted
#' @importFrom httr GET content
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_replace_all str_split
#' @importFrom urltools url_encode
#' @importFrom grDevices rainbow
#' @export

pixel_history <- function(coverage, coord_sys, bands, coords, date = NULL,
                          url=NULL,plot=F){


  desc_url<-createWCS_URLs(type="Meta",url=url)
  query_url<-createWCS_URLs(type="Query",url=url)

  times<-coverage_get_timestamps(desc_url,coverage)

  if(is.null(date)){

    times <- as.Date(times)

    start_date_mod <- times[1]
    end_date_mod <- times[length(times)]

  } else {

    start_date_mod = date[1]
    end_date_mod = date[2]

    times <- times[times >= start_date_mod & times <= end_date_mod] %>% as.Date

  }

  bands_len <- length(bands)

  if(bands_len == 0) stop("No Bands Found")

  responses = list()
  max_res = list()

  for(k in 1:bands_len){

    query <-str_c('for c in ( ',coverage,' ) return encode( c.',bands[k],
                  '[',
                  coord_sys[1],'(',coords[1],'),',
                  coord_sys[2],'(',coords[2],'),',
                  coord_sys[3], '("',
                  start_date_mod,'":"', end_date_mod,'")],"csv")')

    query_encode  <- urltools::url_encode(query)
    request       <- paste(query_url, query_encode, collapse = NULL, sep="")

    r             <- GET(request)
    res           <- content(r, "text")

    num <- str_replace_all(res,"\\{","") %>%
      str_replace_all(.,"\\}","") %>%
      str_replace_all(.,"\"","") %>%
      str_split(.,",") %>% unlist() %>%
      str_split(.," ") %>% unlist() %>%
      as.numeric()

    responses[[k]] = num
    max_res[[k]] = max(num)

  }

  resp<-do.call(cbind.data.frame,responses)
  resp<-cbind.data.frame(times,resp)
  names(resp)<-c("Date",bands)


  if(plot==T){
    abs_max <- responses %>% unlist %>% as.numeric %>% max

    p <- plot(times,responses[[1]], type="o", col = rainbow(1)[1], lwd = 2, xlab="Date", ylab="Single channels", ylim = c(0,abs_max),
              cex.axis = 1.2, cex.lab = 1.2)

    if(length(responses) == 1){

      p<-legend("topright", inset = .02,legend=c(bands), col = c(rainbow(length(responses))), pch=15)

    } else{

      for(l in 2:length(responses)) p <- lines(times, responses[[l]], type = "o", col = rainbow(l)[l], lwd = 2)
      p<-legend("topright", inset = .02,legend=c(bands), col = c(rainbow(length(responses))), pch=15)

    }

  } else return(resp)

}
