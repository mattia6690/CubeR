#' @title Pixel History
#' @description Pixel history of "bands" (coverage bands) for a given point determined "coords"
#' @param coverage name of the coverage [character]
#' @param coord_sys coordinate system [character]
#' @param bands coverage bands to calculate and visualize pixel history [character]
#' @param coords coordinates of the location of interest [character]
#' @param date date range in format (Ymd) [character]
#' @param desc_url Web Coverage Service (WCS) DescribeCoverage url [character].
#' This URL can be built with the *createWCS_URLs* function
#' @param pixel_url Web Coverage Service (WCS) for processing the query [character].
#' This URL can be built with the *createWCS_URLs* function
#' @import stringr
#' @import magrittr
#' @import urltools
#' @import httr
#' @export

pixel_history <- function(coverage, coord_sys, bands, coords, date = NULL,
                          desc_url=NULL,pixel_url=NULL){


  if(is.null(desc_url)) desc_url<-createWCS_URLs(type="Meta")
  if(is.null(pixel_url)) pixel_url<-createWCS_URLs(type="Pixel")

  times<-coverage_get_timestamps(desc_url,coverage)

  if(is.null(date)){

    times <- times %>% as.Date()

    start_date_mod <- times[1]
    end_date_mod <- times[length(times)]

  } else {

    start_date_mod = date[1]
    end_date_mod = date[2]

    times <- times[times >= start_date_mod & times <= end_date_mod] %>% as.Date()

  }

  bands_len <- length(bands)

  if(bands_len == 0){

    p <- plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    p <- text(1,1, "Can't visualize pixel history for selected coverage, reason: No band selected!", cex = 2)

  } else {

    responses = list()

    max_res = list()

    for(k in 1:bands_len){

      query <-str_c('for c in ( ',coverage,' ) return encode( c.',bands[k],'[',coord_sys[1],'(',coords[1],'),',coord_sys[2],'(',coords[2],
                    '),', coord_sys[3], '("', start_date_mod,'":"', end_date_mod,'")],"csv")')

      query_encode  <- urltools::url_encode(query)
      request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")
      r             <- GET(request)

      res <- content(r, "text")

      num <- str_replace_all(res,"\\{","") %>%
        str_replace_all(.,"\\}","") %>%
        str_replace_all(.,"\"","") %>%
        str_split(.,",") %>% unlist() %>%
        str_split(.," ") %>% unlist() %>%
        as.numeric()

      responses[[k]] = num

      max_res[[k]] = max(num)

    }

    abs_max <- max(na.omit(unlist(max_res)))

    p <- plot(times,responses[[1]], type="o", col = rainbow(1)[1], lwd = 2, xlab="Date", ylab="Single channels", ylim = c(0,abs_max),
              cex.axis = 1.2, cex.lab = 1.2)

    if(length(responses) == 1){

      p<-legend("topright", inset = .02,legend=c(bands), col = c(rainbow(length(responses))), pch=15)

    } else{

      for(l in 2:length(responses)){
        p <- lines(times, responses[[l]], type = "o", col = rainbow(l)[l], lwd = 2)

      }

      p<-legend("topright", inset = .02,legend=c(bands), col = c(rainbow(length(responses))), pch=15)

    }

  }

  return(p)

}
