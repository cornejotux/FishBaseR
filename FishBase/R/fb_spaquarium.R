fb_spaquarium <-
function(idCountry = NA, server="http://www.fishbase.tw/")
     {
    require("XML")
    require("stringr")
    require('RCurl')
     if (is.na(idCountry))
           {
           warning("Please provide the Country ID (idCountry)")
           }
     if (!is.na(idCountry))
           {
           if (idCountry<10) {id <- paste("00",idCountry, sep = '')}
           if (idCountry >= 10 & idCountry<100) {id <- paste("0",idCountry,
sep = '')}
           if (idCountry >= 100 & idCountry<1000) {id <- idCountry}
           url <- paste(server,
"Country/CountryAquariumList.php?country=",id, sep = "")
           }
     if (!is.na(url))
           {
           g <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
           b <- g[[2]]
           if (!is.null(dim(g[[2]])))
                 {                       
                 List <- cbind(id, b)
                 names(List) <- c('idCountry', 'SciName', 'LocalName',
'Aq_demand_by', 'Status', 'OtherName')
                 }
           if (is.null(dim(g[[2]])))
                 {
                 List <- matrix(NA, nrow = 1, ncol = 6)
                 List[1,1] <- id
                 List[1,2:6] <- NA
                 List <- as.data.frame(List)
                 names(List) <- c('idCountry', 'SciName', 'LocalName',
'Aq_demand_by', 'Status', 'OtherName')
                 }
           }
     return(List)
     }
