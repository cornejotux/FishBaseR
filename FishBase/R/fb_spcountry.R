fb_spcountry <-
function(idCountry = NA, server = "http://www.fishbase.de/")
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
    id <- idCountry
    if(is.numeric(idCountry))
      {
		  if (idCountry<10) {id <- paste("00",idCountry, sep = '')}
		  if (idCountry >= 10 & idCountry<100) {id <- paste("0",idCountry, sep = '')}
		  if (idCountry >= 100 & idCountry<1000) {id <- idCountry}
      }    
		a <- postForm(paste(server, "Country/CountryChecklist.php", sep=''), 
			what="list", 
			trpp="999999", 
			c_code=id, 	
			sortby="alpha2",	 
			vhabitat="all2", 
			style="post")
		b <- str_replace_all(a, "\t", "")
		b <- str_replace_all(b, "\r", "")
		b <- str_replace_all(b, "\n", "")
		c <- readHTMLTable(b)[[4]]
		c$SPname <- as.character(c$V3)
		if (!is.null(dim(c)))
			{
			c$idCountry <- id
			c$Genus <- NA
			c$Species <- NA
			c <- c[-1,]
			List <- cbind(id, c[1], c[2], c[8], c[9], c[4], c[5], c[6])
			names(List) <- c('idCountry', 'Order', 'Family', 'Genus', 'Species', 'Status', 'FB_name', 'Common_name')
			List$Genus <- as.character(List$Genus)
			List$Species <- as.character(List$Species)
			List$FB_name <- as.character(List$FB_name)
			List$Common_name <- as.character(List$Common_name)
			temp <- strsplit(c$SPname, " ")
			for (i in 1:length(temp))
				{
				List$Genus[i] <- temp[[i]][1]
				List$Species[i] <- temp[[i]][2]
				}
			}
		if (is.null(dim(c)))
			{
			List <- matrix(NA, nrow = 1, ncol = 8)
			List[1,1] <- id
			List <- as.data.frame(List)
			names(List) <- c('idCountry', 'Order', 'Family', 'Genus', 'Species', 'Status', 'FB_name', 'Common_name')
			}
		}
	return(List)
	}
