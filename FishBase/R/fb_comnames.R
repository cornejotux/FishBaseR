fb_comnames <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
  {
require("XML")
require("stringr")
require('RCurl')
	if (is.na(idFB)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server = server)
		idFB <- ids$idFB
		StockCode <- ids$StockCode
		}
  if (is.na(idFB) & (is.na(Genus) | is.na(Species)))
    {
    print("ERROR: idFB or Genus and Specie are required!")
    }
	if(!is.na(idFB))
		{
		url <- paste(server, "comnames/CommonNamesList.php?ID=", idFB, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.null(s$dataTable))
			{
			List <- c(as.character(idFB), as.character(Genus), as.character(Species), t(rep(NA, 5)))
			names(List) <-  c('idFB', 'Genus', 'Species',  'CommonName', 'UsedIn', 'Language', 'Type', 'OffTradeName')
			}
		if (!is.null(s$dataTable))
			{
			List <- cbind(as.character(idFB),  as.character(Genus), as.character(Species), s$dataTable)
			names(List) <-   c('idFB', 'Genus', 'Species',  'CommonName', 'UsedIn', 'Language', 'Type', 'OffTradeName')
			List$idFB <- as.character(List$idFB)
			List$Genus <- as.character(List$Genus)
			List$Species <- as.character(List$Species)
			List$CommonName <- as.character(List$CommonName)
      List$UsedIn <- as.character(List$UsedIn)
			List$Language <- as.character(List$Language)
			List$Type <- as.character(List$Type)
      List$OffTradeName <- as.character(List$OffTradeName)
			}
		}
	return(List)
	}
