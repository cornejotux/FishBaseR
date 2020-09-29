fb_ration <-
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
	if(!is.na(idFB))
		{
		url <- paste(server, "TrophicEco/RationList.php?ID=", idFB, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.null(s$dataTable))
			{
			diet <- c(as.character(idFB), as.character(Genus), as.character(Species), t(rep(NA, 7)))
			names(diet) <-  c('idFB', 'Genus', 'Species',  'Weight', 'Ration', 'K1', 'EvacRate', 'Temperature', 'Salinity', 'FoodI')
			}
		if (!is.null(s$dataTable))
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), s$dataTable)
			names(diet) <-  c('idFB', 'Genus', 'Species',  'Weight', 'Ration', 'K1', 'EvacRate', 'Temperature', 'Salinity', 'FoodI')
			diet$idFB <- as.character(diet$idFB)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$Weight <- as.character(diet$Weight)
			diet$Ration <- as.character(diet$Ration)
			diet$K1 <- as.character(diet$K1)
			diet$EvacRate <- as.character(diet$EvacRate)
			diet$Temperature <- as.character(diet$Temperature)
      diet$Salinity <- as.character(diet$Salinity)
      diet$FoodI <- as.character(diet$FoodI)
			}
		}
	return(diet)
	}
