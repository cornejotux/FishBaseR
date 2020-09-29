fb_foodconsumption <-
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
    warning("idFB or Genus and Specie are required!")
    }
	if(!is.na(idFB))
		{
		url <- paste(server, "TrophicEco/FoodConsumptionList.php?ID=", idFB, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.null(s$dataTable))
			{
			diet <- c(as.character(idFB), as.character(Genus), as.character(Species), t(rep(NA, 8)))
			names(diet) <-  c('idFB', 'Genus', 'Species',  'Country', 'Q_B', 'Winf', 'K', 'Mortality', 'Temperature', 'FoodType', 'Reference')
			}
		if (!is.null(s$dataTable))
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), s$dataTable)
			names(diet) <-  c('idFB', 'Genus', 'Species',  'Country', 'Q_B', 'Winf', 'K', 'Mortality', 'Temperature', 'FoodType', 'Reference')
			diet$idFB <- as.character(diet$idFB)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$Country <- as.character(diet$Country)
			diet$Q_B <- as.character(diet$Q_B)
			diet$Winf <- as.character(diet$Winf)
			diet$K <- as.character(diet$K)
			diet$Mortality <- as.character(diet$Mortality)
			diet$Temperature <- as.character(diet$Temperature)
      diet$FoodType <- as.character(diet$FoodType)
      diet$Reference <- as.character(diet$Reference)
			}
		}
	return(diet)
	}
