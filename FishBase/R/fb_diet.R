fb_diet <-
function(idFB = NA, StockCode = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
	{
require("XML")
require("stringr")
require('RCurl') 
	if (is.na(StockCode) | is.na(idFB)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server = server)
		idFB <- ids$idFB
		StockCode <- ids$StockCode
		}
if (is.na(idFB) | !is.numeric(idFB) | 
  is.na(StockCode) | !is.numeric(StockCode))
    {
    warning("a numeric idFB and StockCode or Genus and Specie are required!")
    }
if(!is.na(StockCode) & !is.na(idFB) & is.numeric(StockCode) & is.numeric(idFB))
		{
		url <- paste(server, "TrophicEco/DietCompoList.php?StockCode=", StockCode, '&ID=', idFB, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.null(s$dataTable))
			{
			diet <- c(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), t(rep(NA, 7)))
			names(diet) <-  c('idFB', 'StockCode', 'Genus', 'Species',  'MainFood', 'Percent', 'Trophic', 'Predator', 'Country', 'Locality', 'Reference')
			}
		if (!is.null(s$dataTable))
			{
			diet <- cbind(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), s$dataTable)
			names(diet) <-   c('idFB', 'StockCode', 'Genus', 'Species',  'MainFood', 'Percent', 'Trophic', 'Predator', 'Country', 'Locality', 'Reference')
			diet$idFB <- as.character(diet$idFB)
			diet$StockCode <- as.character(diet$StockCode)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$MainFood <- as.character(diet$MainFood)
			diet$Percent <- as.character(diet$Percent)
			diet$Trophic <- as.character(diet$Trophic)
			diet$Predator <- as.character(diet$Predator)
			diet$Country <- as.character(diet$Country)
			diet$Locality <- as.character(diet$Locality)
      diet$Reference <- as.character(diet$Reference)
			}
		}
	return(diet)
	}
