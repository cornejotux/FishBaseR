fb_allefreq <-
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
  if ((is.na(idFB) | !is.numeric(idFB)) & (is.na(Genus) | is.na(Species)))
    {
    warning("a numeric idFB or Genus and Specie are required!")
    }
	if(!is.na(idFB) & is.numeric(idFB))
		{
		url <- paste(server, "Genetics/FishElecStudiesList.php?ID=", idFB, sep = "")
    s <- readHTMLTable(url, as.data.frame=TRUE, colnames = c('No', 'Locality', 'Country', 
                        'TotalLoci','HeterObs', 'HeterExp', 'PolyMoLoci'))

		if (is.null(s[[5]]))
			{
			diet <- c(as.character(idFB), as.character(Genus), 
                as.character(Species), t(rep(NA, 7)))
			names(diet) <-  c('idFB', 'Genus', 'Species', 'No', 'Locality', 'Country', 
                        'TotalLoci','HeterObs', 'HeterExp', 'PolyMoLoci')
			}
		if (!is.null(s[[5]]))
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), 
                    s[[5]])
			names(diet) <-   c('idFB', 'Genus', 'Species', 'No', 'Locality', 'Country', 
                        'TotalLoci','HeterObs', 'HeterExp', 'PolyMoLoci')
			diet$idFB <- as.character(diet$idFB)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$No <- as.character(diet$No)
			diet$Locality <- as.character(diet$Locality)
			diet$Country <- as.character(diet$Country)
			diet$TotalLoci <- str_split(as.character(diet$TotalLoci), '-')
      diet$HeterObs <- as.character(diet$HeterObs)
      diet$HeterExp <- as.character(diet$HeterExp)
      diet$PolyMoLoci <- as.character(diet$PolyMoLoci)
      diet <- diet[,-4]
		}
	}
  return(diet)
}
