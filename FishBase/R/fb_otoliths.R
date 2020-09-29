fb_otoliths <-
function(idFB = NA,  Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
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
  if ((is.na(idFB)) & (is.na(Genus) | is.na(Species)))
    {
    warning("idFB or Genus and Specie are required!")
    }
	if(!is.na(idFB))
		{
		url <- paste(server, "physiology/OtolithsSummary.php?speccode=", idFB, sep = "")
    s <- readHTMLTable(url, as.data.frame=TRUE)
 		if (length(s)==1)
			{
			diet <- c(as.character(idFB), as.character(Genus), 
                as.character(Species), t(rep(NA, 8)))
			names(diet) <-  c('idFB', 'Genus', 'Species', 'Type', 'FishLength', 
                        'OtolithLenght', 'OtolithHeight', 'Face', 'Position', 
                        'Picture', 'Locality')
			}
		if (length(s)>=2)
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), 
                    s[[3]])
			names(diet) <-   c('idFB', 'Genus', 'Species', 'Type', 'FishLength', 
                        'OtolithLenght', 'OtolithHeight', 'Face', 'Position', 
                        'Picture', 'Locality')
			diet <- diet[-(1),]
      diet$idFB       <- as.character(diet$idFB)
			diet$Genus      <- as.character(diet$Genus)
			diet$Species    <- as.character(diet$Species)
			diet$Type       <- as.character(diet$Type)
			diet$FishLength <- as.character(diet$FishLength)
			diet$OtolithLenght <- as.character(diet$OtolithLenght)
			diet$OtolithHeight <- as.character(diet$OtolithHeight)
      diet$Face       <- as.character(diet$Face)
      diet$Position   <- as.character(diet$Position)
      diet$Picture    <- as.character(diet$Picture)
      diet$Locality   <- as.character(diet$Locality)
      diet <- diet[,-11]
		}
	}
  return(diet)
}
