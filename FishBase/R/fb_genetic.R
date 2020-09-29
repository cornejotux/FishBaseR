fb_genetic <-
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
  if ((is.na(idFB)) & (is.na(Genus) | is.na(Species)))
    {
    warning("idFB or Genus and Specie are required!")
    }
	if(!is.na(idFB))
		{
		url <- paste(server, "Genetics/FishGeneticsList.php?ID=", idFB, sep = "")
    s <- readHTMLTable(url, as.data.frame=TRUE, colnames = c('idFB', 'Genus', 
                        'Species', 'Locality', 'Country', 'HaploidGam', 
                        'Diploid_zyg','GenMarkers', 'Reference'))

		if (dim(s[[4]])[1]==2)
			{
			list <- c(as.character(idFB), as.character(Genus), 
                as.character(Species), t(rep(NA, 7)))
			names(list) <-  c('idFB', 'Genus', 'Species', 'Locality', 'Country', 'HaploidGam', 
                        'DiploidZygI','DiploidZygII','GenMarkers', 'Reference')
			}
		if (dim(s[[4]])[1]>=3)
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), 
                    s[[4]])
			names(diet) <-   c('idFB', 'Genus', 'Species', 'Locality', 'Country', 'HaploidGam', 
                        'Diploid_zyg','GenMarkers', 'Reference')
			diet$idFB <- as.character(diet$idFB)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$Locality <- as.character(diet$Locality)
			diet$Country <- as.character(diet$Country)
			diet$HaploidGam <- as.character(diet$HaploidGam)
			diet$Diploid_zyg <- str_split(as.character(diet$Diploid_zyg), '-')
      diet$GenMarkers <- as.character(diet$GenMarkers)
      diet$Reference <- as.character(diet$Reference)
      diet <- diet[-(1:2),]
      diet$DiploidZygI <- NA
      diet$DiploidZygII <- NA
      for (i in 1:length(diet$Diploid_zyg))
        {
        diet$DiploidZygI[i] <- diet$Diploid_zyg[[i]][1]
        diet$DiploidZygII[i] <- diet$Diploid_zyg[[i]][2]
        }
      diet <- diet[,-7]
      list <- as.data.frame(cbind(diet$idFB, diet$Genus, diet$Species, diet$Locality, diet$Country,
                    diet$HaploidGam, diet$DiploidZygI, diet$DiploidZygII,
                    diet$GenMarkers, diet$Reference))
      names(list) <-   c('idFB', 'Genus', 'Species', 'Locality', 'Country', 'HaploidGam', 
                        'DiploidZygI','DiploidZygII','GenMarkers', 'Reference')
		}
	}
  return(list)
}
