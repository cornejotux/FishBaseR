fb_gillarea <-
function(idFB = NA, StockCode = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
  {
require("XML")
require("stringr")
require('RCurl')
  if (is.na(idFB) | is.na(StockCode)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server = server)
		idFB <- ids$idFB
		StockCode <- ids$StockCode
		}
  if ((is.na(idFB) | is.na(StockCode)) & (is.na(Genus) | is.na(Species)))
    {
    warning("idFB and StockCode or Genus and Specie are required!")
    }
	if(!is.na(idFB) & !is.na(StockCode))
		{
		url <- paste(server, "physiology/GillAreaDataList.php?ID=", idFB, 
                 "&StockCode=", StockCode, sep = "")
    s <- readHTMLTable(url, as.data.frame=TRUE, colnames = c('BodyWeight', 'GillArea', 
                        'GillAreaWeight', 'GAWReference', 'BlodWaterDist', 'BWDReference'))

		if (dim(s[[2]])[1] == 2)
			{
			diet <- c(as.character(idFB), as.character(Genus), 
                as.character(Species), t(rep(NA, 6)))
			names(diet) <-  c('idFB', 'Genus', 'Species', 'BodyWeight', 'GillArea', 
                        'GillAreaWeight', 'GAWReference', 'BlodWaterDist', 'BWDReference')
			}
		if (dim(s[[2]])[1] >= 3)
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), 
                    s[[2]])
			names(diet) <-   c('idFB', 'Genus', 'Species', 'BodyWeight', 'GillArea', 
                        'GillAreaWeight', 'GAWReference', 'BlodWaterDist', 'BWDReference')
			diet <- diet[-(1:2),]
      diet$idFB <- as.character(diet$idFB)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$BodyWeight <- as.character(diet$BodyWeight)
			diet$GillArea <- as.character(diet$GillArea)
			diet$GillAreaWeight <- as.character(diet$GillAreaWeight)
			diet$GAWReference <- as.character(diet$GAWReference)
      diet$BlodWaterDist <- as.character(diet$BlodWaterDist)
      diet$BWDReference <- as.character(diet$BWDReference)
		}
	}
  return(diet)
}
