fb_introductions <-
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
  if ((is.na(idFB) & is.na(StockCode)) & (is.na(Genus) | is.na(Species)))
    {
    warning("idFB and StockCode or Genus and Specie are required!")
    }
	if (!is.na(idFB) & !is.na(StockCode))
		{
		url <- paste(server, "Introductions/IntroductionsList.php?ID=", idFB, "&StockCode=", StockCode, 
                 "&GenusName=", Genus, "&SpeciesName=", Species, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.null(s[[3]]))
			{
			List <- c(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), t(rep(NA, 5)))
			names(List) <-  c('idFB', 'StockCode', 'Genus', 'Species',  'Year_Period', 'From', 'To', 'Established', 'EcolEffects')
			}
		if (!is.null(s[[3]]))
			{
			List <- cbind(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), s[[3]])
			names(List) <-  c('idFB', 'StockCode', 'Genus', 'Species',  'Year_Period', 'From', 'To', 'Established', 'EcolEffects')
			List$idFB <- as.character(List$idFB)
      List$StockCode <- as.character(List$StockCode)
			List$Genus <- as.character(List$Genus)
			List$Species <- as.character(List$Species)
			List$Year_Period <- as.character(List$Year_Period)
      List$From <- as.character(List$From)
			List$To <- as.character(List$To)
			List$Established <- as.character(List$Established)
      List$EcolEffects <- as.character(List$EcolEffects)
			}
		}
	return(List)
	}
