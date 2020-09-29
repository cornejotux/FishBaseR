fb_ecotox <-
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
		url <- paste(server, "Physiology/CL50List.php?ID=", idFB, sep = "")
		s <- readHTMLTable(url, colnames=c('ChemName', 'CL50', 'ExpTime', 'Reference'))
		if (is.null(s[[4]]))
			{
			diet <- c(as.numeric(idFB), as.character(Genus), 
                as.character(Species), t(rep(NA, 4)))
			names(diet) <-  c('idFB', 'Genus', 'Species',  'ChemName', 'CL50',
                        'ExpTime', 'Reference')
			}
		if (!is.null(s[[4]]))
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), s[[4]])
			names(diet) <-   c('idFB', 'Genus', 'Species',  'ChemName', 'CL50',
                        'ExpTime', 'Reference')
			diet$idFB <- as.numeric(as.character(diet$idFB))
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$ChemName <- as.character(diet$ChemName)
			diet$CL50 <- as.character(diet$CL50)
			diet$ExpTime <- as.character(diet$ExpTime)
			diet$Reference <- as.character(diet$Reference)
			}
		}
	return(diet)
	}
