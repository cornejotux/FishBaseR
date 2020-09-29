fb_ecosystems <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
	{
require("XML")
require("stringr")
require('RCurl')
	if (is.na(idFB) | !is.numeric(idFB)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server = server)
		idFB <- ids$idFB
		StockCode <- ids$StockCode
		}
  if ((is.na(idFB) | !is.numeric(idFB)) & 
    (is.na(Genus) | is.na(Species)))
    {
    warning("Genus and Specie are required!")
    }
	if(!is.na(idFB))
		{
		url <- paste(server, "trophiceco/EcosysList.php?ID=", idFB, "&GenusName=",Genus, "&SpeciesName=", Species, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.null(s$dataTable))
			{
			List <- c(as.character(idFB), as.character(Genus), as.character(Species), t(rep(NA, 4)))
			names(List) <-  c('idFB', 'Genus', 'Species',  'Ecosystem', 'Type', 'Status', 'References')
			}
		if (!is.null(s$dataTable))
			{
			List <- cbind(as.character(idFB),  as.character(Genus), as.character(Species), s$dataTable)
			names(List) <-  c('idFB', 'Genus', 'Species',  'Ecosystem', 'Type', 'Status', 'References')
			List$idFB <- as.character(List$idFB)
			List$Genus <- as.character(List$Genus)
			List$Species <- as.character(List$Species)
			List$Ecosystem <- as.character(List$Ecosystem)
      List$Type <- as.character(List$Type)
			List$Status <- as.character(List$Status)
			List$References <- as.character(List$References)
			}
		}
	return(List)
	}
