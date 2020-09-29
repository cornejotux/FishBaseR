fb_synonyms <-
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
		url <- paste(server, "Nomenclature/SynonymsList.php?ID=", idFB, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.null(s[[5]]))
			{
			List <- c(as.character(idFB), as.character(Genus), as.character(Species), t(rep(NA, 7)))
			names(List) <-  c('idFB', 'Genus', 'Species', 'Nada', 'Synonym', 'Author', 'Status', 'Valid', 'Synonymy', 'Combination')
			}
		if (!is.null(s[[5]]))
			{
			List <- cbind(as.character(idFB),  as.character(Genus), as.character(Species), s[[5]])
			names(List) <-   c('idFB', 'Genus', 'Species', 'Nada', 'Synonym', 'Author', 'Status', 'Valid', 'Synonymy', 'Combination')
			List$idFB <- as.character(List$idFB)
			List$Genus <- as.character(List$Genus)
			List$Species <- as.character(List$Species)
			List$Nada <- as.character(List$Nada)
      List$Synonym <- as.character(List$Synonym)
			List$Author <- as.character(List$Author)
			List$Status <- as.character(List$Status)
      List$Valid <- as.character(List$Valid)
      List$Synonymy <- as.character(List$Synonymy)
      List$Combination <- as.character(List$Combination)
      List <- List[,-4]
			}
		}
	return(List)
	}
