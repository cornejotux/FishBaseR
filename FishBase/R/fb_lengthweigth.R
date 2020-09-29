fb_lengthweigth <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.us/')
	{
require("XML")
require("stringr")
require('RCurl')
	if (is.na(idFB)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server = server)
		idFB <- ids$idFB
		}
  if ((is.na(idFB)) & (is.na(Genus) | is.na(Species)))
    {
    warning("idFB or Genus and Specie are required!")
    }
	if(!is.na(idFB))
		{
		url <- paste(server, "PopDyn/LWRelationshipList.php?ID=", idFB, sep = "")
		lw <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (dim(lw[[3]][-1,])[1] == 0)
			{
			lwu <- data.frame(idFB = as.character(idFB), Genus = as.character(Genus), Species = as.character(Species), 
			                  Score = NA, a = NA, b = NA, Doubful = NA, sex = NA, Length = NA, L_type = NA, 
			                  r2 = NA, SD_b = NA, SD_log10_a = NA, n = NA, Country = NA, Locality = NA, URL=url)
      #t(rep(NA, 13)))
			#names(lwu) <-   c('idFB','Genus', 'Species',  'Score', 'a', 'b', 'Doubful', 'sex', 'Length', 'L_type', 
      #                  'r2', 'SD_b', 'SD_log10_a', 'n','Country', 'Locality')
		
			}
		if (dim(lw[[3]][-1,])[1] != 0)
			{
			lwu <- cbind(as.character(idFB), as.character(Genus), as.character(Species), lw[[3]][-1,], URL=url)
			names(lwu) <-   c('idFB','Genus', 'Species',  'Score', 'a', 'b', 'Doubful', 'sex', 'Length', 'L_type', 
			                  'r2', 'SD_b', 'SD_log10_a', 'n','Country', 'Locality', 'URL')	
		  }
    lwu <- (lwu[,-4])
		}
	return(as.data.frame(lwu))
	}
