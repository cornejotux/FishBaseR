fb_agesize <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
	{
require("XML")
require("stringr")
require('RCurl')
	if (is.na(idFB)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server = server)
		idFB <- ids$idFB
		Age_Size <- c(as.character(idFB), as.character(Genus), as.character(Species), t(rep(NA, 6)))
		names(Age_Size) <-  c('idFB', 'Genus', 'Species', 'Sex', 'Wmax', 'Lmax', 'Tmax', 'Country', 'Locality')
		Age_Size <- t(Age_Size)
		}
  if ((is.na(idFB) | !is.numeric(idFB)) & (is.na(Genus) | is.na(Species)))
    {
    warning("a numeric idFB or Genus and Specie are required!")
    }
		url <- paste(server,"PopDyn/PopCharList.php?ID=", idFB, sep = "")
		age <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.list(age))
			{
			Age_Size <- c(as.character(idFB), as.character(Genus), as.character(Species), t(rep(NA, 6)))
			names(Age_Size) <-  c('idFB', 'Genus', 'Species', 'Sex', 'Wmax', 'Lmax', 'Tmax', 'Country', 'Locality')
			Age_Size <- t(Age_Size)
			}
		if (!is.null(age$dataTable))
			{
			Age_Size <- cbind(as.character(idFB), as.character(Genus), as.character(Species), age$dataTable)
			names(Age_Size) <-  c('idFB', 'Genus', 'Species', 'Sex', 'Wmax', 'Lmax', 'Tmax', 'Country', 'Locality')
			}
	return(as.data.frame(Age_Size))
	}
