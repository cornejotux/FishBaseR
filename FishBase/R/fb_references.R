fb_references <-
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
		url <- paste(server, "References/SummaryRefList.php?ID=", idFB, sep = "")
    s <- readHTMLTable(url, as.data.frame=TRUE)

		if (is.null(s$dataTable))
			{
			diet <- c(as.character(idFB), as.character(Genus), 
                as.character(Species), t(rep(NA, 5)))
			names(diet) <-  c('idFB', 'Genus', 'Species', 'idRef', 'Reference', 'Year', 
                        'SpNameUsed','Page')
			}
		if (!is.null(s$dataTable))
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), 
                    s$dataTable)
			names(diet) <-   c('idFB', 'Genus', 'Species', 'idRef', 'Reference', 'Year', 
                        'SpNameUsed','Page')
			diet$idFB <- as.character(diet$idFB)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$idRef <- as.character(diet$idRef)
			diet$Reference <- as.character(diet$Reference)
			diet$Year <- as.character(diet$Year)
			diet$SpNameUsed <- as.character(diet$SpNameUsed)
      diet$Page <- as.character(diet$Page)
		}
	}
  return(diet)
}
