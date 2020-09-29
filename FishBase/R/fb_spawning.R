fb_spawning <-
function(idFB = NA, StockCode=NA,  Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
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
		url <- paste(server, "Reproduction/SpawningList.php?ID=", idFB, "&StockCode=", 
                 StockCode, sep = "")
		s <- readHTMLTable(url, colnames=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dic', 
                                       'Country', 'Locality'))
		if (is.null(s$dataTable))
			{
			diet <- c(as.character(idFB), as.character(StockCode), as.character(Genus), 
                as.character(Species), t(rep(NA, 14)))
			names(diet) <-  c('idFB', 'StockCode', 'Genus', 'Species', 'Jan', 'Feb', 'Mar', 
                        'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dic', 
                        'Country', 'Locality')
			}
		if (!is.null(s$dataTable))
			{
			diet <- cbind(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), s$dataTable)
			names(diet) <-    c('idFB', 'StockCode', 'Genus', 'Species', 'Jan', 'Feb', 'Mar', 
                        'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dic', 
                        'Country', 'Locality')
			diet$idFB <- as.character(diet$idFB)
    	diet$StockCode <- as.character(diet$StockCode)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$Jan <- as.character(diet$Jan)
			diet$Feb <- as.character(diet$Feb)
			diet$Mar <- as.character(diet$Mar)
			diet$Apr <- as.character(diet$Apr)
      diet$May <- as.character(diet$May)
      diet$Jun <- as.character(diet$Jun)
      diet$Jul <- as.character(diet$Jul)
      diet$Aug <- as.character(diet$Aug)
      diet$Sep <- as.character(diet$Sep)
      diet$Oct <- as.character(diet$Oct)
      diet$Nov <- as.character(diet$Nov)
      diet$Dic <- as.character(diet$Dic)
      diet$Country <- as.character(diet$Country)
      diet$Locality <- as.character(diet$Locality)
			}
		}
	return(diet)
	}
