fb_predators <-
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
		url <- paste(server, "TrophicEco/PredatorList.php?ID=", idFB, sep = "")
		s <- readHTMLTable(url, header = c( 'Country', 'FunGroupI', 'FunGroupII', 'Family', 
                                        'Sp_Name'))
		if (is.null(s$dataTable))
			{
			diet <- c(as.character(idFB), as.character(Genus), 
                as.character(Species), t(rep(NA, 5)))
			names(diet) <-  c('idFB', 'Genus', 'Species',  'Country', 'FunGroupI',
                        'FunGroupII', 'Family', 'Sp_Name')
			}
		if (!is.null(s$dataTable))
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), s$dataTable)
			names(diet) <-   c('idFB', 'Genus', 'Species',  'Country', 'FunGroupI',
                        'FunGroupII', 'Family', 'Sp_Name')
			diet$idFB <- as.character(diet$idFB)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$Country <- as.character(diet$Country)
			diet$FunGroupI <- as.character(diet$FunGroupI)
			diet$FunGroupII <- as.character(diet$FunGroupII)
			diet$Family <- as.character(diet$Family)
			diet$Sp_Name <- as.character(diet$Sp_Name)
			}
		}
	return(diet)
	}
