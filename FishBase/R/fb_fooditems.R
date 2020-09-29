fb_fooditems <-
function(idFB = NA, StockCode = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
	{
require("XML")
require("stringr")
require('RCurl')
	if (is.na(StockCode)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server = server)
		idFB <- ids$idFB
		StockCode <- ids$StockCode
		}
  if (is.na(StockCode) & (is.na(Genus) | is.na(Species)))
    {
    warning("StockCode or Genus and Specie are required!")
    }
	if(!is.na(StockCode))
		{
		url <- paste(server, "TrophicEco/FoodItemsList.php?vstockcode=", StockCode, '&genus=', Genus, '&species=', Species, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (is.null(s$dataTable))
			{
			diet <- c(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), t(rep(NA, 6)))
			names(diet) <-  c('idFB', 'StockCode', 'Genus', 'Species',  'Food_I', 'Food_II', 'Food_III', 'Food_name', 'Country', 'Pred_Stage')
			}
		if (!is.null(s$dataTable))
			{
			diet <- cbind(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), s$dataTable)
			names(diet) <-   c('idFB', 'StockCode', 'Genus', 'Species', 'Food_I', 'Food_II', 'Food_III', 'Food_name', 'Country', 'Pred_Stage')
			diet$idFB <- as.character(diet$idFB)
			diet$StockCode <- as.character(diet$StockCode)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$Food_I <- as.character(diet$Food_I)
			diet$Food_II <- as.character(diet$Food_II)
			diet$Food_III <- as.character(diet$Food_III)
			diet$Food_name <- as.character(diet$Food_name)
			diet$Country <- as.character(diet$Country)
			diet$Pred_Stage <- as.character(diet$Pred_Stage)
			}
		}
	return(diet)
	}
