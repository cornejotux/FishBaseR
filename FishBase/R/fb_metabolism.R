fb_metabolism <-
function(idFB = NA, StockCode = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
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
  if ((is.na(idFB) | is.na(StockCode)) & (is.na(Genus) | is.na(Species)))
    {
    warning("idFB and StockCode or Genus and Specie are required!")
    }
	if(!is.na(idFB) & !is.na(StockCode))
		{
		url <- paste(server, "physiology/OxygenDataList.php?ID=", idFB, "&StockCode=", StockCode, sep = "")
		s <- readHTMLTable(url, header = c( 'OxCon', 'OxCon_20degC', 'Wieght', 'Temperature', 
                                        'Salinity', 'Activity', 'Stress'))

		if (is.null(s$dataTable))
			{
			List <- c(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), t(rep(NA, 7)))
			names(List) <-  c('idFB', 'StockCode', 'Genus', 'Species', 'OxCon', 'OxCon_20degC', 'Wieght', 'Temperature', 'Salinity', 'Activity', 'Stress')
			}
		if (!is.null(s$dataTable))
			{
			List <- cbind(as.character(idFB), as.character(StockCode), as.character(Genus), as.character(Species), s$dataTable)
			names(List) <-    c('idFB', 'StockCode', 'Genus', 'Species', 'OxCon', 'OxCon_20degC', 'Weight', 'Temperature', 'Salinity', 'Activity', 'Stress')
			List$idFB <- as.character(List$idFB)
      List$StockCode <- as.character(List$StockCode)
			List$Genus <- as.character(List$Genus)
			List$Species <- as.character(List$Species)
			List$OxCon <- as.character(List$OxCon)
      List$OxCon_20degC <- as.character(List$OxCon_20degC)
			List$Weight <- as.character(List$Weight)
			List$Temperature <- as.character(List$Temperature)
      List$Salinity <- as.character(List$Salinity)
      List$Activity <- as.character(List$Activity)
      List$Stress <- as.character(List$Stress)
			}
		}
	return(List)
	}
