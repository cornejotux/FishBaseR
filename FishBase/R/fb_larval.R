fb_larval <-
function(idFB = NA, StockCode = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.tw/')
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
			warning("idFB and StockCode or Genus and Species MUST be provided!!")
			}
	if (!is.na(idFB) & !is.na(StockCode))
		{
  	url <- paste(server, "Reproduction/LarvaeDynaSummary.php?ID=", idFB, "&StockCode=", StockCode, 
                 "&GenusName=", Genus, "&SpeciesName=", Species, sep = "")
    l <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
	  }
		
	if (is.null(l[[2]]))
		{
    lu <- c(as.character(idFB), as.character(StockCode), as.character(Genus), 
              as.character(Species), t(rep(NA, 12)))
		names(lu) <-  c("idFB", "StockCode", "Genus", "Species", "MainRef", "Ecosystem", 
                      "Temp", "LarvDur", "DryWHatch", "DryWMeta", "GrowthCoef", "MRate", 
                      "O2Comp", "FoodInge", "Comments", "References")
		}
	if (!is.null(l[[2]]))
		{
		l <- l[[2]]
    lu <- matrix(NA, 1,16)
    lu[1,1] <- as.numeric(as.character(idFB))
    lu[1,2] <- as.numeric(as.character(StockCode))
    lu[1,3] <- as.character(Genus)
    lu[1,4] <- as.character(Species)
		lu[1,5] <- as.character(l$V2[1])
		lu[1,6] <- as.character(l$V2[2])
		lu[1,7] <- as.character(l$V2[3])
		lu[1,8] <- as.character(l$V2[4])
		lu[1,9] <- as.character(l$V2[5])
		lu[1,10] <- as.character(l$V2[6])
		lu[1,11] <- as.character(l$V2[7])
		lu[1,12] <- as.character(l$V2[8])
		lu[1,13] <- as.character(l$V2[9])
		lu[1,14] <- as.character(l$V2[10])
		lu[1,15] <- as.character(l$V2[11])
		lu[1,16] <- as.character(l$V2[12])
   
		lu <-  data.frame(lu)
  	names(lu) <-  c("idFB", "StockCode", "Genus", "Species", "MainRef", "Ecosystem", 
                      "Temp", "LarvDur", "DryWHatch", "DryWMeta", "GrowthCoef", "MRate", 
                      "O2Comp", "FoodInge", "Comments", "References")
  }
	return(lu)
	}
