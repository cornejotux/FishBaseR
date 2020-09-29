fb_lengthlength <-
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
		url <- paste(server, "PopDyn/LLRelationshipList.php?ID=", idFB, sep = "")
    s <- readHTMLTable(url, as.data.frame=TRUE)

		if (dim(s[[2]])[1]==1)
			{
			diet <- c(as.character(idFB), as.character(Genus), 
                as.character(Species), t(rep(NA, 7)))
			names(diet) <-  c('idFB', 'Genus', 'Species', 'UnkLength', 'a', 'b', 'KnownLength', 
                        'r', 'LengthRange', 'Sex')
			}
		if (dim(s[[2]])[1]>=2)
			{
			diet <- cbind(as.character(idFB), as.character(Genus), as.character(Species), s[[2]])
			names(diet) <-   c('idFB', 'Genus', 'Species', 'UnkLength', 'a', 'b', 'KnownLength', 'r', 'LengthRange',
                                       'Sex')
      diet <- diet[-1,]
			diet$idFB <- as.character(diet$idFB)
			diet$Genus <- as.character(diet$Genus)
			diet$Species <- as.character(diet$Species)
			diet$UnkLength <- as.character(diet$UnkLength)
			diet$a <- as.character(diet$a)
			diet$b <- as.character(diet$b)
			diet$KnownLength <- as.character(diet$KnownLength)
      diet$r <- as.character(diet$r)
      diet$LengthRange <- str_split(str_replace_all(as.character(diet$LengthRange), '\r\n', ''), '-')
      diet$Sex <- as.character(diet$Sex)
      diet$MinLength <- NA
      diet$MaxLength <- NA
      for (i in 1:length(diet$LengthRange))
        {
        diet$MinLength[i] <- diet$LengthRange[[i]][1]
        diet$MaxLength[i] <- diet$LengthRange[[i]][2]
        }
      diet <- diet[,-9]
		}
	}
  return(diet)
}
