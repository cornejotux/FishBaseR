fb_speed <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.org/')
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
		url <- paste(server, "Physiology/SpeedList.php?ID=", idFB, sep = "")
		s <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
		if (length(s) == 0)
			{
			speed <- data.frame(idFB = as.numeric(as.character(idFB)), Genus = as.character(Genus), 
                          Species = as.character(Species), Speed = NA, Lengths = NA, 
                          Mode = NA, LengthType = NA, Length = NA)
			}
		if (length(s) != 0)
			{
			speed <- data.frame(idFB = as.character(idFB), Genus = as.character(Genus), 
                          Species = as.character(Species), s[[1]])
			names(speed) <-  c('idFB', 'Genus', 'Species', 'Speed', 'Lengths', 'Mode', 'LengthType', 'Length')
		  speed$TempLengths <- speed$TempLengthType <- speed$TempLength <- speed$TenoMode <- NA
      for (i in 1:length(speed$Lengths))
        {
        speed$TempLengths[i]    <- (str_trim(str_split(string=speed$Lengths[i], pattern='Â')[[1]][2], side="both"))
        speed$TempLengthType[i] <- (str_trim(str_split(string=speed$LengthType[i], pattern='Â')[[1]][2], side="both"))
        speed$TempLength[i]     <- (str_trim(str_split(string=speed$Length[i], pattern='Â')[[1]][2], side="both"))
        speed$TempMode[i]     <- (str_trim(str_split(string=speed$Mode[i], pattern='Â')[[1]][2], side="both"))
        }
      speed$Lengths <- speed$TempLengths
      speed$LengthType <- speed$TempLengthType
      speed$Length <- speed$TempLength
			speed$Mode <- speed$TempMode
      speed <- speed[,-(9:12)]
			}
		}
	return(speed)
	}
