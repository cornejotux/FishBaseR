fb_fecundity <-
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
    url <- paste(server, "Reproduction/FecundityList.php?ID=", idFB, sep = "")
    g <- readHTMLTable(url, as.data.frame=TRUE) ## I got the file as a XML class
    g <- g[[2]]
    if (dim(g)[1] == 2)
      {
      gu <- data.frame(idFB=idFB, Genus=as.character(Genus), Species = as.character(Species),
                       Country = NA, Locality = NA, AbsFec_min = NA, AbsFec_max = NA, RelFec_min = NA, RelFec_mean = NA,
                       RealFec_max = NA, FecLenRel_a = NA, FecLenRel_b = NA, URL = url)

      }
    if (dim(g)[1] > 2)
      {
      gu <- cbind(as.numeric(as.character(idFB)), as.character(Genus), as.character(Species),g[-(1:2),], URL= url)
      names(gu) <-  c('idFB', 'Genus', 'Species', 'Country', 'Locality', 'AbsFec_min', 'AbsFec_max', 'RelFec_min',
                      'RelFec_mean', 'RealFec_max', 'FecLenRel_a', 'FecLenRel_b', 'URL')
      
      gu$Genus <- as.character(gu$Genus)
      gu$Species <- as.character(gu$Species)
      gu$Country <- as.character(gu$Country)
      gu$Locality <- as.character(gu$Locality)
      gu$AbsFec_min  <- as.character(gu$AbsFec_min)
      gu$AbsFec_max  <- as.character(gu$AbsFec_max)
      gu$RelFec_min  <- as.numeric(as.character(gu$RelFec_min))
      gu$RelFec_mean <- as.numeric(as.character(gu$RelFec_mean))
      gu$RealFec_max <- as.numeric(as.character(gu$RealFec_max))
      gu$FecLenRel_a <- as.numeric(as.character(gu$FecLenRel_a))
      gu$FecLenRel_b <- as.numeric(as.character(gu$FecLenRel_b))
      
      for(i in 1: length(gu$AbsFec_min))
      {
        gu$AbsFec_min[i] <- str_replace_all(gu$AbsFec_min[i], ',', '')
        gu$AbsFec_max[i] <- str_replace_all(gu$AbsFec_max[i], ',', '')
      }
      
      gu$AbsFec_min  <- as.numeric(gu$AbsFec_min)
      gu$AbsFec_max  <- as.numeric(gu$AbsFec_max)
  
      }
  }
  return(data.frame(gu))
}
