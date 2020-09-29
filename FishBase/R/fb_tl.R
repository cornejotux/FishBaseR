fb_tl <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.us/')
	{
require("XML")
require("stringr")
require('RCurl')

	tl <- data.frame(idFB = idFB, Genus = Genus, Species = Species, tl = NA, tl_se = NA, BasedOn = NA)
	url <- NA;
  if (is.na(Genus) | is.na(Species))
		{
		print("The Genus and Species are needed!!")
		}
	if (!is.na(Genus) & !is.na(Species)) 
		{
		url <- paste(server, "Summary/",Genus,"-",Species, ".html", sep = "")
		}
	if (!is.na(url))
		{
		g <- htmlParse(url)
		g2 <- toString.XMLNode(g)
		g3 <- toString(str_replace_all(str_replace_all(str_replace_all(g2, "\t", ""), "\n", ""), "\r", ""))
		g4 <- strsplit(g3, "<!-- Start Trophic Level -->")[[1]][2]
		g5 <- strsplit(g4,"se;")[[1]][1]
		BasedOn <- strsplit(g4,"se;")[[1]][2]
		BasedOn <- strsplit(BasedOn,"</div>")[[1]][1]
		g5.1 <- strsplit(g5,"</a>):")[[1]][2]
		g5.2 <- strsplit(g5.1," se;")[[1]][1]
		g6 <- str_replace_all(g5.2, "\U3e30613c", "")
		g6 <- str_replace_all(g6, "\U3e31623c", "")
		g6 <- strsplit(g6, " ")
		tl$tl <- as.numeric(g6[[1]][1])
		tl$tl_se <- as.numeric(g6[[1]][3])
		tl$BasedOn <- as.character(BasedOn)
		}
	return(tl)
	}
