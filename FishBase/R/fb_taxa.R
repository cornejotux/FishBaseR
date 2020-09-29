fb_taxa <-
function(idFB = NA, Genus = NA, Species = NA, server = 'http://www.fishbase.us/')
	{
require("XML")
require("stringr")
require('RCurl')
	Taxa <- as.data.frame(t(rep(NA, 8)))
	names(Taxa) <- c('idFB', 'Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'ScientificName')
	if (is.na(idFB)) 
		{
		ids <- fb_ids(Genus=Genus, Species=Species, server=server)
		idFB <- ids$idFB
		}
	if(!is.na(idFB))
		{
		Taxa$idFB <- idFB
		url <- paste(server, "maintenance/FB/showXML.php?identifier=FB-", idFB, sep = "")
		doc <- xmlTreeParse(url, useInternalNodes = TRUE) ## I got the file as a XML class
		a <- str_trim(toString.XMLNode(getNodeSet(doc, "//dwc:Kingdom")[[1]]), side="both")
		a <- str_replace_all(str_replace_all(a, "<dwc:Kingdom>", ""), "</dwc:Kingdom>", "")
		Taxa$Kingdom <- a
		a <- str_trim(toString.XMLNode(getNodeSet(doc, "//dwc:Phylum")[[1]]), side="both")
		a <- str_replace_all(str_replace_all(a, "<dwc:Phylum>", ""), "</dwc:Phylum>", "")
		Taxa$Phylum <- a
		a <- str_trim(toString.XMLNode(getNodeSet(doc, "//dwc:Class")[[1]]), side="both")
		a <- str_replace_all(str_replace_all(a, "<dwc:Class>", ""), "</dwc:Class>", "")
		Taxa$Class <- a
		a <- str_trim(toString.XMLNode(getNodeSet(doc, "//dwc:Order")[[1]]), side="both")
		a <- str_replace_all(str_replace_all(a, "<dwc:Order>", ""), "</dwc:Order>", "")
		Taxa$Order <- a
		a <- str_trim(toString.XMLNode(getNodeSet(doc, "//dwc:Family")[[1]]), side="both")
		a <- str_replace_all(str_replace_all(a, "<dwc:Family>", ""), "</dwc:Family>", "")
		Taxa$Family <- a
		a <- str_trim(toString.XMLNode(getNodeSet(doc, "//dwc:Genus")[[1]]), side="both")
		a <- str_replace_all(str_replace_all(a, "<dwc:Genus>", ""), "</dwc:Genus>", "")
		Taxa$Genus <- a
		a <- str_trim(toString.XMLNode(getNodeSet(doc, "//dwc:ScientificName")[[1]]), side="both")
		a <- str_replace_all(str_replace_all(a, "<dwc:ScientificName>", ""), "</dwc:ScientificName>", "")
		Taxa$ScientificName <- a
		free(doc)
		}
	return(as.data.frame(Taxa))
	}
