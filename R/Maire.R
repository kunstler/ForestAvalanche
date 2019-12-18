process_maire_2015 <- function(filename) {
  ## There are several strategies for reading in an excel file, but
  ## this one works quite well.
    d <- readxl::read_xlsx(filename, sheet = 4)
    d <- d[, names(d) %in% c("SITECODE", "Latitude", "Longitude", "ELEV",
                             "Location", "Continent",
                             "Country", "Cite", "Genus.spp", "TaxGroup",
                             "Order", "Family",
                             "Genus", "Species", "GF", "DecEv", "SLA",
                             "Amass", "Aarea", "Nmass",
                           "Narea", "Pmass", "Parea", "Gs")]

    names(d) <-c("SITECODE", "Latitude", "Longitude", "ELEV",
                 "Location", "Continent",
               "Country", "Cite", "Binomial", "TaxGroup", "Order", "Family",
               "Genus", "Species", "GF", "DecEv", "SLA", "Amass",
               "Aarea", "Nmass",
               "Narea", "Pmass", "Parea", "Gs")
   # remove empty row (no idea why this happen with the xls file ...
  d <- d[d$Binomial != "", ]
  d$SLA <- as.numeric(d$SLA)
  d$Nmass <- as.numeric(d$Nmass)
  d$Narea <- as.numeric(d$Narea)
  d$Pmass <- as.numeric(d$Pmass)
  d$Parea <- as.numeric(d$Parea)

return(d)

}


