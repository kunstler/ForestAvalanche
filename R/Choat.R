process_choat_2012 <- function(filename) {

  d <- readxl::read_excel(filename, sheet = 1)
  d <- d[-1, ]
 names(d) <- c("Group", "Binomial", "PI50", "PI88", "Psi_min_midday", "Psi_min",
               "Psi_50_safety_margin", "Psi_88_safety_margin", "MAT",
               "MAP", "MPDQ",
               "PET", "AI", "Growth_form",
               "Developmental_stage", "Biome", "Latitude", "Longitude", "Elevation",
               "Location", "Country", "Notes", "Reference")

for (i in c("PI50", "PI88", "Psi_min_midday", "Psi_min",
           "Psi_50_safety_margin", "Psi_88_safety_margin")    ){
 d[[i]]<- as.numeric(d[[i]])
 }
return(d)

}
