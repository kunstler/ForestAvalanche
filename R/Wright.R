process_wright_2004 <- function(filename, sitevars_file) {

  d <- readxl::read_excel(filename, sheet = 1, skip = 10)

  ## Do some name translations:
  tr <- c("Code"="Code",
          "Dataset"="Dataset",
          "BIOME"="Biome",
          "Species"="Binomial",
          "GF"="GrowthForm",
          "Decid/E'green"="Deciduous",
          "Needle/Broad lf"="Needle",
          "C3C4"="C3",
          "N2-fixer"="N2fixer",
          "log LL"="LogLeafLifespan",
          "log LMA"="LogLMA",
          "log Nmass"="Log.N.mass",
          "log Narea"="Log.N.area",
          "log Pmass"="Log.P.mass",
          "log Parea"="Log.P.area",
          "log Amass"="Log.A.mass",
          "log Aarea"="Log.A.area",
          "log Gs"="Log.Gs",
          "log Rdmass"="Log.Rd.mass",
          "log Rdarea"="Log.Rd.area",
          "Ca - Ci"="CaCi")
  names(d)[match(names(tr), names(d))] <- tr

  ## Drop blank columns
  d <- d[names(d) != " "]

  ## Data tweaking:
  d[["Code"]] <- as.integer(d[["Code"]])
  d[["CaCi"]] <- as.numeric(d[["CaCi"]])

  d[["Deciduous"]] <- category_to_logical(d[["Deciduous"]], "D")
  d[["Needle"]]    <- category_to_logical(d[["Needle"]],    "N")
  d[["C3"]]        <- category_to_logical(d[["C3"]],        "C3")
  d[["N2fixer"]]   <- category_to_logical(d[["N2fixer"]],   "Y")

  names(d) <- gsub("Log\\.", "Log", names(d))
  re <- "Log"
  i_log <- grep(re, names(d))
  d[i_log] <- lapply(d[i_log], as.numeric)
  d_unlogged <- as.data.frame(10^d[i_log])
  names(d_unlogged) <- sub(re, "", names(d_unlogged))

  d <- cbind(d[-c(i_log)], d_unlogged)

  # add location info
  sitevars <- read.csv(sitevars_file, stringsAsFactors = FALSE,
                       encoding = "UTF-8")

  data <- merge(d, sitevars, by.x = 'Dataset', by.y = 'dataset_location',
                all.x = TRUE, all.y = FALSE, sort = FALSE)

  #lowercase names
  names(data) <- tolower(names(data))

  # unit conversions
  data$lma <- data$lma/1000 # Converts to kg
  data$n.area <- data$n.area/1000 # Converts to kg
  data$a.area <- (data$a.area * 31557.6)*10^-6
       # converts to mol/kg/yr from micro-mol/g/s
  data$rd.area <- (data$rd.area * 31557.6)*10^-6 # converts to mol/kg/yr from micro-mol/g/s

  data$leaflifespan <- data$leaflifespan/12 ## convert LL from months to years
  data$leaf_turnover <- 1/data$leaflifespan ## per year

  data$mat_o_map<-  (data$mat+18)/data$map

  names(data)[names(data) %in%
              c('dataset', 'mat_degc', 'map_mm')]<- c('location','mat', 'map')

  names(data)[names(data) == "binomial"]<- c('Binomial')
  return(data)
}


process_wright_2017 <- function(filename) {

  d <- readxl::read_xlsx(filename, sheet = 2)

  d <- d[, !names(d) %in% c("MAT", "Tgs", "TCM", "TCMgs", "TWM", "MAP",
                             "PPTgs", "cvPPT", "MIann", "MIgs", "ETq", "ETqgs", "RADann",
                             "RADgs", "RHann", "RHgs")]

  ## Do some name translations:
  tr <- c("ID"= "ID",
          "Site number" = "Site_number",
          "Site name" = "Site_name",
          "Reference" = "Reference",
          "Genus species" = "Binomial",
          "Family" = "Family",
          "Order" = "Order",
          "Name_orig" = "Name_orig",
          "TaxonGroup" = "TaxonGroup",
          "woody_non-woody" = "woody_nonwoody",
          "Growth form" = "Growth_form",
          "DecidEver (woody only)" = "DecidEver",
          "Compound_Simple" = "Compound_Simple",
          "Leaf size (cm2)" = "Leaf_size_cm2",
          "Whole leaf size (cm2)" = "Whole_leaf_size_cm2",
          "Country" = "Country",
          "Latitude" = "Latitude",
          "Longitude" = "Longitude",
          "Elevation (m)" = "Elevation_m")

  names(d)[match(names(tr), names(d))] <- tr

    d$Leaf_size_cm2 <- as.numeric(d$Leaf_size_cm2)
    d$Whole_leaf_size_cm2 <- as.numeric(d$Whole_leaf_size_cm2)

return(d)

}
