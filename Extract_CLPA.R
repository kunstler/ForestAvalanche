########################
########################
### READ CLPA shapefile and extrcat value for IFN points (here with coordinates of the nodes)

require(rgdal)
require(rgeos)
require(sp)
source("READ_FRENCH_NFI_Vallet.R")

# read shapefiles
zetude <-readOGR(dsn = "merged", layer = "CLPA_zetude_L93")
zont <-readOGR(dsn = "merged", layer = "CLPA_zont_L93")
zonpi <-readOGR(dsn = "merged", layer = "CLPA_zonpi_L93")
wzon <-readOGR(dsn = "merged", layer = "CLPA_wzon_L93")
lint <-readOGR(dsn = "merged", layer = "CLPA_lint_L93")
linpi <-readOGR(dsn = "merged", layer = "CLPA_linpi_L93")

# read IFN plots (2009- 2015)
df <- read.csv("data_plot.csv")
coordinates(df) =~ xl93+yl93
proj4string(df) <- proj4string(zetude)
# all data from Patrick function
list_all  <- Read_French_NFI("IFN_ALL/IFNCYCLE4")
df_all <- list_all$placette
df_all_yr <- dplyr::filter(df_all, annee %in% 5:15)
coordinates(df_all_yr) =~ xl93+yl93
proj4string(df_all_yr) <- proj4string(zetude)
arbres_vivants <- list_all$arbresVivants

# subset IFN points to "zone etude" of clpa
df_subset <- df[zetude, ]
df_all_subset <- df_all_yr[zetude, ]

plot(zetude)
plot(df_all_yr, cex = 0.1, add = TRUE)
plot(df_all_subset, cex = 0.3, col = "red", add = TRUE)

plot(zetude)
plot(zont)
plot(lint, add = TRUE, col = "red")
points(df_subset, cex = 0.1, col = "green")

# add buffer of 25m around each IFN beacuase of accuracy of CLPA (~ 50m)
pointbuff_df <- gBuffer(df_subset, width=c(25), byid=T)

## # plot
## plot(zonpi, col = unclass(zonpi$CODE), xlim = c(961900, 982867), ylim = c(6448800, 6469126))
## plot(zetude, add = TRUE)
## plot(pointbuff_df, col="pink", border="pink", add=T)

# Extract CODE from shapefile
df_subset$CODE_zont <- over(pointbuff_df, zont[, "CODE"])
df_subset$CODE_lint <- over(pointbuff_df, lint[, "CODE"])
df_subset$CODE_zonpi <- over(pointbuff_df, zonpi[, "CODE"])
df_subset$CODE_linpi <- over(pointbuff_df, linpi[, "CODE"])
df_subset$CODE_wzon <- over(pointbuff_df, wzon[, "CODE"])

# Write extracted data
write.csv(df_subset, "extracted_clpa.csv")

## read data extracted with exact coordinates
df_extract <- read.csv("extracted_clpa_FM.csv", sep = ";")
# merge with all to get X Y
df_extract <- dplyr::left_join(df_extract,
                               as.data.frame(df_all_yr),
                               by = "idp")
coordinates(df_extract) =~ xl93+yl93
proj4string(df_extract) <- proj4string(zetude)
sum(!df_all_subset$idp %in% df_extract$idp)
sum(!df_extract$idp %in% df_all_yr$idp)


# PLOT MATCHING AND NON MATCHING PLOTS
plot(zetude,  col = "green")
points(df_extract[df_extract$idp %in% df_all_subset$idp, ], cex = 0.5, pch = 1)
points(df_extract[!df_extract$idp %in% df_all_subset$idp, ],
       col = "red", cex = 1, pch =16)
points(df_all_subset[!df_all_subset$idp %in% df_extract$idp, ],
       col = "blue", cex = 1, pch =16)
# THE PLOTS THAT ARE MISSING ARE ON THE BOUNDARY OF THE STUDY BECAUSE OF DIFF IN COORDINATES

## read IFN cycle 3 data
placetteC3 <- data.table::fread(file.path("IFN_ALL", "IFNCYCLE3",
                                          "placettedef.txt"))
speciesC3 <- data.table::fread(file.path("IFN_ALL", "IFNCYCLE3",
                                         "speciesnames.txt"))
xyC3 <- data.table::fread(file.path("IFN_ALL", "IFNCYCLE3", "xy.txt"))
plac3 <- dplyr::left_join(xyC3[, c("CPP", "XL2", "YL2")], placetteC3,
                          by = "CPP")
coordinates(plac3) =~ XL2+YL2
proj4string(plac3) <- CRS("+init=epsg:27572")
plac3N <- spTransform(plac3, proj4string(zetude))

# Extract clpa data
plac3N_subset <- plac3N[zetude, ]

# add buffer of 25m around each IFN beacuase of accuracy of CLPA (~ 50m)
pointbuff_plac3N <- gBuffer(plac3N_subset, width=c(25), byid=T)
# Extract CODE from shapefile
pointbuff_plac3N$CODE_zont <- over(pointbuff_plac3N, zont[, "CODE"])
pointbuff_plac3N$CODE_lint <- over(pointbuff_plac3N, lint[, "CODE"])
pointbuff_plac3N$CODE_zonpi <- over(pointbuff_plac3N, zonpi[, "CODE"])
pointbuff_plac3N$CODE_linpi <- over(pointbuff_plac3N, linpi[, "CODE"])
pointbuff_plac3N$CODE_wzon <- over(pointbuff_plac3N, wzon[, "CODE"])


# CYCLE 2
library(readxl)
tt  <-  grep("xls", list.files(file.path("IFN_ALL", "IFNCYCLE2")), value = TRUE)
list_point  <- lapply(tt,
                      function( nn) {
                          read_excel(file.path("IFN_ALL",
                                               "IFNCYCLE2", nn),
                                     sheet = "Point", skip = 5)})

list_arbre  <- lapply(tt,
                      function( nn) {
                          read_excel(enc2native(file.path("IFN_ALL",
                                               "IFNCYCLE2", nn)),
                                     sheet = "Arbre", skip = 5,
                                     col_types = c("text", "text",
                                                  "numeric", "text",
                                                  "text", "text",
                                                  "text", "numeric",
                                                  "text", "numeric",
                                                  "numeric", "numeric",
                                                  "numeric", "numeric",
                                                  "numeric", "numeric",
                                                  "numeric", "numeric"))})

placetteC2 <- dplyr::bind_rows(list_point)
arbreC2 <- dplyr::bind_rows(list_arbre)

speciesC2 <- read.csv(file.path("IFN_ALL", "IFNCYCLE2", "species.csv"),
                      sep =";")

placetteC2 <- placetteC2[!is.na(placetteC2$XL), ]
coordinates(placetteC2) =~ XL+YL
proj4string(placetteC2) <- CRS("+init=epsg:27572")
placetteC2 <- spTransform(placetteC2, proj4string(zetude))

# Extract clpa data
placetteC2_subset <- placetteC2[zetude, ]

# add buffer of 25m around each IFN beacuase of accuracy of CLPA (~ 50m)
pointbuff_placetteC2 <- gBuffer(placetteC2_subset, width=c(25), byid=T)
# Extract CODE from shapefile
pointbuff_placetteC2$CODE_zont <- over(pointbuff_placetteC2, zont[, "CODE"])
pointbuff_placetteC2$CODE_lint <- over(pointbuff_placetteC2, lint[, "CODE"])
pointbuff_placetteC2$CODE_zonpi <- over(pointbuff_placetteC2, zonpi[, "CODE"])
pointbuff_placetteC2$CODE_linpi <- over(pointbuff_placetteC2, linpi[, "CODE"])
pointbuff_placetteC2$CODE_wzon <- over(pointbuff_placetteC2, wzon[, "CODE"])

## NEED TO LIST SPECIES

