########################
########################
### READ CLPA shapefile and extrcat value for IFN points (here with coordinates of the nodes)

require(rgdal)
require(rgeos)
require(sp)
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

# subset IFN points to "zone etude" of clpa
df_subset <- df[zetude, ]

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

# TODO add elevation slope and aspect either from dem or from field measurements

# Write extracted data
write.csv(df_subset, "extracted_clpa.csv")
