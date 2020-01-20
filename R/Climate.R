## CHELSA
#' Download chelsa climatologies
#'
#' @description Download chelsa climatologies for a given variables
#'
#' @export
#'
#' @param name name of the bioclimatic variables in Bio01 to Bio19
#' @param path path where to store the data (a 'chelsa_climatologies' directory is created to store the data')
#' @param overwrite should you overwrite the downloaded data
#' @param ... additional param not use for the moment
#' @import archive raster
download_climatologies <- function(name = "Bio01",path = '', overwrite = FALSE, ...) {
require(archive)
if(length(name)>1) stop("\nname must be of length one")
if(!name %in% paste0("Bio", c(paste0("0", 1:9), 10:19)))
    stop(paste0("\nArgument name must be in ",
                paste(paste0("Bio", c(paste0("0", 1:9), 10:19)),
                      collapse = ", "),
                " see http://chelsa-climate.org/bioclim/"))
# create dir
path <- 'chelsa_climatologies'
dir.create(path, showWarnings=FALSE)

address <- paste0('https://www.wsl.ch/lud/chelsa/data/bioclim/integer/CHELSA_bio10_',
                  gsub("Bio","",name), "_land.7z")
filename <- file.path(path,paste0('CHELSA_bio10_',
                                  gsub("Bio","",name),
                                  "_land.7z"))

filename_tif<- file.path(path,paste0('CHELSA_bio10_',
                                  as.numeric(gsub("Bio","",name)),
                                  ".tif"))

if (!file.exists(filename_tif) | overwrite) {
  .download(address, filename)
  if (!file.exists(filename))	{
  	message("\nCould not download file -- perhaps it does not exist")
  }
  #unzip strange format 7z
  #archive(filename)
  archive_extract(filename, dir = path)
}
st <- raster(x= filename_tif)
projection(st) <- "+proj=longlat +datum=WGS84"
return(st)
}



## From package raster can we call it directly as it is a hidden function?
.download <- function(aurl, filename) {
	fn <- paste(tempfile(), '.download', sep='')
	res <- utils::download.file(url=aurl, destfile=fn, method="auto",
                                    quiet = FALSE, mode = "wb", cacheOK = TRUE)
	if (res == 0) {
		w <- getOption('warn')
		on.exit(options('warn' = w))
		options('warn'=-1)
		if (! file.rename(fn, filename) ) {
			# rename failed, perhaps because fn and filename refer to different devices
			file.copy(fn, filename)
			file.remove(fn)
		}
	} else {
		stop('could not download the file' )
	}
}

## Function to extract aridity from http://www.cgiar-csi.org/data/global-aridity-and-pet-database
download_aridity <- function(dir_temp = "clim_temp"){
require(raster)
require(R.utils)
# download raster
url_clim <- "https://www.dropbox.com/sh/e5is592zafvovwf/AACSS163OQ2nm5m1jmlZk4Gva/Global%20PET%20and%20Aridity%20Index/Global%20Aridity%20-%20Annual.zip?dl=1"
raster_name_zip <- "Global Aridity - Annual.zip"
if(!dir.exists(file.path(dir_temp, "AI_annual"))){
    dir.create(dir_temp)
    download.file(url_clim, file.path(dir_temp, raster_name_zip))
    unzip(zipfile = file.path(dir_temp, raster_name_zip),
          exdir = dir_temp)
}

raster_aridity <- raster(file.path(dir_temp, "AI_annual",
                                   "ai_yr",  "w001001x.adf"))
e1 <- extent(-30, 60, 30, 80 )
#
res <- crop(raster_aridity, e1)
return(res)
}


## Function to extract Priestley-Taylor alpha coefficient from http://www.cgiar-csi.org/data/global-high-resolution-soil-water-balance
download_alpha <- function(dir_temp = "clim_temp"){
require(raster)
require(R.utils)
# download raster

url_clim <- "https://www.dropbox.com/sh/e5is592zafvovwf/AABrH65bVecLHfUcqazSnFeMa/Global%20Soil-Water%20Balance/Priestley-Taylor%20Alpha%20Coefficient.rar?dl=1"
raster_name_zip <- "Priestley-Taylor Alpha Coefficient.rar"
if(!dir.exists(file.path(dir_temp, "ALPHA"))){
    dir.create(dir_temp)
    download.file(url_clim, file.path(dir_temp, raster_name_zip))
    system(paste0("unrar x ", dir_temp, "/'", raster_name_zip,"' ", dir_temp))
}

raster_alpha <- raster(file.path(dir_temp, "ALPHA",
                                   "alpha",  "w001001x.adf"))
e1 <- extent(-30, 60, 30, 80 )
#
res <- crop(raster_alpha, e1)
return(res)
}


# Functions to extract soil data from SoilGrid
## TODO change for ftp://ftp.soilgrids.org/data/aggregated/1km/ for sl1 and sl2 and take the mean

download_ph <- function(dir_temp = "soil_temp"){

require(raster)
require(R.utils)
# download raster
url_ph <- "ftp://ftp.soilgrids.org/data/aggregated/1km/PHIHOX_M_sl2_1km_ll.tif"
raster_name <- "PHIHOX_M_sl1_1km_ll.tif"
if(!file.exists(file.path(dir_temp, raster_name))){
    dir.create(dir_temp)
    download.file(url_ph, file.path(dir_temp, raster_name),mode="wb")
}
# load raster
raster_ph <- raster(file.path(dir_temp, raster_name))
e1 <- extent(-30, 60, 30, 80 )
 #
res <- crop(raster_ph, e1)
return(res)
}

#######################
## Function to extract annual AET from http://www.cgiar-csi.org/data/global-high-resolution-soil-water-balance
download_AETy <- function(dir_temp = "clim_temp"){
require(raster)
require(R.utils)
# download raster
url_clim <- "https://www.dropbox.com/sh/e5is592zafvovwf/AADpYnw-nBpeLobJEbIzb-Xla/Global%20Soil-Water%20Balance/Mean%20Annual%20AET.rar?dl=1"
raster_name_zip <- "Mean Annual AET.rar"
if(!dir.exists(file.path(dir_temp, "AET_YR"))){
    dir.create(dir_temp)
    download.file(url_clim, file.path(dir_temp, raster_name_zip))
    system(paste0("unrar x ", dir_temp, "/'", raster_name_zip,"' ", dir_temp))
}

raster_AETy <- raster(file.path(dir_temp, "AET_YR",
                                   "aet_yr",  "w001001x.adf"))
e1 <- extent(-30, 60, 30, 80 )
res <- crop(raster_AETy, e1)
return(res)
}


## stack data

stack_list <- function(Bio01, Bio04, Bio10, Bio11, Bio15, Bio16,
                       aridity, alpha, AETy, Ph){
listt <- list(Bio01, Bio04, Bio10, Bio11, Bio15, Bio16)
require(raster)
e <- extent(-30, 60, 30, 80)
res <- crop(raster::stack(listt), e)
res <- raster::stack(res, aridity, alpha, AETy, Ph)
return(res)
}

# EXTRACT climate
extract_clim <- function(points, l_stack){
 val <- raster::extract(x=l_stack,y=points) # Extraction of the values
 colnames(val) <- c("Bio01", "Bio04", "Bio10", "Bio11",
                 "Bio15", "Bio16", "aridity", "alpha", "AETy", "Ph")
 df <- cbind(data.frame(points),val)
 return(df)
}


### function to convert plot into Lat Long
to_latlong  <- function(df){
  coordinates(df) =~ xl93+yl93
  proj4string(df) <- CRS("+init=epsg:2154")
  df <- spTransform(df, "+init=epsg:4326")
  return(df)
}



