## functions to exctract or/and download climatiques variables
## some data needs to be download manually

extract_raster <- function(nb){
  
  require(raster)
  
  path <- file.path("data","chelsa_climatologies")
  if(dir.exists(file.path(path,paste0('CHELSA_bio10_', nb,"_land")))) {
    filename_tif<- file.path(path,paste0('CHELSA_bio10_', nb,"_land"),
                             paste0('CHELSA_bio10_', nb,".tif"))
  } else {
    filename_tif<- file.path(path,paste0('CHELSA_bio10_', nb,".tif"))
  }
  
  st <- raster::raster(filename_tif)
  projection(st) <- "+proj=longlat +datum=WGS84"
  
  e <- raster::extent(-4, 9, 40, 48)
  st <- raster::crop(st, e)
  
  return(st)
}




extract_climatique <- function(dir_temp = file.path("data"),file_nb){
  
  require(raster)
  
  
  if(!dir.exists(file.path(dir_temp,"chelsa_climatologies"))){
    dir.create(file.path(dir_temp, "chelsa_climatologies"))
    message("Please download data from: https://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4 and extract them from .tar.gz format to .tif format inside chelsa_climatologies folder")
    
  }
  
  data_raster <- extract_raster(file_nb[1])
  
  for (nb in file_nb[-1]) {
    raster_supl <- extract_raster(nb)
    data_raster <- raster::stack(data_raster,raster_supl)
  }
  return(data_raster)
}




download_aridity <- function(dir_temp = file.path("data","clim_temp")){
  require(raster)
  require(R.utils)
  # download raster
  url_clim <- "https://www.dropbox.com/sh/e5is592zafvovwf/AACSS163OQ2nm5m1jmlZk4Gva/Global%20PET%20and%20Aridity%20Index/Global%20Aridity%20-%20Annual.zip?dl=1"
  raster_name_zip <- "Global Aridity - Annual.zip"
  if(!dir.exists(file.path(dir_temp, "AI_annual"))){
    dir.create(dir_temp)
    download.file(url_clim, file.path(dir_temp, raster_name_zip), mode='wb')
    utils::unzip(zipfile = file.path(dir_temp, raster_name_zip),
                 exdir = dir_temp)
  }
  
  raster_aridity <- raster(file.path(dir_temp, "AI_annual",
                                     "ai_yr",  "w001001x.adf"))
  e1 <- extent(-4, 9, 40, 48)
  #
  res <- crop(raster_aridity, e1)
  return(res)
}




download_alpha <- function(dir_temp = file.path("data","clim_temp")){
  
  require(raster)
  require(R.utils)
  
  # download raster
  url_clim <- "https://www.dropbox.com/sh/e5is592zafvovwf/AABrH65bVecLHfUcqazSnFeMa/Global%20Soil-Water%20Balance/Priestley-Taylor%20Alpha%20Coefficient.rar?dl=1"
  raster_name_rar <- "Priestley-Taylor_Alpha_Coefficient.rar"
  if(!dir.exists(file.path(dir_temp, "ALPHA")) & dir.exists(file.path(dir_temp))){
    dir.create(file.path(dir_temp, "ALPHA"))
    download.file(url_clim, file.path(paste0(dir_temp,"/",raster_name_rar)),  mode='wb')
    filename <- file.path(dir_temp,raster_name_rar)
    
    seven_zip <- shQuote('C:\\Program Files\\7-Zip\\7z') # seven-zip installation location
    rar_file <- paste(dir_temp, '/Priestley-Taylor_Alpha_Coefficient.rar', sep = '') # rar file to unrar
    store_file <- file.path(dir_temp,"ALPHA") # path where store your unrar file
    cmd = paste(seven_zip, ' e ', rar_file, ' -y -o', store_file, '/', sep='')
    
    shell(cmd) # file extraction (runs on windows operating system, to check with other OS...) 
  }
  
  raster_alpha <- raster::raster(file.path(dir_temp, "ALPHA", "w001001x.adf"))
  e1 <- raster::extent(-4, 9, 40, 48)
  #
  res <- raster::crop(raster_alpha, e1)
  return(res)
}




download_AETy <- function(dir_temp = file.path("data","clim_temp")){
  
  require(raster)
  require(R.utils)
  require(sm)
  
  # download raster
  if(!dir.exists(file.path(dir_temp, "AET_YR"))){
    dir.create(file.path(dir_temp, "AET_YR"))
    message("Please download AET_YR.rar file at https://figshare.com/articles/Global_High-Resolution_Soil-Water_Balance/7707605/3 and store it in dir_temp folder before continue")
    message(dir_temp)
    
    seven_zip <- shQuote('C:\\Program Files\\7-Zip\\7z') # seven-zip installation location
    rar_file <- paste(dir_temp, '/AET_YR.rar', sep = '') # rar file to unrar
    store_file <- file.path(dir_temp,"AET_YR") # path where store your unrar file
    cmd = paste(seven_zip, ' e ', rar_file, ' -y -o', store_file, '/', sep='')
    
    shell(cmd) # file extraction (runs on windows operating system, to check with other OS...) 
  }
  
  raster_AETy <- raster::raster(file.path(dir_temp, "AET_YR",  "w001001x.adf"))
  e1 <- raster::extent(-4, 9, 40, 48)
  res <- raster::crop(raster_AETy, e1)
  return(res)
}




extract_soil <- function(dir_temp = file.path("data","clim_temp","Soil_data"), tiff_file_name){
  
  require(raster)
  require(R.utils)
  # download ras
  
  # load raster
  raster_soil <- raster(file.path(dir_temp, tiff_file_name))
  e1 <- extent(-4, 9, 40, 48)
  #
  res <- crop(raster_soil, e1)
  return(res)
}


stack_list_clim <- function(Bio_stack, aridity, alpha, AETy){
  
  require(raster)

  res <- raster::stack(Bio_stack, aridity, alpha, AETy)
  names(res) <- c("CHELSA_bio10_1","CHELSA_bio10_4",  "CHELSA_bio10_10",
                  "CHELSA_bio10_11","CHELSA_bio10_15", "CHELSA_bio10_16", "CHELSA_bio10_17",
                   "aridity", "alpha", "AETy")

  return(res)
}

stack_list_soil <- function(Ph, CEC, OCC){
  
  require(raster)
  
  res <- raster::stack(Ph, CEC, OCC)
  names(res) <- c("pH", "cation_exchange_capacity", "organic_carbon_content")
  
  return(res)
}



# EXTRACT climate
extract_clim <- function(points, clim_stack, soil_stack){
  
 val_clim <- raster::extract(x=clim_stack,y=points) # Extraction of the values
 colnames(val_clim) <- c("Bio01", "Bio04", "Bio10", "Bio11",
                 "Bio15", "Bio16", "Bio17", "aridity", "alpha", "AETy")
 df <- cbind(data.frame(points),val_clim)
 
 val_soil <- raster::extract(x=soil_stack,y=points) # Extraction of the values
 colnames(val_soil) <- c("pH", "cation_exchange_capacity", "organic_carbon_content")
 df <- cbind(data.frame(df),val_soil)

  return(df)
}


### function to convert plot into Lat Long
to_latlong  <- function(df){
  coordinates(df) =~ xl93+yl93
  proj4string(df) <- CRS("+init=epsg:2154")
  df <- spTransform(df, "+init=epsg:4326")
  return(df)
}



# extraction des Altitudes
#Cycle 4
extract_alti_C4 <- function(path,plot_IFN4_CLPA,clim_IFN_C4){
  all_alt_C4 <- read.csv(path, sep = ";")
  all_alt_C4 <- all_alt_C4[which(all_alt_C4$idp != "NULL"),]
  all_alt_C4$idp <- as.numeric(as.character(all_alt_C4$idp))
  alt_C4 <- dplyr::filter(all_alt_C4, all_alt_C4$idp %in% plot_IFN4_CLPA$idp)
  names(alt_C4) <- c("idp","altitude")
  clim_IFN_C4 <- left_join(clim_IFN_C4,alt_C4, by = c("idp" = "idp"))
  return(clim_IFN_C4)
}

extract_alti_C32 <- function(MNT_path = file.path("data","France_Entiere","france_mnt_2154.tif"),plot_IFN2_CLPA, plot_IFN3_CLPA, clim_IFN_C2, clim_IFN_C3 ){
MNT_altitude_fr <- raster::raster(MNT_path)

point_C2 <- plot_IFN2_CLPA
coordinates(point_C2) =~ xl93+yl93
proj4string(point_C2) <- CRS("+init=epsg:2154")
point_C2 <- spTransform(point_C2, CRS(projection(MNT_altitude_fr)))
altitude <- raster::extract(MNT_altitude_fr,point_C2)
point_C2 <- as.data.frame(point_C2)
clim_IFN_C2 <- cbind(clim_IFN_C2,altitude)

point_C3 <- plot_IFN3_CLPA
coordinates(point_C3) =~ xl93+yl93
proj4string(point_C3) <- CRS("+init=epsg:2154")
point_C3 <- spTransform(point_C3, CRS(projection(MNT_altitude_fr)))
altitude <- raster::extract(MNT_altitude_fr,point_C3)
point_C3 <- as.data.frame(point_C3)
clim_IFN_C3 <- cbind(clim_IFN_C3,altitude)

return(list(clim_IFN_C2,clim_IFN_C3))

}













clim_attrib <- function(plot_IFN2_CLPA, plot_IFN3_CLPA, plot_IFN4_CLPA){
  

  Bio_stack <-  extract_climatique(dir_temp = file.path("data"),file_nb = c(1,4,10,11,15,16,17))
  aridity <-  download_aridity(dir_temp = file.path("data","clim_temp"))
  alpha <- download_alpha(dir_temp = file.path("data","clim_temp"))
  AETy <-  download_AETy(dir_temp = file.path("data","clim_temp"))
  
  pH <-  extract_soil(dir_temp = file.path("data","clim_temp","Soil_data"),
                    tiff_file_name = "PHIHOX_M_sl2_1km_France.tiff")
  CEC <-  extract_soil(dir_temp = file.path("data","clim_temp","Soil_data"),
                      tiff_file_name = "CECSOL_M_sl2_1km_France.tiff")
  OCC <-  extract_soil(dir_temp = file.path("data","clim_temp","Soil_data"),
                     tiff_file_name = "ORCDRC_M_sl2_1km_France.tiff")
  
  clim_stack <-  stack_list_clim(Bio_stack, aridity, alpha, AETy)
  soil_stack <- stack_list_soil(pH, CEC, OCC)
  
  pointsIFN_C2 <- to_latlong(plot_IFN2_CLPA)
  clim_IFN_C2 <- extract_clim(pointsIFN_C2, clim_stack, soil_stack)
  clim_IFN_C2 <- as.data.frame(clim_IFN_C2)
  
  pointsIFN_C3 <- to_latlong(plot_IFN3_CLPA)
  clim_IFN_C3 <- extract_clim(pointsIFN_C3, clim_stack, soil_stack)
  clim_IFN_C3 <- as.data.frame(clim_IFN_C3)
  
  pointsIFN_C4 <- to_latlong(plot_IFN4_CLPA)
  clim_IFN_C4 <- extract_clim(pointsIFN_C4, clim_stack, soil_stack)
  clim_IFN_C4 <- as.data.frame(clim_IFN_C4)
  
  clim_IFN_C4 <- extract_alti_C4(path = file.path("data","altitudes_exactes_C4.csv"),plot_IFN4_CLPA,clim_IFN_C4)
  clim_IFN_C32 <- extract_alti_C32(MNT_path = file.path("data","France_Entiere","france_mnt_2154.tif"),
                                   plot_IFN2_CLPA, plot_IFN3_CLPA, clim_IFN_C2, clim_IFN_C3)
  
  return(list(clim_IFN_C32[[1]],clim_IFN_C32[[2]],clim_IFN_C4))                            
}


