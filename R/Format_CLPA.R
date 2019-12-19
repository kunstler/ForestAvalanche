## Functions to format CLPA with NFI data

format_cycle4_CLPA <- function(list_all,
                             path = file.path("data",
                                              "extracted_clpa_FM.csv")){
  df_all <- list_all$placette
  df_all_yr <- dplyr::filter(df_all, annee %in% 5:15)
  df_extract <- read.csv(file.path("data", "extracted_clpa_FM.csv"),
                         sep = ";")
  # merge with all to get X Y
  df_extract <- dplyr::left_join(df_extract,
                                 as.data.frame(df_all_yr),
                                 by = "idp")
  return(df_extract)
}


format_cycle3_CLPA  <- function(zetude, zont, lint, zonpi, linpi, wzon,
                                file_plot = file.path("data", "IFN_ALL",
                                                      "IFNCYCLE3",
                                                      "placettedef.txt"),
                                file_xy = file.path("data", "IFN_ALL",
                                                    "IFNCYCLE3", "xy.txt")){
  placetteC3 <- data.table::fread(file_plot)
  xyC3 <- data.table::fread(file_xy)
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
  bb <- coordinates(pointbuff_plac3N)
  return(data.frame(xl93 = bb[, 1], yl93 = bb[, 2],
                    data.frame(as.data.frame(pointbuff_plac3N))))
}


format_cycle2_CLPA  <- function(zetude, zont, lint, zonpi, linpi, wzon,
                                path_plot = file.path("data", "IFN_ALL",
                                                      "IFNCYCLE2")){
  tt  <-  grep("xls", list.files(path_plot),
               value = TRUE)
  list_point  <- lapply(tt,
                        function( nn) {
                            read_excel(file.path("data", "IFN_ALL",
                                                 "IFNCYCLE2", nn),
                                       sheet = "Point", skip = 5)})
  placetteC2 <- dplyr::bind_rows(list_point)
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
  bb <- coordinates(pointbuff_placetteC2)
  return(data.frame(xl93 = bb[, 1], yl93 = bb[, 2],
                    data.frame(as.data.frame(pointbuff_placetteC2))))
}


## get species C3 C2

species_C3_C2  <- function(path_sp = file.path("data", "IFN_ALL", "IFNCYCLE3",
                                         "speciesnames.txt")){
  speciesC3 <- data.table::fread(path_sp)
  return(speciesC3)
}


species_C4  <- function(file_sp = file.path("data", "IFN_ALL", "IFNCYCLE4",
                                         "species.csv")){
  speciesC4 <- data.table::fread(file_sp)
  return(speciesC4)
}


## alive tree

trees_C4  <- function(list_all){
    treesC4 <- list_all$arbresVivants
    return(treesC4)
}


trees_C3  <- function(file_trees = file.path("data", "IFN_ALL", "IFNCYCLE3",
                                          "arbres2.txt")){
    treesC3 <- data.table::fread(file_trees)
    return(treesC3)
}


trees_C2_csv  <- function(path_trees = file.path("data", "IFN_ALL",
                                                 "IFNCYCLE2")){
    tt  <-  grep("xls", list.files(path_trees),
                 value = TRUE)
    print(tt)
    list_arbre  <- lapply(tt, function( nn) {
                              read_excel(enc2native(file.path("data", "IFN_ALL",
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

    arbreC2 <- dplyr::bind_rows(list_arbre)
    write.csv(arbreC2, file = file.path("data", "IFN_ALL",
                                        "IFNCYCLE2", "arbresC2.csv"),
              row.names = FALSE)
}    

trees_C2  <- function(file_trees = file.path("data", "IFN_ALL",
                                             "IFNCYCLE2", "arbresC2.csv")){
    treesC2 <- data.table::fread(file_trees)
    return(treesC2)
}    

