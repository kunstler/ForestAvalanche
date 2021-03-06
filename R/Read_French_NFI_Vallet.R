read_french_NFI <- function(path){
    # Chargement des "packages" utiles
    require(data.table)
    require(stringr)
    # Liste des fichiers ` extraire (dans les fichiers zip IFN)
    ListZip <- dir(path, pattern = ".zip")
    ListFichArb <- grep(pattern = "arbres_foret",
                        unlist(lapply(file.path(path, ListZip),
                                      FUN = unzip, list = TRUE)),
                        value = T)
    ListFichArbMort <- grep(pattern = "arbres_morts_foret",
                            unlist(lapply(file.path(path, ListZip),
                                          FUN = unzip, list = TRUE)),
                            value = T)

    ListFichPlacette <- grep(pattern = "placettes_foret_20",
                             unlist(lapply(file.path(path, ListZip),
                                           FUN = unzip, list = TRUE)),
                             value = T)

    ListFichEcologie <- grep(pattern = "ecologie_20",
                             unlist(lapply(file.path(path, ListZip),
                                           FUN = unzip, list = TRUE)),
                             value = T)
    # Chargement des donnees
    ListArbresVivants <- mapply(
        function(x, y) read.table(unz(x, y), header = TRUE,
                                  sep = ";", dec = ".", quote = "",
                                  encoding = "UTF-8"),
        file.path(path, ListZip), ListFichArb, SIMPLIFY = F)

    ListArbresMorts <- mapply(
        function(x, y) read.table(unz(x, y), header = TRUE,
                                  sep = ";", dec = ".", quote = "",
                                  encoding = "UTF-8"),
        file.path(path, ListZip), ListFichArbMort, SIMPLIFY = F)

    ListPlacette <- mapply(
        function(x, y) read.table(unz(x, y), header = TRUE,
                                  sep = ";", dec = ".", quote = "",
                                  encoding = "UTF-8"),
        file.path(path, ListZip), ListFichPlacette, SIMPLIFY = F)

    ListEcologie <- mapply(
        function(x, y) read.table(unz(x, y), header = TRUE,
                                  sep = ";", dec = ".", quote = "",
                                  encoding = "UTF-8"),
        file.path(path, ListZip), ListFichEcologie, SIMPLIFY = F)
    # Creation des data.table
    arbresVivants <- rbindlist(ListArbresVivants, fill=T);
    arbresVivants$espar  <- as.character(arbresVivants$espar)
    arbresVivants$espar[ arbresVivants$espar == "2"] <- "02"
    arbresVivants$espar[ arbresVivants$espar == "3"] <- "03"
    arbresVivants$espar[ arbresVivants$espar == "4"] <- "04"
    arbresVivants$espar[ arbresVivants$espar == "5"] <- "05"
    arbresVivants$espar[ arbresVivants$espar == "6"] <- "06"
    arbresVivants$espar[ arbresVivants$espar == "7"] <- "07"
    arbresVivants$espar[ arbresVivants$espar == "9"] <- "09"
    arbresMorts <- rbindlist(ListArbresMorts, fill=T);
    #flore <- rbindlist(ListFlore); setkey(flore, "idp")
    placette <- rbindlist(ListPlacette, fill=T)
    names(placette)[names(placette)=="tm2"]<-"tm_2"
    placette$annee <- floor(placette$idp/100000)+ 5
    ecologie <- rbindlist(ListEcologie, fill=T)

    return(list("arbresVivants" = arbresVivants, "arbresMorts" = arbresMorts,
                "placette" = placette, "ecologie" = ecologie))
}
