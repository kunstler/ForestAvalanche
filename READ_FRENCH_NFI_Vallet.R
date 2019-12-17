Read_French_NFI <- function(path){
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
    setkey(arbresVivants, "idp")
    arbresMorts <- rbindlist(ListArbresMorts, fill=T);
    setkey(arbresMorts, "idp")
    #flore <- rbindlist(ListFlore); setkey(flore, "idp")
    placette <- rbindlist(ListPlacette, fill=T)
    names(placette)[names(placette)=="tm2"]<-"tm_2"
    placette$annee <- floor(placette$idp/100000)+ 5
    ecologie <- rbindlist(ListEcologie, fill=T)

    tt  <-  list(arbresVivants, arbresMorts,
                 placette, ecologie)
    return(list("arbresVivants" = arbresVivants, "arbresMorts" = arbresMorts,
                "placette" = placette, "ecologie" = ecologie))
}
