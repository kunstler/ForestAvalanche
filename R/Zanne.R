process_zanne_2010 <- function(filename) {
  ## There are several strategies for reading in an excel file, but this one works
  ## quite well.
  df <- readxl::read_xls(filename, sheet = 2)
  # remove empty row (no idea why this happen with the xls file ...
  df <- df[df$Family != "", ]
  names(df) <- gsub("_$", "", gsub("__", "_", gsub("[ '('')''/''^'-]", "_", names(df))))
  df$Genus <- sub(" .*", "", df$Binomial)
  df$Species <- sub(".* ", "", df$Binomial)
  for (i in names(df)[3:ncol(df)]) {
    df[[i]] <- as.numeric(df[[i]])
  }
  return(df)
}

