process_chave_2009 <- function(filename) {
  df <- readxl::read_excel(filename, sheet = 2)
  # remove empty row (no idea why this happen with the xls file ...
  df <- df[df$Family != "", ]
  names(df) <- gsub("_$", "", gsub("__", "_",
                                   gsub("[ '('')''/''^'-]", "_", names(df))))
  df$Genus <- sub(" .*", "", df$Binomial)
  df$Species <- sub(".* ", "", df$Binomial)
  names(df) <- c("Number", "Family", "Binomial", "Wood_density",
                 "Region", "Reference_Number", "Genus", "Species")
  for (i in names(df)[c(1, 4, 6)]) {
    df[[i]] <- as.numeric(df[[i]])
  }
  #compute mean WD per species
  require(dplyr)
  df_sp <-  group_by(df, Binomial) %>% mutate(Wood_density = mean(Wood_density, na.rm = TRUE),
                                                  count = n()) %>% slice(1) %>%  ungroup()
  df_sp
}

