########################
########################
### drake plan CLPA, NFI and Traits data

# package
require(rgdal)
require(rgeos)
require(sp)
require(stringr)
require(readxl)
require(dplyr)
require(drake)

# source all files
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))

### plan

plan <- drake_plan(
   choat = process_choat_2012(file_in("data/Traits/choat-2012.xls")),
   maire = process_maire_2015(file_in(file.path("data", "Traits",
                                                "maire_2015.xlsx"))),
   wright2017 = process_wright_2017(file_in("data/Traits/wright-2017.xls")),
   wright2004 =  process_wright_2004(file_in("data/Traits/wright-2004.xls"),
                              file_in("data/Traits/wright_2004_locations.csv")),
   chave = process_chave_2009(file_in("data/Traits/GlobalWoodDensityDatabase.xls")),
   zanne = process_zanne_2010(file_in("data/Traits/GlobalVesselAnatomyDatabase.xls")),
   zetude = readOGR(dsn = file.path("data", "merged"), layer = "CLPA_zetude_L93"),
   zont = readOGR(dsn = file.path("data", "merged"), layer = "CLPA_zont_L93"),
   zonpi = readOGR(dsn = file.path("data", "merged"), layer = "CLPA_zonpi_L93"),
   wzon = readOGR(dsn = file.path("data", "merged"), layer = "CLPA_wzon_L93"),
   lint = readOGR(dsn = file.path("data", "merged"), layer = "CLPA_lint_L93"),
   linpi = readOGR(dsn = file.path("data", "merged"), layer = "CLPA_linpi_L93"),
   list_all = read_french_NFI(file.path("data", "IFN_ALL", "IFNCYCLE4")),
   plot_IFN4_CLPA = format_cycle4_CLPA(list_all,
                                  file_in(file.path("data",
                                                    "extracted_clpa_FM.csv"))),
   plot_IFN3_CLPA = format_cycle3_CLPA(zetude, zont, lint, zonpi, linpi, wzon,
                   file_plot = file_in(file.path("data", "IFN_ALL",
                                                 "IFNCYCLE3",
                                                 "placettedef.txt")),
                   file_xy = file_in(file.path("data", "IFN_ALL",
                                               "IFNCYCLE3", "xy.txt"))),
   plot_IFN2_CLPA = format_cycle2_CLPA(zetude, zont, lint, zonpi, linpi, wzon,
                   path_plot = file_in(file.path("data", "IFN_ALL",
                                                 "IFNCYCLE2"))),
   speciesC32 = species_C3_C2(file_in(file.path("data", "IFN_ALL", "IFNCYCLE3",
                                                "speciesnames.txt"))),
   speciesC4 = species_C4(file_in(file.path("data", "IFN_ALL", "IFNCYCLE4",
                                            "all_species_C4.csv"))),
   treesC4 = trees_C4(list_all),
   treesC3 = trees_C3(file_in(file.path("data", "IFN_ALL", "IFNCYCLE3",
                                        "arbres2.txt"))),
   treesC2 = trees_C2(),
   ms = rmarkdown::render(
    knitr_in("ms.Rmd"),
    output_file = file_out("ms.pdf"),
    quiet = TRUE),
   spvecC4 = unique(as.vector(na.exclude(gsub("_", " ", speciesC4$Latin_Name)))),
   spvecC32 = unique(as.vector(na.exclude(gsub("_", " ", str_to_title(speciesC32$Latin_Name))))),
   traitsC4 = extract_public_trait(spvecC4, wright2004, wright2017, maire,
                                   chave, zanne, choat),
   traitsC32 = extract_public_traitC32(spvecC32, wright2004, wright2017, maire,
                                      chave, zanne, choat, speciesC32),
   treesCLPA_C2 = trees_CLPA_C2(treesC2,plot_IFN2_CLPA),
   treesCLPA_C3 = trees_CLPA_C3(treesC3,plot_IFN3_CLPA),
   treesCLPA_C4 = trees_CLPA_C4(treesC4,plot_IFN4_CLPA),
   basal_area_C2 = BA_C2_calcul(treesCLPA_C2, speciesC32),
   basal_area_C3 = BA_C3_calcul(treesCLPA_C3, speciesC32),
   basal_area_C4 = BA_C4_calcul(treesCLPA_C4, speciesC4),
   essence_C2 = format_cycle2_essence(file.path("data", "IFN_ALL","IFNCYCLE2"),
                                      basal_area_C2),
   stem_nb_C4 = stem_number_C4(treesCLPA_C4),
   stem_nb_C3 = stem_number_C3(treesCLPA_C3),
   stem_nb_C2 = stem_number_C2(essence_C2),
   DQ_C4 = root_mean_square(basal_area_C4,stem_nb_C4),
   DQ_C3 = root_mean_square(basal_area_C3,stem_nb_C3),
   DQ_C2 = root_mean_square(basal_area_C2,stem_nb_C2),
   Cv_C2 = var_coef_C2(treesCLPA_C2),
   Cv_C3 = var_coef_C3(treesCLPA_C3),
   Cv_C4 = var_coef_C4(treesCLPA_C4),
   clim_var = clim_attrib(plot_IFN2_CLPA,plot_IFN3_CLPA,plot_IFN4_CLPA)
   )

# Make plan
make(plan)

# plot plan
config <- drake_config(plan)
vis_drake_graph(config)

### TODO

# - compute plots CWM mean and sd of trait per traits and multi traits (based on basal area)?

