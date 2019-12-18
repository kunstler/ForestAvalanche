########################
########################
### drake plan CLPA, NFI and Traits data

# package
require(rgdal)
require(rgeos)
require(sp)
require(readxl)
require(dplyr)
require(drake)

# source
lapply(list.files("R"), function(x) source(file.path("R", x)))

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
                                            "species.csv"))),
   treesC4 = trees_C4(list_all),
   treesC3 = trees_C3(file_in(file.path("data", "IFN_ALL", "IFNCYCLE3",
                                        "arbres2.txt"))),
   treesC2 = trees_C2(),
   ms = rmarkdown::render(
    knitr_in("ms.Rmd"),
    output_file = file_out("ms.pdf"),
    quiet = TRUE)
  )




# Make plan
make(plan)

config <- drake_config(plan)
vis_drake_graph(config)



### TODO

# - tree   
# - merge tree and plot


## open_traits:
##     command: extract_public_trait(sps, wright2004, wright2017, maire, chave, zanne, choat)
