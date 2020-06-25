# Anaylsis of The French NFI data in relation to snow avalanche disturbance

INRAE LESSEM Grenoble

The snow avalanche disturbance is based on the CLPA data base see http://www.avalanches.fr/clpa-presentation/ which map snow avalanche paths in two categories main avalanche path and border of avalanche path.

For each NFI plot of cycle 4 3 and 2 we extracted based on the coordinates of the plots if the plot was located in a avalanche path or a the border of an avalanche path.

Tree species traits were extracted from public data.

## Installation

This R cran code require the package `drake`.

In addition the following packages are required:

```
require(rgdal, rgeos, sp, stringr, readxl, tidyr, dplyr, drake, raster, FD, ggplot2, reshape2, data.table, RColorBrewer, ggfortify)

```


## Folders structure

 * The R script functions are in the folder `R`
 * The folder `data` contains the data for the CLPA IFN (cycle 4 3 and 2) and the traits open data.
 * The folder `ms` contains the file for the pdf Rmarkdown report (latex header bibliography)


## Data

 * Download data from dropbox.
 * Unzip "chelsa_climatologies.zip".
 * The folder `AlpesPyr` contains CLPA shapes.
 * The folder `chelsa_climatologies` contains data used to unclude environnemental conditions in our generalised linear models.
 * The folder `clim_temp` contains data used to unclude environnemental conditions in our generalised linear models.
 * The folder `France_Entiere` contains Modèle Numerique de Terrain (MNT) at France scale
 * The folder `IFN_ALL` contains IFN data used for cycles 2 3 and 4.
 * The folder `merged` contains shapes assigning each IFN plot to a CLPA category of cycle 4.
 * The folder `Traits` contains the data of functional traits studied.
 * The file `altitudes_exactes_C4` contains the exact plot elevations of Cycle 4.
 * The file `data_plot` contains ???
 * The file `extracted_clpa_2020` contains CLPA category for ech Cycle 4 plots
