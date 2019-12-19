# Anaylsis of The French NFI data in relation to snow avalanche disturbance

INRAE LESSEM Grenoble

The snow avalanche disturbance is based on the CLPA data base see http://www.avalanches.fr/clpa-presentation/ which map snow avalanche paths in two categories main avalanche path and border of avalanche path.

For each NFI plot of cycle 4 3 and 2 we extracted based on the coordinates of the plots if the plot was located in a avalanche path or a the border of an avalanche path.

Tree species traits were extracted from public data.

## Installation

This R cran code require the package `drake`.

In addition the following packages are required:

```
require(rgdal)
require(rgeos)
require(sp)
require(readxl)
require(dplyr)
require(drake)
```


## Folders structure

 * The R script functions are in the folder `R`
 * The folder `data` contains the data for the CLPA IFN (cycle 4 3 and 2) and the traits open data.
 * The folder `ms` contains the file for the pdf Rmarkdown report (latex header bibliography)




