# bioRad
R package for extracting and visualising bird signals from weather radar data

# installation
To install the bioRad package in R use the devtools package:
```
install.packages("devtools")
library(devtools)
install_github("adokter/bioRad")
```

### Docker
The functionality of [vol2bird](https://github.com/adokter/vol2bird) is available in bioRad through Docker.

Go to the [Docker](https://www.docker.com/) webpage for instructions on how to install Docker on your local system. On 8 Dec 2016 Docker is available for Windows 10 Professional or Enterprise 64-bit, MacOS Yosemite 10.10.3 or above, or any linux/unix distribution.

Without a Docker installation, the bioRad package disables volbird automatically. All the other tools will still work.

### rhdf5
bioRad requires the rhdf5 library to read [hdf5](https://support.hdfgroup.org/HDF5/) files. This library is available through bioconductor (not CRAN). To install:
``` 
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```

### ggplot2 and ggmap
bioRad requires the ggplot2 and ggmap packages to be installed in R. While these are both available throught CRAN, on MacOS I found that I needed to install the latest versions from Github (8 Dec 2016)
```
install_github("dkahle/ggmap")
install_github("hadley/ggplot2")
```
