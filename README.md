# bioRad
R package for extracting and visualising biological signals from weather radar data

# installation
To install `bioRad` complete these four steps:
### 1.a Install Docker
The functionality of [vol2bird](https://github.com/adokter/vol2bird), an algorithm to extract vertical profiles of birds from weather radar data, is available in bioRad through Docker.

Go to the [Docker](https://www.docker.com/) webpage for instructions on how to install Docker on your local system. On 8 Dec 2016 Docker is available for Windows 10 Professional or Enterprise 64-bit, MacOS Yosemite 10.10.3 or above, or any linux/unix distribution.

Without a Docker installation, the bioRad package disables volbird automatically. All the other tools will still work.

### 1.b Setup Docker
Docker needs local drives to be available for Docker containers. To enable:
* right click the Docker (whale) icon on your task or menu bar
* select settings -> shared drives
* select the drives where you will be processing radar files
* click apply

### 2. rhdf5
bioRad requires the rhdf5 library to read [hdf5](https://support.hdfgroup.org/HDF5/) files. This library is available through bioconductor (not CRAN). To install, run in R:
``` 
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```

### 3. ggplot2, ggmap, fields, devtools
Install these packages manually in R before installing bioRad:
```
install.packages("ggplot2")
install.packages("ggmap")
install.packages("fields")
install.packages("devtools")
```

### 4. bioRad 
You are now ready to install the bioRad package. In R, first load the devtools package, then install using 'install_github':
```
library(devtools)
install_github("adokter/bioRad")
```

### install note 1: ggplot2 and ggmap on Mac OSX
bioRad requires the ggplot2 and ggmap packages to be installed in R. While these are both available through CRAN, on MacOS I found that I ran into this error when using bioRad's function `map`:
```
Error: GeomRasterAnn was built with an incompatible version of ggproto.
Please reinstall the package that provides this extension.
```
This issue is fixed when installing the latest versions from Github (8 Dec 2016)
```
install_github("hadley/ggplot2")
install_github("dkahle/ggmap")
```

### install note 2: rgdal on Mac OSX / linux:
bioRad requires an installation of rgdal, which can be fetched from CRAN. When you want to compile rgdal using a non-default installation directory of the proj.4 library that rgdal depends on, install from source using the following command (example here with `/opt/local/lib/proj47` as the proj4 path):
```
install.packages('rgdal',configure.args=c('--with-proj-include=/opt/local/lib/proj47/include', '--with-proj-lib=/opt/local/lib/proj47/lib'),type="source")
```
