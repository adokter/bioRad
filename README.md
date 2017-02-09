# bioRad
bioRad is an R package for extracting and visualising biological signals from weather radar data.

* analyzes time series of profile data, and makes profile visualisations (see this real-time [example](http://www.flysafe-birdtam.eu/profile.php?radar=debilt)).
* overlays radar scans with geographic maps and satellite imagery of various online sources (e.g Google Maps and Stamen Maps), using  [ggmap](https://cran.r-project.org/web/packages/ggmap/index.html).
* contains an implementation of [vol2bird](https://github.com/adokter/vol2bird), an algorithm to extract vertical profiles of bird migration from weather radar data. 
* Reads radar files in [ODIM](http://www.eumetnet.eu/sites/default/files/OPERA2014_O4_ODIM_H5-v2.2.pdf) format, which is the implementation of the OPERA data information model in [HDF5](https://support.hdfgroup.org/HDF5/) format, or formats supported by the [RSL library](http://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/), such as [NEXRAD](https://www.ncdc.noaa.gov/data-access/radar-data/nexrad) data.

The [vol2bird](https://github.com/adokter/vol2bird) algorithm, and a tool to convert NEXRAD data into ODIM format, require a working installation of [Docker](https://www.docker.com/).

# installation
To install `bioRad` complete these four steps:

### 1. rhdf5
bioRad requires the rhdf5 library to read [hdf5](https://support.hdfgroup.org/HDF5/) files. This library is available through bioconductor (not CRAN). To install, run in R:
``` 
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```

### 2. ggplot2, ggmap, fields, devtools
Install these packages manually in R before installing bioRad:
```
install.packages("ggplot2")
install.packages("ggmap")
install.packages("fields")
install.packages("devtools")
```

### 3. bioRad 
You are now ready to install the bioRad package. In R, first load the devtools package, then install using `install_github`:
```
library(devtools)
install_github("adokter/bioRad")
```
### 4. Docker (optional)
You only need to install Docker if:
* you want to run the [vol2bird](https://github.com/adokter/vol2bird) algorithm.
* you want to analyze NEXRAD data. The tools to convert NEXRAD data into ODIM format require Docker.

#### 4.a Install Docker
The functionality of [vol2bird](https://github.com/adokter/vol2bird), an algorithm to extract vertical profiles of birds from weather radar data, is available in bioRad through Docker.

Go to the [Docker](https://www.docker.com/) webpage for instructions on how to install Docker on your local system. On 8 Dec 2016 Docker is available for Windows 10 Professional or Enterprise 64-bit, MacOS Yosemite 10.10.3 or above, or any linux/unix distribution.

Without a Docker installation, the bioRad package disables volbird automatically. All the other tools will still work.

#### 4.b Setup Docker
Docker needs local drives to be available for Docker containers. To enable:
* right click the Docker (whale) icon on your task or menu bar
* select settings -> shared drives
* select the drives where you will be processing radar files
* click apply


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
After installation, restart R.
### install note 2: rgdal on Mac OSX / linux:
bioRad requires an installation of rgdal, which can be fetched from CRAN. The GDAL and PROJ.4 libraries are external to the rgdal package, and, when installing the package from source, must be correctly installed first.

You can use the package managing systems, like Macports on Mac, to install these dependencies, e.g.
```
sudo port install proj
sudo port install gdal +expat
```
When compiling rgdal you need to specify the installation directory of the PROJ.4 and GDAL libraries that rgdal depends on. Install from source using the following command (example here with `/opt/local/` as the leading path, which is the directory where the Macports package managing system installs PROJ.4 and GDAL):
```
install.packages('rgdal',configure.args=c('--with-proj-include=/opt/local/include', '--with-proj-lib=/opt/local/lib', '--with-gdal-config=/opt/local/bin/gdal-config'),type="source")
```

### Install note 3: Virtualbox / Hyper-V conflicts
Unfortunately, Hyper-V can not run together with Virtualbox. When you want to use Virtualbox after running Docker, you need to disable Hyper-V, requiring a reboot of the system. [Here](https://marcofranssen.nl/switch-between-hyper-v-and-virtualbox-on-windows/) some instructions on how to set up a dual-boot system fairly easily (haven't tested this myself yet)

