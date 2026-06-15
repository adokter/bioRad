# Introductory exercises with bioRad

*These course materials were developed for the 4th Radar Aeroecology
Training School, Jul 30 - Aug 5 2022, Fort Collins, CO, USA.*

## Getting started

Execute each of the code examples provided below in
[RStudio](https://posit.co/), and try to complete the exercises.

``` r

# make sure you start with a fresh R session
# load the bioRad package
library(bioRad)
# check the package version
packageVersion("bioRad")
```

All bioRad’s functions are documented in an extensive [function
reference](https://adriaandokter.com/bioRad/dev/reference/index.html)
online, as well as in manual pages within R:

``` r

# bring up the package general help page:
?bioRad
```

Start by making a new directory on your local machine that you will use
for this practical:

``` r

# make a new local directory on your machine for this practical
# replace the string below with the path of that directory:
HOME <- "your/personal/working/directory/"
# check that the directory exists. If the next statement evaluates to FALSE, something went wrong: the directory does not exist or you didn't specify its path correctly
file.exists(HOME)
# we will make HOME our work directory, the default folder in which R will look
# for files, and where it will output newly generated files.
setwd(HOME)
# next, we will create two directories where we will be storing data:
dir.create("./data_vpts") # here we will store vertical profile time series (vpts data)
dir.create("./data_pvol") # here we will store polar volumes (pvol data)
# Finally, we set the local time zone to UTC, so all plotted time axes will be in UTC
Sys.setenv(TZ = "UTC")
```

Next, download the two files posted in the `#biorad` channel, and put
them in the newly created `data_vpts` folder

Your R session is now properly set up.

## Basic visualization of radar scans

### The structure of polar volumes

``` r

# Let's first download the NEXRAD polar volume files for the KHGX radar (Houston)
# for a 15 minute period in 2017:
download_pvolfiles(date_min=as.POSIXct("2017-05-04 01:25:00"), date_max=as.POSIXct("2017-05-04 01:40:00"), radar="KHGX", directory="./data_pvol")
# store the filenames in my_pvolfiles
my_pvolfiles <- list.files("./data_pvol", recursive = TRUE, full.names = TRUE, pattern="KHGX")
# print to console our files:
my_pvolfiles
# let's load the first of our downloaded files:
my_pvol <- read_pvolfile(my_pvolfiles[1])
```

**Exercise 1:** What is the minimum and maximum scan elevation contained
in the volume? And which scan parameters are available? (See manual page
of the
[`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md)
function for the nomenclature of various available quantities).

### Plotting radar scans

``` r

# let's extract the scan collected at 1.5 degree elevation from our polar volume:
my_scan <- get_scan(my_pvol, 0.5)
# print some information about this scan:
my_scan
# let's plot the reflectivity factor parameter of the scan in a range - azimuth coordinate system:
plot(my_scan, param = "DBZH")
```

Usually it is easier to visually explore radar scans as a PPI (plan
position indicator), which is a projection of the scan on a Cartesian
(X,Y) or (lat,lon) grid:

``` r

# before we can plot the scan, we need to project it on a Cartesian grid,
# i.e. we need to make a Plan Position Indicator (PPI)
my_ppi <- project_as_ppi(my_scan)
# print some information about this ppi:
my_ppi
# you can see we projected it on a 500 meter grid
# (check the manual of the project_as_ppi function to see how you can
# change the grid size (argument grid_size) and the maximum distance
# from the radar up to where to plot data (argument range_max))
#
# Now we are ready to plot the ppi, for example let's plot reflectivity factor DBZH:
plot(my_ppi, param = "DBZH")
```

**Exercise 2:** This case shows an incoming precipitation front,
characterized by localized but intense thunderstorms, as well as
biological scattering. Make also a ppi plot of the correlation
coefficient (RHOHV) and radial velocity (VRADH). Verify which regions
are precipitation, and the approximate direction of movement of biology
and precipitation.

**Exercise 3:** Based on the radial velocity image, are the biological
scatterers birds or insects? Why?

### Overlaying radar scans on maps

``` r

# It is often informative to plot radar data on a base layer.
# First choose a base layer from the list of rosm::osm.types()
basemap = "osm"
# then overlay the PPI on the basemap, restricting the color scale from -20 to 40 dBZ:
map(my_ppi, map = basemap, param = "DBZH", zlim = c(-20, 40))
```

## Screening out weather

### using correlation coefficient

Screening precipitation based on correlation coefficient is arguably the
most simple and established approach for screening out weather in cases
where dual-polarization data is available. It is based on removing data
above a certain $`\rho_{/HV}`$ thresholds, most commonly 0.95.

``` r

# Screen out the reflectivity areas with RHOHV < 0.95
my_ppi_clean <- calculate_param(my_ppi, DBZH = ifelse(RHOHV > 0.95, NA, DBZH))
# plot the original and cleaned up reflectivity:
map(my_ppi, map = basemap, param = "DBZH", zlim = c(-20, 40))
map(my_ppi_clean, map = basemap, param = "DBZH", zlim = c(-20, 40))
```

### using MistNet

You can use MistNet for screening out weather when \* the radar operates
at S-band wavelengths \* you only have single polarization data
available (specifically, the three basic quantities radial velocity
VRADH, reflectivity DBZH, and spectrum width WRADH).

``` r

# apply the MistNet model to the polar volume file and load it as a polar volume (pvol):
my_pvol <- apply_mistnet(my_pvolfiles[1])
# mistnet will add additional parameters to the
# elevation scans at 0.5, 1.5, 2.5, 3.5 and 4.5 degrees
# let's extract the scan closest to 0.5 degrees:
my_scan <- get_scan(my_pvol, 0.5)
# plot some summary info about the scan to the console:
my_scan
```

### using depolarization ratio

Another quantity that has been proposed for distinguishing weather and
biology is the depolarization ratio ($`D_r`$), which is defined as

``` math
D_r=\frac{Z_{DR}+ 1 -2 \sqrt{Z_{DR}} \ \rho_{HV}}{Z_{DR}+ 1 +2 \sqrt{Z_{DR}} \ \rho_{HV}}
```
First we add the depolarization ratio (DR) as a parameter. We’ll express
DR on a dB scale by transforming:
``` math
DR=10\log_{10}(D_r) 
```
(see *Kilambi et al. 2018, A Simple and Effective Method for Separating
Meteorological from Nonmeteorological Targets Using Dual-Polarization
Data* for more information)

``` r

# let's add depolarization ratio (DR) as a parameter (following Kilambi 2018):
my_ppi <- calculate_param(my_ppi, DR = 10 * log10((1+ ZDR - 2 * (ZDR^0.5) * RHOHV) /
  (1 + ZDR+ 2 * (ZDR^0.5) * RHOHV)))
```

Like correlation coefficient you can apply simple thresholds to its
value to screen out precipitation. It has a good (potentially even
better) ability to distinguish weather and biology:

``` r

  # plot the depolarization ratio, using a viridis color palette:
map(my_ppi, map = basemap, param = "DR", zlim=c(-25,-5), palette = viridis::viridis(100))
```

MistNet adds several new parameters: \* `WEATHER`: a probability (0-1)
for the weather class \* `BIOLOGY`: a probability (0-1) for the biology
class \* `BACKGROUND`: a probability (0-1) for the background (empty
space) class \* `CELL`: the final segmentation, which is based on the
WEATHER parameters of all five MistNet elevation scans. `CELL` also
includes a fringe calculated by a region growing approach. `CELL` values
equal 1 for the additional fringe, and 2 for the originally segmented
precipitation area by MistNet.

``` r

# as before, project the scan as a ppi:
my_ppi <- project_as_ppi(my_scan)
# plot the probability for the WEATHER class
plot(my_ppi, param = 'WEATHER')
# plot the final segmentation result:
# plot the probability for the WEATHER class
plot(my_ppi, param = 'CELL')
# let's remove the identified precipitation area (and additional fringe) from the ppi, and plot it:
my_ppi_clean <- calculate_param(my_ppi, DBZH = ifelse(CELL >= 1, NA, DBZH))
map(my_ppi_clean, map=basemap, param = 'DBZH')
```

**Exercise 4:** Calculate and plot a ‘cleaned up’ PPI for the radial
velocity that includes only the segmentation by MistNet and not the
additional fringe.

## Vertical profiles

In this section you will learn to compute, interpret and analyze
vertical profiles (vp). A vp consists of the (bird) density, speed and
directions at different altitudes at the location of a single radar. It
is typically calculated for all data within a cylinder of 35 km radius
around the radar, i.e. only containing data at relatively short
distances, where the radar beam is still sufficiently narrow to resolve
altitude information.

Section 3 has examples that show how to process polar volume data into
vertical profiles. To save time, we will start below with a list of
pre-processed vertical profiles for the Brownsville radar in Texas
(KBRO).

### Loading processed vertical profiles

``` r

# Usually we would load processed vertical profiles (vp files) by:
# my_vplist <- read_vpfiles("./your/directory/with/processed/profiles/goes/here")
# my_vplist contains after running the command a list of vertical profile (vp) objects
# To save time, we load these data directly from file
my_vplist <- readRDS("data_vpts/KBRO20170514.rds")
# print the length of the vplist object. It should contain 95 profiles
length(my_vplist)
```

### Inspecting single vertical profiles

Now that you have loaded a list of vertical profiles, we can start
exploring them. We will start with plotting and inspecting single
vertical profiles, i.e. a single profile from the list of vp objects you
have just loaded.

``` r

# let's extract a profile from the list, in this example the 41st profile:
my_vp <- my_vplist[[41]]
# print some info for this profile to the console
my_vp
# test whether this profile was collected at day time:
check_night(my_vp)
# plot the vertical profile, in terms of reflectivity factor
plot(my_vp, quantity = "dbz")
# plot the vertical profile, in terms of (linear) reflectivity
plot(my_vp, quantity = "eta")
```

`eta` and `dbz` are closely related, the main difference is that
reflectivity factors are logarithmic, and reflectivities linear. You can
convert one into the other using
[`eta_to_dbz()`](http://adriaandokter.com/bioRad/dev/reference/eta_to_dbz.md)
and
[`dbz_to_eta()`](http://adriaandokter.com/bioRad/dev/reference/dbz_to_eta.md)
functions, which follows this simple formula:

    eta = (radar-wavelength dependent constant) * 10^(dbz/10)

The reflectivity factor `dBZ` is the quantity used by most
meteorologist. It has the useful property that at different radar
wavelengths (e.g. S-band versus C-band) the same amount of precipitation
shows up at similar reflectivity factors. The same holds for insects, as
well as any other target that is much smaller than the radar wavelength
(S-band = 10 cm, C-band = 5 cm), the so-called Rayleigh-scattering
limit.

In the case of birds we are outside the Rayleigh limit, because birds
are of similar size as the radar wavelength. In this limit reflectivity
`eta` is more similar between S-band and C-band. `eta` is also more
directly related to the density of birds, since `eta` can be expressed
as (bird density) x (radar cross section per bird). For these two
reasons, for weather radar ornithologists reflectivity `eta` is the more
conventional unit.

``` r

# let's plot the vertical profile, in terms of bird density
plot(my_vp, quantity = "dens")
# print the currently assumed radar cross section (RCS) per bird:
rcs(my_vp)
```

**Exercise 5:** If you change your assumption on the bird’s radar cross
section in the previous example, and assume the RCS is 10 times as
large, what will be the effect on the bird density profile?

The assumed radar cross section can be changed as follows:

``` r

# let's change the RCS to 110 cm^2
rcs(my_vp) <- 110
```

**Exercise 6:** Verify your answers on the previous two questions, by
re-plotting the vertical profiles for the bird density quantity.

### Plotting vertical profile time series

We will now examine multiple vertical profiles at once that are ordered
into a time series, e.g. the vertical profiles obtained from a single
radar over a full day.

``` r

# convert the list of vertical profiles into a time series:
my_vpts <- bind_into_vpts(my_vplist)
# time series objects can be subsetted, just as you may be used to with vectors
# here we subset the first 50 timesteps:
my_vpts[1:50]
# here we extract a single timestep, which gives you back a vertical profile class object:
my_vpts[100]
# to plot the full time series:
plot(my_vpts)
# check the help file for the plotting function of profile time series
# Because profile timeseries are of class 'vpts', it's associated plotting function
# is called plot.vpts:
?plot.vpts
```

Let’s make a plot for a subselection of the time series:

``` r

# filter our vpts for night time
my_vpts_night <- filter_vpts(my_vpts, night=TRUE)
# plot this smaller time series:
plot(my_vpts_night)
```

**Exercise 7:** Interpret the wind barbs in the profile time series
figure: what is the approximate speed and direction at 1500 meter at 6
UTC? In the speed barbs, each half flag represents 2.5 m/s, each full
flag 5 m/s, \[each pennant (triangle) 25 m/s, not occurring in this
case\].

**Exercise 8:** Extract the vertical profile at 6 UTC from the time
series and plot the vertical profile of ground speed (quantity `ff`).
Hint: use function
[`filter_vpts()`](http://adriaandokter.com/bioRad/dev/reference/filter_vpts.md)
to extract the 6 UTC profile. Check whether your answer to the previous
question was approximately correct.

### Vertical and time integration of profiles

Often you will want to sum together all the migrants in the vertical
dimension, for example if you want a single index of how many birds are
migrating at a certain instant. There are at least two ways in which you
can do that:

- by calculating the vertically integrated bird density (VID), which is
  *surface* density as opposed to a *volume* densities you have been
  plotting in the previous exercises: this number gives you how many
  migrants are aloft per square kilometer earth’s surface (unit
  individuals/km$`^{2}`$), obtained by a vertical integration of the
  volume densities (unit individuals/km$`^{3}`$).
- Note that the VID quantity doesn’t depend on the speed of the
  migrants. A common measure that reflects both the density and speed of
  the migration is migration traffic rate (MTR). This is flux measure
  that gives you how many migrants are passing the radar station per
  unit of time and per unit of distance perpendicular to the migratory
  direction (unit individuals/km/hour).

We will be using bioRad’s
[`integrate_profile()`](http://adriaandokter.com/bioRad/dev/reference/integrate_profile.md)
function to calculate these quantities:

``` r

# Let's continue with the vpts object created in the previous example.
# The vertically integrated quantities are calculated as follows:
my_vpi <- integrate_profile(my_vpts)
# The my_vpi object you created is a vpi class object, which is an acronym for "vertical profile integrated". It has its own plot method, which by default plots migration traffic rate (MTR):
plot(my_vpi)
# you can also plot vertically integrated densities (VID):
plot(my_vpi, quantity = "vid")
# the gray and white shading indicates day and night, which is calculated
# from the date and the radar position. You can also turn this off:
plot(my_vpi, night_shade = FALSE)
# plot the cumulative number of birds passing the radar, i.e. migration traffic (mt):
plot(my_vpi, quantity = "mt")
# execute `?plot.vpi` to open the help page listing all the options.
?plot.vpi
```

The following questions only require pen and paper. Assume a night
migration event in which the volume density of birds from 0-1 km above
ground is 200 birds per cubic kilometer, and from 1-1.5 km 100 birds per
cubic kilometer. Above 1500 meter there are no birds.

**Exercise 9:** What is in this case the bird’s vertically integrated
density (VID)? Give your answer in units birds/km$`^2`$.

**Exercise 10:** Let’s assume that in the lower layer birds fly at 50
km/hour, and in the upper layer at 100 km/hour. What is in this case the
migration traffic rate across a transect perpendicular to the direction
of movement? Give your answer in units birds/km/hour.

**Exercise 11:**Let’s assume migration continued for exactly three hours
after sunset, and then halted abruptly. How many birds have passed a 10
km transect perpendicular to the direction of movement in this night?
Give your answer in terms of migration traffic (mt) in units birds/km.

Both MTR, VID and MT depend on the assumed radar cross section (RCS) per
bird. If you are unwilling/unable to specify RCS, alternatively you can
use two closely related quantities that make no assumptions RCS:

``` r

# instead of vertically integrated density (VID), you can use vertically integrated reflectivity (VIR):
plot(my_vpi, quantity = "vir")
# instead of migration traffic rate (MTR), you can use the reflectivity traffic rate (RTR):
plot(my_vpi, quantity = "rtr")
# instead of migration traffic (MT), you can use the reflectivity traffic (RT):
plot(my_vpi, quantity = "rt")
```

VIR gives you the total cross-sectional area of air-borne targets per
square kilometer of ground surface, whereas RTR gives you the total
cross-sectional area of targets flying across a one kilometer line
perpendicular to the migratory flow per hour.

### Inspecting precipitation signals in profiles

Precipitation is known to have a major influence on the timing and
intensity of migration, therefore it is a useful skill to be able to
inspect profiles for presence of precipitation.

Also, although automated bird quantification algorithms become more and
more reliable, distinguishing precipitation from birds remains
challenging for algorithms in specific cases. It is therefore important
to have the skills to inspect suspicious profiles. That may help you to
identify potential errors of the automated methods, and prevent your
from overinterpreting the data.

An easy way of doing that is plotting the vertical profile of total
reflectivity (quantity DBZH), which includes everything: birds, insects
and precipitation. Precipitation often has higher reflectivities than
birds, and also extends to much higher altitudes.

``` r

# load a time series for the KBGM radar in Binghamton, NY
my_vpts <- readRDS("data_vpts/KBGM20170527-20170602.rds")
# print the loaded vpts time series for this radar:
my_vpts
# plot the bird density over time:
plot(my_vpts, quantity = "dens")
```

**Exercise 12:** Compare the above plot for bird density (quantity
`dens`) with a profile plot for total reflectivity (quantity `DBZH`,
showing birds and precipitation combined). Compare the two plots to
visually identify periods and altitude layers with precipitation.

## Range bias correction

In the previous section we have explored vertical integration of
vertical profiles (`vp`’s). In this paragraph we will generalize
vertical integration to entire radar images (`ppi`’s). To do that
properly, we will account for the changing beam shape of the radar with
range.

First, let’s examine the beam shape of the lowest elevation scan of the
radar, which is typically around 0.5 degrees.

``` r

# define ranges from 0 to 2500000 meter (250 km), in steps of 100 m:
range <- seq(0, 250000, 100)

# plot the beam height of the 0.5 degree elevation beam:
plot(range, beam_height(range, 0.5), ylab = "beam height [m]", xlab = "range [m]", type='l')

# let's add the lower and upper altitude of the beam, as determined by the beam width:
points(range, beam_height(range, 0.5)-beam_width(range)/2, type='l',lty=3)
points(range, beam_height(range, 0.5)+beam_width(range)/2, type='l',lty=3)
```

We will start with downloading a polar volume and processing it into a
profile:

### Processing a polar volume into a profile

``` r

# download a polar volume for the KBRO radar in Brownsville, TX
download_pvolfiles(date_min=as.POSIXct("2017-05-14 05:50:00"), date_max=as.POSIXct("2017-05-14 06:00:00"), radar="KBRO", directory="./data_pvol")
# Load all the polar volume filenames downloaded so far for the KBRO radar:
my_pvolfiles <- list.files("./data_pvol", recursive = TRUE, full.names = TRUE, pattern="KBRO")
# we will process the first one into a vp:
my_pvolfile <- my_pvolfiles[1]
# calculate the profile, using MistNet to remove precipitation:
# we calculate 60 layers of 50 meter width, so up to 30*100=3000 m.
my_vp <- calculate_vp(my_pvolfile, n_layer=60, h_layer=50, sd_vvp_threshold = 1)
```

**Exercise 13:** Plot the bird density in the vertical profile you just
estimated and compare it with the plots above of the beam height and
width of the lowest radar beam. At which approximate range do you expect
the radar will no longer be able to resolve the altitude distribution of
the migratory birds. And at which range will the radar start to
overshoot the migration layer entirely?

To correct for the decreasing ability of the radar to resolve the
altitude distributions of birds with range, we will make one important
(and likely imperfect!) assumption:

**We assume that all birds within the image are distributed vertically
according to the same relative vertical profile.**

This assumptions simplifies the problem, and allows us to estimate the
spatial distribution of the birds, as we will explore in the next
paragraph:

### Range bias correction and vertical integration on a map

``` r

# We will use the piping operator %>% of magrittr package to
# execute multiple operations in one statement:
library(magrittr)
# first, load the polar volume:
my_pvolfile %>% read_pvolfile() -> my_pvol
# Next, let's calculate a PPI for the 1.5 degree elevation scan
# Finally, we calculated the vertically integrated PPI
my_ppi_integrated <- integrate_to_ppi(pvol=my_pvol,vp=my_vp, res=1000)
```

**Exercise 14:** Visually compare the PPI for the 1.0 degree sweep and
the vertically integrated PPI, and explain the difference in spatial
pattern. (For the clearest comparison, make plots of comparable
parameters that are either linear or logarithmic in bird density).

## Processing many polar volumes

### Accessing radar data

- US NEXRAD polar volume data can be accessed in the [Amazon
  cloud](https://s3.amazonaws.com/noaa-nexrad-level2/index.html). Use
  function
  [`download_pvolfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_pvolfiles.md)
  for local downloads
- European radar data can be accessed at <https://aloftdata.eu>. These
  are processed vertical profiles, the full polar volume data are not
  openly available for most countries. Use function
  [`download_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_vpfiles.md)
  for local downloads.

The names of the radars in the networks can be found here:

- US: NEXRAD network
- Europe: [OPERA
  database](http://eumetnet.eu/wp-content/themes/aeron-child/observations-programme/current-activities/opera/database/OPERA_Database/index.md)

Useful sites for inspecting pre-made movies of the US composite are
<https://birdcast.info/migration-tools/live-migration-maps/> and
<https://www.pauljhurtado.com/US_Composite_Radar/>.

### Processing multiple polar volumes

This section contains an example for processing a directory of polar
volumes into profiles:

First we download more files, and prepare an output directory for
storing the processed profiles:

``` r

# First we download more data, for a total of one additional hour for the same radar:
download_pvolfiles(date_min=as.POSIXct("2017-05-04 01:40:00"), date_max=as.POSIXct("2017-05-04 02:40:00"), radar="KHGX", directory="./data_pvol")
# We will process all the polar volume files downloaded so far:
my_pvolfiles <- list.files("./data_pvol", recursive = TRUE, full.names = TRUE, pattern="KHGX")
my_pvolfiles
# create output directory for processed profiles
outputdir <- "./data_vp"
dir.create(outputdir)
```

We will use the following custom settings for processing: \* We will use
MistNet to screen out precipitation \* we will calculate profile layers
of 50 meter width \* we will calculate 60 profile layers, so up to
50\*60=3000 meter altitude

Note that we enclose the
[`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
function in [`tryCatch()`](https://rdrr.io/r/base/conditions.html) to
keep going after errors with specific files

Having generated the profiles, we can read them into R:

``` r

# we assume outputdir contains the path to the directory with processed profiles
my_vpfiles <- list.files(outputdir, full.names = TRUE, pattern="KHGX")
# print them
my_vpfiles
# read them
my_vplist <- read_vpfiles(my_vpfiles)
```

You can now continue with visualizing and post-processing as we did
earlier:

``` r

# make a time series of profiles:
my_vpts <- bind_into_vpts(my_vplist)
# plot them between 0 - 3 km altitude:
plot(my_vpts, ylim = c(0, 3000))
# because of the rain moving in, our ability to estimate bird profiles slowly drops:
# let's visualize rain by plotting all aerial reflectivity:
plot(my_vpts, ylim = c(0, 3000), quantity="DBZH")
```

Note that we enclose the
[`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
function in [`tryCatch()`](https://rdrr.io/r/base/conditions.html) to
keep going after errors with specific files

### Parallel processing

We may use one of the parallelization packages in R to further speed up
our processing. We will use
[`mclapply()`](https://rdrr.io/r/parallel/mclapply.html) from package
`parallel`. First we wrap up the processing statements in a function, so
we can make a single call to a single file. We will disable MistNet, as
this deep-learning model does not parallelize well on a cpu machine.

``` r

process_file <- function(file_in){
  # construct output filename from input filename
  file_out <- paste(outputdir, "/", basename(file_in), "_vp.h5", sep = "")
  # run calculate_vp()
  vp <- tryCatch(calculate_vp(file_in, file_out, mistnet = FALSE, h_layer=50, n_layer=60), error = function(e) NULL)
  if(is.vp(vp)){
    # return TRUE if we calculated a profile
    return(TRUE)
  } else{
    # return FALSE if we did not
    return(FALSE)
  }
}

# To process a file, we can now run
process_file(my_pvolfiles[1])
```

Next, we use [`mclapply()`](https://rdrr.io/r/parallel/mclapply.html) to
do the parallel processing for all files:

``` r

# load the parallel library
library(parallel)
# detect how many cores we can use. We will keep 2 cores for other tasks and use the rest for processing.
number_of_cores = detectCores() - 2
# Available cores:
number_of_cores
# let's loop over the files and generate profiles
start=Sys.time()
mclapply(my_pvolfiles, process_file, mc.cores = number_of_cores)
end=Sys.time()
# calculate the processing time that has passed:
end-start
```

## Further analysis outside bioRad

In many cases you will want to convert bioRad’s objects into a
convenient form for your own further analyses. To convert bioRad objects
to a simple `data.frame`:

- vertical profile (`vp`): as.data.frame(`your_vp_object`)
- vertical profile time series (`vpts`):
  as.data.frame(`your_vpts_object`)
- integrated vertical profile time series (`vpi`): is already a
  `data.frame`

Converting polar scans (`scan` objects) to common spatial formats:

- use
  [`scan_to_raster()`](http://adriaandokter.com/bioRad/dev/reference/scan_to_raster.md)
  to convert to a `RasterBrick` object (package raster)
- use
  [`scan_to_spatial()`](http://adriaandokter.com/bioRad/dev/reference/scan_to_spatial.md)
  to convert to a `SpatialPointsDataFrame` object (package sp)

Converting PPI (`ppi` objects) to common spatial formats: \* access the
data slot (`my_ppi$data`) to extract a `SpatialGridDataFrame` object
(package sp)
