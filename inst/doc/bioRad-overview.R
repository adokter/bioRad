## ---- eval=FALSE---------------------------------------------------------
#  # make sure you start with a fresh R session
#  # load the bioRad package
#  library(bioRad)
#  # check the package version
#  packageVersion("bioRad")
#  # make sure you have the latest version (0.3.0). If you have an older version, update as follows:
#  library(devtools)
#  install_github("adokter/bioRad", ref = "ecography")

## ---- eval=FALSE---------------------------------------------------------
#  # bring up the package general help page:
#  ?bioRad

## ---- eval=FALSE---------------------------------------------------------
#  # make a new local directory on your machine where to download data for this practical
#  # replace the string below with the path of that directory:
#  HOME="your/personal/working/directory/"
#  # check that the directory exists. If the next statement evaluates to FALSE, something went wrong: the directory does not exist or you didn't specify its path correctly
#  file.exists(HOME)
#  # we will make HOME our work directory, the default folder in which R will look
#  # for files, and where it will output newly generated files.
#  setwd(HOME)
#  # Finally, we set the local time zone to UTC, so all plotted time axes will be in UTC
#  Sys.setenv(TZ="UTC")

## ---- eval=FALSE---------------------------------------------------------
#  # start your local Docker installation
#  # we first test whether R can communicate with Docker:
#  check_docker()

## ---- eval=FALSE---------------------------------------------------------
#  #  let's read in the downloaded volume:
#  file.in="KPAH20171025_040338_V06"
#  # or in case you downloaded the file that doesn't require docker, the filename is different:
#  file.in="volume.h5"
#  # check that the file is stored in the right location:
#  file.exists(file.in)
#  # load the polar volume:
#  pvol=read_pvolfile(file.in)
#  ## print some information about the polar volume
#  pvol
#  # print information about the polar scans contained in this polar volume:
#  pvol$scans

## ---- eval=FALSE---------------------------------------------------------
#  # let's extract the third scan, which was collected at 0.48 degree elevation:
#  pscan = pvol$scans[[1]]
#  # print some information about this scan:
#  pscan
#  # before we can plot the scan, we need to project it on a Cartesian grid,
#  # i.e. we need to make a Plan Position Indicator (PPI)
#  my_ppi = project_as_ppi(pscan,cellsize=1000,range.max=25000)
#  # print some information about this ppi:
#  str(my_ppi)
#  # you can see we projected it on a 500 metre grid.
#  # Check the manual of the ppi function to see how you can change the projection
#  # Now we are ready to plot the ppi
#  # plot the reflectivity factor image:
#  plot(my_ppi, param="VRADH",zlim=c(-30,30))
#  # see plot.ppi for all the plot options for a ppi object:
#  ?plot.ppi

## ---- eval=FALSE---------------------------------------------------------
#  # It is often informative to plot radar data on a base layer, such as google earth maps.
#  # first download the background image:
#  satelliteImage=download_basemap(my_ppi,maptype="satellite")
#  # then overlay the PPI on the satellite image:
#  map(my_ppi,param="DBZH",map=satelliteImage, zlim=c(-20,15))
#  # Note that in R, spatial data is often contained in class objects of packag 'sp'
#  # bioRad also uses these objects in the background, and they can be extracted if you want to.
#  # The spatial data is stored in the data slot, as in:
#  my_spatialgrid=my_ppi$data
#  # you can use the sp package to save the spatial data to all kinds GIS formats, for example ArcGis:
#  library(sp)
#  write.asciigrid(my_spatialgrid,"PPI_in_arcgis_format.asc")

## ---- eval=FALSE---------------------------------------------------------
#  # we will process the same file as in section 1.3:
#  file.in
#  # check whether the file is still there:
#  file.exists(file.in)
#  # run vol2bird
#  vp=calculate_vp(file.in,range.max=35000,sd_vvp=1,dealias=T,dualpol=T)
#  # vp is now a 'vp', a vertical profile
#  vp
#  # alternatively, you may also store the profile on disk as a hdf5 file, which is what we will do next:
#  # let's first define the name of the output file (we paste the extention ".h5" to the name)
#  file.out=paste(file.in,".h5",sep="")
#  # print the newly generated output file name to which we will write:
#  # note that this is only a string, you can give it any other name if you want to
#  file.out
#  # finally, run vol2bird; this generates an output file as specified in file.out
#  # we set autoconf to TRUE, to let vol2bird figure out the optimal settings by itself
#  calculate_vp(file.in,file.out,range.max=35000,sd_vvp=1,dealias=T,dualpol=T)
#  # your work directory should now contain a new file with the name you specified in file.out
#  # check that we can read this file, and retrieve the vertical profile from it:
#  vp=read_vpfiles(file.out)

## ---- eval=FALSE---------------------------------------------------------
#  # plot the vertical profile, in terms of reflectivity factor
#  plot(vp, quantity="dbz")
#  # plot the vertical profile, in terms of reflectivity
#  plot(vp, quantity="eta")

## ---- eval=FALSE---------------------------------------------------------
#  # print the currently assumed radar cross section (RCS) per bird:
#  rcs(vp)
#  # plot the vertical profile, in terms of bird density
#  plot(vp, quantity="dens")

## ---- eval=FALSE---------------------------------------------------------
#  # let's change the RCS to 110 cm^2
#  rcs(vp) = 110
#  plot(vp, quantity="dd")

## ---- eval=FALSE---------------------------------------------------------
#  # unzip the profiles.zip file, either by clicking the file or the command below; after unzipping you should have a folder 'profiles' with processed data
#  # load all the filenames in the new 'profiles' subfolder in your working directory
#  vp_paths=dir("./profiles", recursive=TRUE,full.names=TRUE)
#  vp_paths
#  # read these vertical profiles (hdf5 files) into R (may take a minute to load)
#  vplist=read_vpfiles(vp_paths)
#  # print some information on the vplist object. It should contain 71 profiles
#  vplist
#  # save the object, which allows you to load the data more quickly next time
#  save(vplist,file="vplist.RData")
#  # you can restore the vplist object at any time as follows:
#  load("vplist.RData")

## ---- eval=FALSE---------------------------------------------------------
#  # convert the list of vertical profiles into a time series:
#  # In case your vplist contains profiles of different radars, this function will
#  # group the profiles and make a separate time-series for each radar.
#  # (but in our case we have profiles for only one radar)
#  ts=vplist_to_vpts(vplist)
#  # print summary information
#  ts
#  # plot the time series in terms of reflectivity factors, from 0-2000 metre altitude:
#  plot(ts, quantity="dbz",ylim=c(0,2000))
#  # plot the time series in terms of bird density:
#  plot(ts, quantity="dens",ylim=c(0,2000))
#  # change the radar cross-section works the same as for single vertical profiles, for example:
#  rcs(ts)=20
#  # check the help file for more plotting options
#  # Because profile timeseries are of class 'vpts', it's associated plotting function
#  # is plot.vpts:
#  ?plot.vpts

## ---- eval=FALSE---------------------------------------------------------
#  # time series objects can be subsetted, just as you may be used to with vectors
#  # here we subset the first 50 timesteps:
#  ts[1:50]

## ---- eval=FALSE---------------------------------------------------------
#  # plot the time series
#  plot(ts, quantity="DBZH")

## ---- eval=FALSE---------------------------------------------------------
#  # You can also extract the data from bioRad's class objects into a simple R format:
#  #
#  # Extract some data from the time series, e.g. the bird density
#  get_quantity(ts, quantity="dens")
#  # convert all the data in the time series to a standard data.frame:
#  my_dataframe=as.data.frame(ts)
#  my_dataframe

## ---- eval=FALSE---------------------------------------------------------
#  # Let's continue with the ts object created in the previous example.
#  # The vertically integrated quantities are calculated as follows:
#  integrated.ts = integrate_profile(ts)
#  # plot the integrated data to screen:
#  integrated.ts
#  # The integrated.ts object you created is a vpi class object, which is an acronym for Vertical Profiles Integrated. It has its own plot method, which by default plots migration traffic rate (MTR):
#  plot(integrated.ts)
#  # you can also plot vertically integrated densities (VID):
#  plot(integrated.ts, quantity="vid")

## ---- eval=FALSE---------------------------------------------------------
#  # Let's continue with the ts object created in the previous example.
#  # we can integrate all the traffic rates [unit: birds/km/h] over time, to obtain the number of birds that
#  # have passed over the radar [unit: birds/km]. Like the traffic rates, these numbers are calculated for
#  # a 1 km transect perpendicular to the migratory direction, and stored in column mt of the object:
#  plot(integrated.ts, quantity="mt")
#  

