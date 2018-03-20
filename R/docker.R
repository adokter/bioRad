#' flag indicating whether docker is running
#' @keywords internal
docker=F
#' flag indicating whether vol2bird docker container is mounted
#' @keywords internal
mounted=F
#' @keywords internal
#' the current mountpoint of the vol2bird docker container
mount="~/"

#' Checks that Docker is running
#'
#' Checks that \href{https://www.docker.com/}{Docker} daemon is running correctly on the local system
#' @param verbose logical which indicates whether to print test results to R console. On Windows always TRUE.
#' @export
#' @return 0 upon success, otherwise an error code.
checkDocker = function(verbose=T){
  if(.Platform$OS.type=="unix"){
    system("docker rm -f hello-world",ignore.stderr=T,ignore.stdout=T)
    result=system("docker run --name hello-world hello-world",ignore.stderr=!verbose,ignore.stdout=!verbose)
  }
  else{
    suppressWarnings(system("docker rm -f hello-world",ignore.stderr=T,ignore.stdout=T,show.output.on.console=FALSE))
    result=suppressWarnings(system("docker run --name hello-world hello-world",ignore.stderr=!verbose,ignore.stdout=!verbose,show.output.on.console = TRUE))
  }
  parent.env=environment(checkDocker)
  unlockBinding("docker", parent.env)
  unlockBinding("mounted", parent.env)
  parent.env$docker=(result==0)
  parent.env$mounted=F
  lockBinding("docker", parent.env)
  lockBinding("mounted", parent.env)
  if(!verbose) return(result)
}

#' Update Docker image for vol2bird
#'
#' Pulls and installs the latest Docker image used by bioRad from Docker hub
#' @details
#' This command pulls the latest \href{https://hub.docker.com/r/adokter/vol2bird/}{vol2bird} Docker image from \href{https://hub.docker.com}{Docker hub}.
#' Run this command to ensure all Docker functionality (e.g. the \link{vol2bird} function) runs at the latest available version.
#' @export
#' @return the POSIXct creation date of the installed Docker image
updateDocker = function(){
  creationDate=NULL
  if(.Platform$OS.type=="unix"){
    result=system("docker pull adokter/vol2bird:latest")
    if(result==0) creationDate=system("docker inspect -f '{{ .Created }}' adokter/vol2bird:latest",intern=T)
  }
  else{
    result=suppressWarnings(system("docker pull adokter/vol2bird:latest"))
    if(result==0) creationDate=suppressWarnings(system("docker inspect -f '{{ .Created }}' adokter/vol2bird:latest",intern=T))
  }
  if(!is.null(creationDate)) creationDate=as.POSIXct(creationDate,format="%Y-%m-%dT%T")
  return(creationDate)
}

startContainer = function(mount="~/"){
  parent.env=environment(startContainer)
  # if docker not running, cannot start container
  if(!parent.env$docker) return(1)
  # if container already running at this mount point, nothing to be done:
  if(parent.env$mounted & parent.env$mount==mount) return(0)
  # remove any existing vol2bird containers
  if(.Platform$OS.type=="unix") system("docker rm -f vol2bird", ignore.stderr=T,ignore.stdout=T)
  else suppressWarnings(system("docker rm -f vol2bird", ignore.stderr=T,ignore.stdout=T,show.output.on.console = FALSE))
  # fire up the container:
  if(.Platform$OS.type=="unix") result=system(paste("docker run -v ",normalizePath(mount,winslash="/"),":/data -d --name vol2bird adokter/vol2bird sleep infinity",sep=""),ignore.stdout=T)
  else result=suppressWarnings(system(paste("docker run -v ",normalizePath(mount,winslash="/"),":/data -d --name vol2bird adokter/vol2bird sleep infinity",sep=""),ignore.stdout=T,show.output.on.console=FALSE))
  if(result!=0) warning(paste("failed to mount",mount,"... Go to 'Docker -> preferences -> File Sharing' and add this directory (or its root directory) as a bind mounted directory"))
  else{
    unlockBinding("mounted",parent.env)
    unlockBinding("mount",parent.env)
    parent.env$mounted=(result==0)
    parent.env$mount=mount
    lockBinding("mounted", parent.env)
    lockBinding("mount", parent.env)
  }
  return(result)
}

setLoadActions(function(ns)
  cat("Loading package", sQuote(getNamespaceName(ns)),"version",as.character(packageVersion(getNamespaceName(ns))),"...\n"),
  function(ns) if((checkDocker(verbose=F)!=0)){
    cat("Warning: no running Docker daemon found\n")
    cat("Warning:",getNamespaceName(ns),"functionality requiring Docker has been disabled\n\n")
    cat("To enable Docker functionality, start Docker and run 'checkDocker()' in R")
    unlockBinding("docker", environment(checkDocker))
    assign("docker", F, envir = ns)
  }
  else{
    cat("Docker daemon running, Docker functionality enabled.\n")
  }
)
