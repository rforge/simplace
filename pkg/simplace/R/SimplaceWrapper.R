# Functions to communicate with Simplace
#
# Provides methods to run Solutions and Projects in Simplace 
# and to retrieve and convert the data 
# 
#
###############################################################################

# Some helper definitions for calling/parsing


nullObject <- .jnull(class="java/lang/Object") # java null object
nullString <- .jnull(class="java/lang/String") # java null string
dateAttributes <- attributes(as.POSIXct("2000-01-01")) # attribute to apply to date field/vector


#' Initialisation of Framework
#' 
#' Initializes the JVM and creates the SimplaceWrapper object which 
#' is used to interact with Simplace.
#' 
#' @param InstallationDir directory where simplace, lap and simplacerun are located
#' @param WorkDir working directory where solutions, projects and data resides (_WORKDIR_)
#' @param OutputDir directory for output (_OUTPUTDIR_)
#' @return handle to the SimplaceWrapper object
#' @export
initSimplace <- function(InstallationDir,WorkDir,OutputDir)
{
  
  .jinit() # inits java
  
  # add the classpaths  
  classpaths = c(
    "simplace/build/classes",
    "simplace/conf",
    "lap/build/classes",
    "simplacerun/build/classes",
    "simplacerun/conf",
    "simplace/lib/simplace.jar",
    "simplace/lib/simplace_run.jar",
    "simplace/lib/simplace-lap.jar",
    "simplace/lib/schmitzm-core-2.7-SNAPSHOT.jar",
    "simplace/lib/schmitzm-gt-2.7-SNAPSHOT.jar",
    "simplace/lib/hsqldb-1.8.0.7.jar",
    "simplace/lib/schmitzm/schmitzm-core-2.7-SNAPSHOT.jar",
    "simplace/lib/schmitzm/schmitzm-gt-2.7-SNAPSHOT.jar",
    "simplace/lib/geotools/hsqldb-1.8.0.7.jar",
    "simplace/lib/commons-io-1.3.1.jar",
    "simplace/lib/commons-lang-2.4.jar",
    "simplace/lib/h2-1.1.117.jar",
    "simplace/lib/javaws.jar",
    "simplace/lib/jaxen-full.jar",
    "simplace/lib/jdom.jar",
    "simplace/lib/jfreechart-1.0.14.jar",
    "simplace/lib/jcommon-1.0.17.jar",
    "simplace/lib/jRegistryKey.jar",
    "simplace/lib/log4j-1.2.15.jar",
    "simplace/lib/oro.jar",
    "simplace/lib/saxpath.jar",
    "simplace/lib/xercesImpl-2.7.1.jar",
    "simplace/lib/jena-2.6.4.jar",
    "simplace/lib/iri-0.8.jar",
    "simplace/lib/icu4j-3.4.4.jar",
    "simplace/lib/slf4j-api-1.5.11.jar",
    "simplace/lib/slf4j-log4j12-1.5.11.jar",
    "lapclient/lib/ant.jar",
    "lapclient/lib/ant-launcher.jar",
    "lapclient/lib/jtds.jar",
    "lapclient/lib/postgresql.jar",
    "simplace/res/files"
  )
  sapply(classpaths, function(s) .jaddClassPath(paste(InstallationDir,s,sep="")))  
  
  # create and return an instance of RSimplaceWrapper class
  .jnew("net/simplace/simulation/wrapper/SimplaceWrapper", WorkDir, OutputDir)
}



######################## Call Simplace Methods ######################


#' Initializes a simplace project (solution and projectfile)
#' 
#' Initializes a project. The absolute path to a solution file is mandatory. 
#' Project file is optional.
#' 
#' @title Open Project
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param solution solution file with absolute path
#' @param project project file with absolute path, can be omitted to run solution only
#' @seealso \code{\link{closeProject}}
#' @export
openProject <- function (simplace, solution, project=nullString)
{
  .jcall(simplace, "V", "prepareProject", project, solution)
}


#' Finalizes the project
#' 
#' Call to the finalize method of the simulation.
#' 
#' @title Close Project
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @seealso \code{\link{openProject}}
#' @export
closeProject <- function (simplace)
{
  .jcall(simplace,"V","finalize")
}


#' Creates a simulation from the solution and sets some parameters
#' 
#' Creates a simulation from the opened project and substitutes
#' the values of the parameters given in the parameter list. 
#' Simulation can be enqueued, so that they run in parallel.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param parameterList a list with the parameter name as key and parametervalue as value
#' @param queue if true put simulation to the prepared project to run them in parallel 
#' @return handle to the simulation
#' @export
createSimulation <- function (simplace,parameterList=NULL, queue=TRUE) {
  paramObject <- parameterListToStringArray(parameterList)
  .jcall(simplace,"Lnet/simplace/simulation/FWSimSimulation;","createSimulation",paramObject,queue)
}


#' Run the simulations
#' 
#' Run the created simulations. 
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param updateresources if true update ressources
#' @param selectsimulation if true keeps a selected simulation
#' @export
runSimulations <- function(simplace, updateresources=FALSE,selectsimulation=FALSE)
{
  .jcall(simplace, "V", "runSimulations", updateresources, selectsimulation )
}


#' Runs a simulation stepwise
#' 
#' Performs \code{count} steps of the simulation and returns the values from 
#' the actual variable map. Can be called consecutively.
#' 
#' @title Run simulation step
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param filter vector of the variable names to be included in the result. If not set, all variables are returned
#' @param count number of steps to be performed
#' @return handle to the data container which has to be processed afterwards
#' @export
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' vm <- stepSimulation(simplace,count=22)
#' vm_s <- stepSimulation(simplace,filter=c("CURRENT.DATE","LintulBiomass.sWSO"),count=18)
#' closeProject(simplace)   }

#' 
stepSimulation <- function (simplace, count=1,filter=NULL)
{
  if(class(filter)=="character" && length(filter)>0)
    .jcall(simplace, "Lnet/simplace/simulation/wrapper/SimplaceWrapper$DataContainer;", "step", filter, as.integer(count))
  else
    .jcall(simplace, "Lnet/simplace/simulation/wrapper/SimplaceWrapper$DataContainer;", "step", as.integer(count))
}


#' List with the IDs of the performed simulations
#' 
#' Returns a vector with the IDs of the simulations. IDs are required to
#' get the output of the simulations.
#' 
#' @param simplace handle to the SimplaceWrapper object
#' @return list with the IDs
#' @export
getSimulationIDs <- function(simplace)
{
  .jcall(simplace, "[S", "getSimulationIDs")
}


#' Fetch output from a simulation
#' 
#' The output is a JavaObject containing the variable names, 
#' data types, units and the values. Output can be converted
#' with \code{\link{resultToList}} or \code{\link{resultToDataframe}}
#' to R objects.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param outputId id of the output
#' @param simulationId id of the simulation
#' @return handle to the data container which has to be processed afterwards
#' @export
getResult <- function(simplace, outputId, simulationId)
{
  .jcall(simplace,"Lnet/simplace/simulation/wrapper/SimplaceWrapper$DataContainer;","getResult",outputId, simulationId);
}



############# Convert Java Objects to R #################


#' Converts a list of named parameters to a java string array
#'
#' @param parameterList list of n parameter values indexed by parameter name
#' @return a java object of type Object[n][2]
parameterListToStringArray <- function (parameterList) 
{
  if(!is.null(parameterList) && length(parameterList)>0)
  {
    objlist <- vector("list",length(parameterList));    # creates an empty list
    names <- names(parameterList)    # get the keys
    
    for(i in 1:length(parameterList))
    {
      name <- names[[i]]   # key
      value <- toString(parameterList[[i]])   # value
      objlist[i] <-.jarray(c(name, value))   # add array entry = {key, value}
    }
    .jcast(.jarray(objlist),"[[Ljava/lang/Object;") # convert list to java array and cast it to Object[][]
  }
  else
  {
    .jcast(nullObject,"[[Ljava/lang/Object;")
  }
}


#' Converts the varmap to a list
#' 
#' Converts the varMap to a list. All elements are converted to appropriate
#' R objects. Arrays are expanded to vectors by default.
#' 
#' @title Convert varmap to list
#' @param varmap the varMap returned by \code{\link{stepSimulation}}
#' @param expand if \code{TRUE} expand array objects to vector.
#' @return list with parameter name as key and parameter value as value
#' @export
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' varmap <- stepSimulation(simplace,count=22)
#' closeProject(simplace)   
#' 
#' varlist <- varmapToList(varmap)
#' 
#' varlist$startdate - 24*3600
#' varlist$LintulBiomass.sWSO}

varmapToList <- function(varmap,expand=TRUE)
{
  headerarray <- .jcall(varmap,"[S","getHeaderStrings")
  types <- .jcall(varmap,"[S","getTypeStrings")
  #  units <- .jcall(varmapObj,"[S","getHeaderUnits")
  data <- .jcall(varmap,"[Ljava/lang/Object;","getDataObjects")
  names(data) <- headerarray
  if(expand)
  {
    for(i in (1:length(headerarray)))
    {
      if(types[i]=="NULL")
        data[[i]] <- NA
      else if (types[i]=="DOUBLEARRAY" || types[i]=="INTARRAY")
      {
        data[[i]] <-.jevalArray(data[[i]],simplify=TRUE)
      }  
      else if (types[i]=="DATE")
      {
        data[[i]] <- .jcall(data[[i]],"J","getTime")/1000
        attributes(data[[i]]) <- dateAttributes
      }  
      else
        data[[i]] <- .jsimplify(data[[i]])
      
    }
  }
  data
}

#' Converts the simulation output to a list. Arrays are not expanded by default
#' 
#' Converts all scalar columns to appropriate R lists. Columns containing
#' arrays are left unchanged, unless expand is TRUE. 
#' 
#' @title Convert result to list
#' @param result handle to the data container returned by \code{\link{getResult}}
#' @param expand if true columns with arrays are partially expanded
#' @param from start of the result range, if to/from are not set, full result is returned
#' @param to end of the result range, if to/from are not set, full result is returned
#' @return list with output columns
#' @seealso \code{\link{resultToDataframe}} returns the scalar output columns as \code{\link{data.frame}}
#' @export
#' @examples 
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' parameter <- list()
#' parameter$vTempLimit <- 32
#' createSimulation(simplace,parameter)
#' runSimulations(simplace)
#' simulationlist <-getSimulationIDs(simplace)
#' result <- getResult(simplace,"DIAGRAM_OUT", simulationlist[[1]]);
#' closeProject(simplace)
#' resultlist <- resultToList(result)
#' resullist$CURRENT.DATE}
resultToList <-function(result,expand=FALSE,from=NULL,to=NULL) {
  headerarray <- .jcall(result,"[S","getHeaderStrings")
  types <- .jcall(result,"[S","getTypeStrings")
  if(!is.null(from) && !is.null(to) && to>=from && from >=0)
    data <- .jcall(result,"[Ljava/lang/Object;","getDataObjects",as.integer(from),as.integer(to))
  else
    data <- .jcall(result,"[Ljava/lang/Object;","getDataObjects")
  for(i in (1:length(headerarray)))
  {
    if(types[i]=="NULL")
      data[[i]] <- NA
    else if (expand & (types[i]=="DOUBLEARRAY" | types[i]=="INTARRAY"))
    {
      data[[i]] <-.jevalArray(data[[i]],simplify=TRUE)
      data[[i]] <-lapply(data[[i]],function(a).jevalArray(a, simplify=TRUE))
    }  
    else if (types[i]=="DATE")
    {
      data[[i]] <- sapply(unlist(.jevalArray(data[[i]])),function(o){.jsimplify(o)/1000})
      attributes(data[[i]]) <- dateAttributes
    }  
    else if(types[i]=="DOUBLE")
      data[[i]] <- .jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Double;")
    else if(types[i]=="INT")
      data[[i]] <- .jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Integer;")
    else if(types[i]=="CHAR")
      data[[i]] <- .jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;")
  }
  names(data)<-headerarray
  data
}


#' Converts the simulation output to a dataframe ignogring array values
#' 
#' All scalar output columns are transformed to appropriate R objects
#' and then glued together in a dataframe
#' 
#' @title Convert result to dataframe
#' @param result handle to the data container returned by \code{\link{getResult}}
#' @param from start of the result range, if to/from are not set, full result is returned
#' @param to end of the result range, if to/from are not set, full result is returned
#' @return data.frame with scalar output columns
#' @seealso \code{\link{resultToList}} returns the output columns as list
#' @export
#' @examples 
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' parameter <- list()
#' parameter$vTempLimit <- 32
#' createSimulation(simplace,parameter)
#' runSimulations(simplace)
#' simulationlist <-getSimulationIDs(simplace)
#' result <- getResult(simplace,"DIAGRAM_OUT", simulationlist[[1]]);
#' closeProject(simplace)
#' resultframe <- resultToDataframe(result)
#' resultframe[3,]}
resultToDataframe <- function(result,from=NULL,to=NULL) {
  oheaderarray <- .jcall(result,"[S","getHeaderStrings")
  otypes <- .jcall(result,"[S","getTypeStrings")
  if(!is.null(from) && !is.null(to) && to>=from && from >=0)
    odata <- .jcall(result,"[Ljava/lang/Object;","getDataObjects",as.integer(from),as.integer(to))
  else
    odata <- .jcall(result,"[Ljava/lang/Object;","getDataObjects")
  
  index <- (otypes!="DOUBLEARRAY" & otypes!="INTARRAY")
  headerarray <- oheaderarray[index]
  types <- otypes[index]
  data <- odata[index]
  for(i in (1:length(headerarray)))
  {
    if(types[i]=="NULL")
      data[[i]] <- NA
    else if (types[i]=="DATE")
    {
      data[[i]] <- sapply(unlist(.jevalArray(data[[i]])),function(o){.jsimplify(o)/1000})
      attributes(data[[i]]) <- dateAttributes
    }  
    else if(types[i]=="DOUBLE")
      data[[i]] <- .jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Double;")
    else if(types[i]=="INT")
      data[[i]] <- .jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Integer;")
    else if(types[i]=="CHAR")
      data[[i]] <- .jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;")
  }
  names(data)<-headerarray
  do.call(cbind.data.frame,data)
}
