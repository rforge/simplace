# Functions to communicate with Simplace
#
# Provides methods to run Solutions and Projects in Simplace 
# and to retrieve and convert the data 
# 
# @author Gunther Krauss
###############################################################################


# Some helper definitions for calling/parsing

nullObject <- rJava::.jnull(class="java/lang/Object") # java null object
nullString <- rJava::.jnull(class="java/lang/String") # java null string


#' Initialisation of Framework
#' 
#' Initializes the JVM and creates the SimplaceWrapper object which 
#' is used to interact with Simplace.
#' 
#' @param InstallationDir directory where simplace, lap and simplacerun are located
#' @param WorkDir working directory where solutions, projects and data resides (_WORKDIR_)
#' @param OutputDir directory for output (_OUTPUTDIR_)
#' @param additionalClasspaths vector with class paths relative to InstallationDir that are to be added
#' @param javaparameters parameters that are passed to the java virtual machine
#' @return handle to the SimplaceWrapper object
#' @export
initSimplace <- function(InstallationDir,WorkDir,OutputDir,additionalClasspaths = c(),javaparameters = getOption("java.parameters"))
{
  
  rJava::.jinit(parameters=javaparameters) # inits java
  
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
    "simplace/lib/org.eclipse.mylyn.wikitext.core_2.0.0.20140108-1934.jar",
    "simplace/lib/org.eclipse.mylyn.wikitext.tracwiki.core_2.0.0.20131126-1957.jar",
    "simplace/lib/commons-jexl-2.1.1.jar",
    "simplace/res/files",
    
    "simplace/lib/geotools/commons-logging-1.1.1.jar",
    "simplace/lib/jcifs-1.3.17.jar",
    
    additionalClasspaths
  )
  sapply(classpaths, function(s) rJava::.jaddClassPath(paste(InstallationDir,s,sep="")))  
  
  # create and return an instance of RSimplaceWrapper class
  rJava::.jnew("net/simplace/simulation/wrapper/SimplaceWrapper", WorkDir, OutputDir)
}



######################## Call Simplace Methods ######################


#' Opens a Simplace project
#' 
#' Initializes a project. The absolute path to a solution file is mandatory. 
#' Project file is optional.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param solution solution file with absolute path
#' @param project project file with absolute path, can be omitted to run solution only
#' @seealso \code{\link{closeProject}}
#' @export
openProject <- function (simplace, solution, project=nullString)
{
  rJava::.jcall(simplace, "Lnet/simplace/simulation/FWSimSession;", "prepareSession", project, solution)
}


#' Close Project
#' 
#' Call to the finalize method of the simulation.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @seealso \code{\link{openProject}}
#' @export
closeProject <- function (simplace)
{
  rJava::.jcall(simplace,"V","finalize")
}


#' Creates a simulation and substitute parameters
#' 
#' Creates a simulation from the opened project and substitutes
#' the values of the parameters given in the parameter list. 
#' Simulation will be put into the queue by default.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param parameterList a list with the parameter name as key and parametervalue as value
#' @param queue boolean - simulation is added to queue if true, else start new queue 
#' @return id of the created simulation
#' @export
#' @seealso \code{\link{runSimulations}}, \code{\link{resetSimulationQueue}}
createSimulation <- function (simplace,parameterList=NULL, queue=TRUE) {
  paramObject <- parameterListToStringArray(parameterList)
  if(queue)
  {
    rJava::.jcall(simplace,"V","resetSimulationQueue") 
  }
  id <- rJava::.jcall(simplace,"Lnet/simplace/simulation/FWSimSimulation;","createSimulation",paramObject)
  rJava::.jcall(id,"S","getID")
}


#' Run the created simulations
#' 
#' Run the created simulations from the queue. If the queue is empty, the
#' last created simulation will be run.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param updateresources if true update ressources
#' @param selectsimulation if true keeps a selected simulation
#' @export
#' @seealso \code{\link{createSimulation}}, \code{\link{resetSimulationQueue}}
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' parameters <- list()
#' parameters$vLUE <- 3.0
#' s1 <- createSimulation(simplace, parameters,queue=TRUE)
#' parameters$vLUE <- 3.2
#' s2 <- createSimulation(simplace, parameters,queue=TRUE)
#' runSimulations(simplace)
#' ...
#' parameters$vLUE <- 2,8
#' s3 <- createSimulation(simplace, parameters,queue=TRUE)
#' runSimulations(simplace)
#' ...
#' closeProject(simplace)   }
runSimulations <- function(simplace, updateresources=FALSE,selectsimulation=FALSE)
{
  rJava::.jcall(simplace, "V", "runSimulations", updateresources, selectsimulation )
}


#' Clears the list of simulations
#' 
#' Simulation list is cleared
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @export
#' @seealso \code{\link{createSimulation}}, \code{\link{runSimulations}}
resetSimulationQueue <- function (simplace) {
  rJava::.jcall(simplace,"V","resetSimulationQueue")
}


#' Runs the opened project
#' 
#' Runs the simulation(s) as defined in the solution and project files. 
#' There is no accessible MEMORY output, but one can load the CSV or 
#' database output.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @export
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution, Project)
#' runProject(simplace)
#' ...
#' closeProject(simplace)   }
runProject <- function(simplace) {
  rJava::.jcall(simplace,"V","run")
}


#' Run simulation stepwise
#' 
#' Performs \code{count} steps of the simulation and returns the values from 
#' the actual variable map. Can be called consecutively.
#' 
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
    rJava::.jcall(simplace, "Lnet/simplace/simulation/wrapper/SimplaceWrapper$DataContainer;", "step", filter, as.integer(count))
  else
    rJava::.jcall(simplace, "Lnet/simplace/simulation/wrapper/SimplaceWrapper$DataContainer;", "step", as.integer(count))
}


#' Lists IDs of the performed simulations
#' 
#' Returns a vector with the IDs of the simulations. IDs are required to
#' get the output of the simulations.
#' 
#' @param simplace handle to the SimplaceWrapper object
#' @return list with the IDs
#' @export
getSimulationIDs <- function(simplace)
{
  rJava::.jcall(simplace, "[S", "getSimulationIDs")
}


#' Fetch output from a simulation
#' 
#' The output is a JavaObject containing the variable names, 
#' data types, units and the values. Output can be converted
#' with \code{\link{resultToList}} or \code{\link{resultToDataframe}}
#' to R objects. Only MEMORY outputs are accessible. For CSV or 
#' database outputs you have to read the data by generic methods.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param outputId id of the output. Only MEMORY outputs are accessible.
#' @param simulationId id of the simulation
#' @return handle to the data container which has to be processed afterwards
#' @export
getResult <- function(simplace, outputId, simulationId)
{
  rJava::.jcall(simplace,"Lnet/simplace/simulation/wrapper/SimplaceWrapper$DataContainer;","getResult",outputId, simulationId);
}



############# Convert Java Objects to R #################


#' Converts a list of named parameters to a java string array
#'
#' @param parameterList list of n parameter values indexed by parameter name
#' @return a java object of type Object[n][2]
#' @keywords internal
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
      objlist[i] <-rJava::.jarray(c(name, value))   # add array entry = {key, value}
    }
    rJava::.jcast(rJava::.jarray(objlist),"[[Ljava/lang/Object;") # convert list to java array and cast it to Object[][]
  }
  else
  {
    rJava::.jcast(nullObject,"[[Ljava/lang/Object;")
  }
}


#' Converts the varmap to a list
#' 
#' Converts the varMap to a list. All elements are converted to appropriate
#' R objects. Arrays are expanded to vectors by default.
#' 
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
  headerarray <- rJava::.jcall(varmap,"[S","getHeaderStrings")
  types <- rJava::.jcall(varmap,"[S","getTypeStrings")
  #  units <- .jcall(varmapObj,"[S","getHeaderUnits")
  data <- rJava::.jcall(varmap,"[Ljava/lang/Object;","getDataObjects")
  names(data) <- headerarray
  if(expand)
  {
    for(i in (1:length(headerarray)))
    {
      if(types[i]=="NULL")
        data[[i]] <- NA
      else if (types[i]=="DOUBLEARRAY" || types[i]=="INTARRAY")
      {
        data[[i]] <-rJava::.jevalArray(data[[i]],simplify=TRUE)
      }  
      else if (types[i]=="DATE")
      {
        data[[i]] <- as.Date(rJava::.jcall(data[[i]],"S","toString"))   
      }  
      else
        data[[i]] <- rJava::.jsimplify(data[[i]])
      
    }
  }
  data
}


#' Convert result to list
#' 
#' Converts all scalar output columns to appropriate R lists. Columns containing
#' arrays are left unchanged, unless expand is TRUE. 
#' 
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
  headerarray <- rJava::.jcall(result,"[S","getHeaderStrings")
  types <- rJava::.jcall(result,"[S","getTypeStrings")
  if(!is.null(from) && !is.null(to) && to>=from && from >=0)
    data <- rJava::.jcall(result,"[Ljava/lang/Object;","getDataObjects",as.integer(from),as.integer(to))
  else
    data <- rJava::.jcall(result,"[Ljava/lang/Object;","getDataObjects")
  for(i in (1:length(headerarray)))
  {
    if(types[i]=="NULL")
      data[[i]] <- NA
    else if (expand & types[i]=="INTARRAY")
    {
      data[[i]] <-rJava::.jevalArray(data[[i]],rawJNIRefSignature="[Ljava/lang/Integer;")
      data[[i]] <-lapply(data[[i]],function(a)rJava::.jevalArray(a, simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Integer;"))
    }  
    else if (expand & types[i]=="DOUBLEARRAY")
    {
      data[[i]] <-rJava::.jevalArray(data[[i]],rawJNIRefSignature="[Ljava/lang/Double;")
      data[[i]] <-lapply(data[[i]],function(a)rJava::.jevalArray(a, simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Double;"))
    }  
    else if (types[i]=="DATE")
      data[[i]] <- as.Date(rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;")) 
    else if(types[i]=="DOUBLE")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Double;")
    else if(types[i]=="INT")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Integer;")
    else if(types[i]=="CHAR")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;")
    else if(types[i]=="BOOLEAN")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Boolean;")
  }
  names(data)<-headerarray
  data
}


#' Convert result to dataframe
#' 
#' All scalar output columns are transformed to appropriate R objects
#' and then glued together in a dataframe. Array outputs columns are ignored.
#' 
#' 
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
  oheaderarray <- rJava::.jcall(result,"[S","getHeaderStrings")
  otypes <- rJava::.jcall(result,"[S","getTypeStrings")
  if(!is.null(from) && !is.null(to) && to>=from && from >=0)
    odata <- rJava::.jcall(result,"[Ljava/lang/Object;","getDataObjects",as.integer(from),as.integer(to))
  else
    odata <- rJava::.jcall(result,"[Ljava/lang/Object;","getDataObjects")
  
  index <- (otypes!="DOUBLEARRAY" & otypes!="INTARRAY")
  headerarray <- oheaderarray[index]
  types <- otypes[index]
  data <- odata[index]
  for(i in (1:length(headerarray)))
  {
    if(types[i]=="NULL")
      data[[i]] <- NA
    else if (types[i]=="DATE")
      data[[i]] <- as.Date(rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;"))
    else if(types[i]=="DOUBLE")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Double;")
    else if(types[i]=="INT")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Integer;")
    else if(types[i]=="CHAR")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;")
    else if(types[i]=="BOOLEAN")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Boolean;")
  }
  names(data)<-headerarray
  do.call(cbind.data.frame,data)
}


#' Get the units of the result variables
#' 
#' Get the units of each variable (i.e. data column) in a 
#' human readable format.
#' The output is a named character vector, where each element is named
#' by the variables name.
#' 
#' @param result handle to the data container returned by \code{\link{getResult}}
#' @return character vector with the units
#' @export
getUnitsOfResult <- function(result)
{
  units <- rJava::.jcall(result,"[S","getHeaderUnits")
  names(units) <-  rJava::.jcall(result,"[S","getHeaderStrings")
  units
}

############# Additional functions ################


#' Sets the log level of simplace
#' 
#' Sets the level of logger output - FATAL is least verbose,
#' TRACE most verbose. You have to call \code{\link{initSimplace}} first.
#' 
#' @param level is a string with possible values: FATAL, ERROR, WARN, INFO, DEBUG, TRACE
#' @export
#' @examples 
#' \dontrun{
#' setLogLevel("INFO")}
setLogLevel <- function(level)
{
  lg <- rJava::.jnew("net/simplace/simulation/io/logging/Logger")
  lgl <- switch(level,
    FATAL = lg$LOGLEVEL$FATAL,
    ERROR = lg$LOGLEVEL$ERROR,
    WARN = lg$LOGLEVEL$WARN,
    INFO = lg$LOGLEVEL$INFO,
    DEBUG = lg$LOGLEVEL$DEBUG,
    TRACE = lg$LOGLEVEL$TRACE,
    lg$LOGLEVEL$INFO
    )
  rJava::.jcall("net/simplace/simulation/io/logging/Logger","V","setLogLevel",lgl)
}


#' Sets number of used CPUs
#' 
#' Sets the number of processors that are used parallel.
#' The function can be used only after \code{\link{initSimplace}} has been
#' called.
#'
#' @param count number of processors
#' @export
setSlotCount <- function(count) {
  rJava::.jcall("net/simplace/simulation/FWSimEngine","V","setSlotCount",as.integer(count))
}
