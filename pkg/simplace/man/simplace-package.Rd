\name{simplace-package}
\alias{simplace-package}
\alias{simplace}
\docType{package}
\title{Interface to use the modelling framework SIMPLACE}
\description{Interface to interact with the modelling framework SIMPLACE and to parse the
    results of simulations.
}
\details{
\tabular{ll}{
Package: \tab simplace\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2013-11-28\cr
License: \tab GPL-2\cr
}
Package needs a Java Runtime Environment as well as an installation of SIMPLACE. See \href{http://www.simplace.net/}{www.simplace.net} for more information about SIMPLACE.
}
\author{
Gunther Krauss

Maintainer: Gunther Krauss <guntherkrauss@uni-bonn.de>
}
\references{\href{http://www.simplace.net/}{www.simplace.net}}
\keyword{ package }

\examples{
\dontrun{
SimplaceInstallationDir <- "D:/java/simplace/"
SimplaceWorkDir <- paste(SimplaceInstallationDir,"simplacerun/simulation/",sep="")
SimplaceOutputDir <-  paste(SimplaceInstallationDir,"simplacerun/output/",sep="")
Solution <- paste(SimplaceWorkDir,"gk/solution/complete/Complete.sol.xml",sep="")

simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)

openProject(simplace, Solution)

parameter <- list()
parameter$vTempLimit <- 32

createSimulation(simplace,parameter)
runSimulations(simplace)

simulationlist <-getSimulationIDs(simplace)

result <- getResult(simplace,"DIAGRAM_OUT", simulationlist[[1]]);

closeProject(simplace)

resultlist <- resultToList(result)
resultframe <- resultToDataframe(result)
}
}
