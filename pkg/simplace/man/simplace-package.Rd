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
Version: \tab 0.90\cr
Date: \tab 2014-11-01\cr
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

SimplaceWorkDir <- "D:/java/simplace/simplacerun/simulation/"
SimplaceOutputDir <-  "D:/java/simplace/simplacerun/output/"

Solution <- "D:/java/simplace/simplacerun/simulation/gk/solution/complete/Complete.sol.xml"

simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)

openProject(simplace, Solution)

parameter <- list()
parameter$vTempLimit <- 32

simid <- createSimulation(simplace,parameter)
runSimulations(simplace)

result <- getResult(simplace,"DIAGRAM_OUT", simid);

closeProject(simplace)

resultlist <- resultToList(result)
resultframe <- resultToDataframe(result)
}
}
