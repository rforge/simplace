\name{stepSimulation}
\alias{stepSimulation}
\title{Run simulation step}
\usage{
  stepSimulation(simplace, count = 1, filter = NULL)
}
\arguments{
  \item{simplace}{handle to the SimplaceWrapper object
  returned by \code{\link{initSimplace}}}

  \item{filter}{vector of the variable names to be included
  in the result. If not set, all variables are returned}

  \item{count}{number of steps to be performed}
}
\value{
  handle to the data container which has to be processed
  afterwards
}
\description{
  Runs a simulation stepwise
}
\details{
  Performs \code{count} steps of the simulation and returns
  the values from the actual variable map. Can be called
  consecutively.
}
\examples{
\dontrun{
simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
openProject(simplace, Solution)
varmap1_22 <- stepSimulation(simplace,count=22)
simple_varmap23_40 <- stepSimulation(simplace,filter=c("CURRENT.DATE","LintulBiomass.sWSO"),count=18)
closeProject(simplace)   }
}

