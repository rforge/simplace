\name{runProject}
\alias{runProject}
\title{Runs the opende project}
\usage{
  runProject(simplace)
}
\arguments{
  \item{simplace}{handle to the SimplaceWrapper object
  returned by \code{\link{initSimplace}}}
}
\description{
  Runs the simulation(s) as defined in the solution and
  project files.
}
\examples{
\dontrun{
simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
openProject(simplace, Solution, Project)
runProject(simplace)
...
closeProject(simplace)   }
}

