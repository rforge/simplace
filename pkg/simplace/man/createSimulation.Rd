\name{createSimulation}
\alias{createSimulation}
\title{Creates a simulation from the solution and changes some parameters}
\usage{
createSimulation(simplace, parameterList = NULL, queue = TRUE)
}
\arguments{
  \item{simplace}{handle to the SimplaceWrapper object
  returned by \code{\link{initSimplace}}}

  \item{parameterList}{a list with the parameter name as
  key and parametervalue as value}

  \item{queue}{if true put simulation to the prepared
  project to run them in parallel}
}
\value{
id of the created simulation
}
\description{
Creates a simulation from the opened project and
substitutes the values of the parameters given in the
parameter list. Simulation will be put into the queue by
default.
}
\seealso{
\code{\link{runSimulations}},
\code{\link{resetSimulationQueue}}
}

