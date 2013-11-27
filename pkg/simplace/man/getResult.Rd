\name{getResult}
\alias{getResult}
\title{Fetch output from a simulation}
\usage{
  getResult(simplace, outputId, simulationId)
}
\arguments{
  \item{simplace}{handle to the SimplaceWrapper object
  returned by \code{\link{initSimplace}}}

  \item{outputId}{id of the output}

  \item{simulationId}{id of the simulation}
}
\value{
  handle to the data container which has to be processed
  afterwards
}
\description{
  The output is a JavaObject containing the variable names,
  data types, units and the values. Output can be converted
  with \code{\link{resultToList}} or
  \code{\link{resultToDataframe}} to R objects.
}

