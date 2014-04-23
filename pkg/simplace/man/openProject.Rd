\name{openProject}
\alias{openProject}
\title{Open Project}
\usage{
openProject(simplace, solution, project = nullString)
}
\arguments{
  \item{simplace}{handle to the SimplaceWrapper object
  returned by \code{\link{initSimplace}}}

  \item{solution}{solution file with absolute path}

  \item{project}{project file with absolute path, can be
  omitted to run solution only}
}
\description{
Initializes a simplace project (solution and projectfile)
}
\details{
Initializes a project. The absolute path to a solution file
is mandatory. Project file is optional.
}
\seealso{
\code{\link{closeProject}}
}

