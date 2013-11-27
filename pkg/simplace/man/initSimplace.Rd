\name{initSimplace}
\alias{initSimplace}
\title{Initialisation of Framework}
\usage{
  initSimplace(InstallationDir, WorkDir, OutputDir)
}
\arguments{
  \item{InstallationDir}{directory where simplace, lap and
  simplacerun are located}

  \item{WorkDir}{working directory where solutions,
  projects and data resides (_WORKDIR_)}

  \item{OutputDir}{directory for output (_OUTPUTDIR_)}
}
\value{
  handle to the SimplaceWrapper object
}
\description{
  Initializes the JVM and creates the SimplaceWrapper
  object which is used to interact with Simplace.
}

