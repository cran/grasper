\name{grasp.GUI}
\alias{grasp.GUI}
\alias{grasp.README.GUI}
\alias{grasp.options.GUI}
\alias{grasp.about.GUI}
\alias{grasp.gpl.GUI}
\alias{grasp.import.GUI}
\alias{grasp.model.GUI}
\alias{grasp.select.responses}
\alias{grasp.select.predictors}
\alias{grasp.TODO.GUI}
\alias{grasp.import}
\alias{grasp.in.GUI}
\alias{grasp.GRASS}
\alias{grasp.dump}
\title{GRASP-R Graphical User Interface}
\description{Launches the Graphical User Interface of GraspeR}
\usage{grasp.GUI(grass.on=FALSE)}
\arguments{
	\item{grass.on}{If TRUE, functions for GRASS will be activated}
}
\details{
    Grasp.GUI can be used instead of \code{\link{grasp.in}}. It initiliazes all the libraries, including the tcltk library which makes the GUI work. It has menues for about all the functions of GRASP-R, plus links to Interfaces to different functions.
}
\note{
    The help menu is not yet totally implemented. Look at the documentation for more details at \code{\url{http://www.cscf.ch/grasp}}. If you want to use the GRASS commands, install the GRASS package from CRAN, and launch grass5 BEFORE launching R FROM THE grass5 COMMAND LINE INTERFACE.
}
\author{
Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton. Have a look at \url{http://www.cscf.ch/grasp} for details and update.
}
\seealso{
    \code{\link{grasp.in}}
}
\keyword{models} \keyword{regression}
