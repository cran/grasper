\name{grasp.datamap}
\alias{grasp.datamap}
\title{GraspeR spatial distribution plot}
\description{Plots the spatial distribution of the input response data}
\usage{grasp.datamap(gr.Yi)}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
}
\details{
    This function is a visual check of the spatial distribution of the input response data in case of spatial use of GraspeR. The geographic coordinates of points must be contained in the XXX data frame, columns 2 and 3 with x and y column name respectively.
}
\author{
    Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, 
    J.R. Leathwich and J. McC Overton. Look at \url{http://www.fivaz.ch/grasper/index.html} for details and update.
}
\seealso{
    \code{\link{grasp.histo}}, \code{\link{grasp.RvsP}}, \code{\link{grasp.cormat}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
