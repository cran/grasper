\name{grasp.lut}
\alias{grasp.lut}
\title{GRASP-R lookup tables}
\description{Create lookup tables (LUT)}
\usage{grasp.lut(gr.Yi, grasp.model=grasp.start, path="pred1.lutSp1"}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
    \item{grasp.model}{Name of the model, created by grasp.model, to use}
    \item{path}{Path for the two files that will be created}
}
\details{
    This function creates two files containing the lookup tables, one with prediction, the other with standard deviation errors. These exportation format is interesting when predictions must be done on huge datasets, i.e. when XXXpred is too big to be directly predicted from R (usually files that have more than 100'000 lines). Use the ArcView script available on our website to predict inside ArcView. Look at the help attached with this script for details.
}
\author{
	Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton.
}
\seealso{
	\code{\link{grasp.export}} and \code{\link{grasp.ascii}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
