\name{grasp.limits}
\alias{grasp.limits}
\alias{grasp.limits.GUI}
\title{GraspeR limits}
\description{GraspeR - Restricting the data within limits}
\usage{grasp.limits(gr.Yi, sX, npast, lim)}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
    \item{sX}{Selected predictors to limit from}
    \item{npast}{Number of 0 observations kept on each side of the last presence}
    \item{lim}{Limits type generaly "and" }
}
\details{
    The grasp.limits function is optional. It restricts the data within limits defined for selected response variables by keeping a given number of 0 
    observations on each side of the last presence along selected predictors.
}
\note{
    You have to call grasp.limits before calling other functions in case you want to apply limits to all the other computations. 
    The function can be accessed through \code{\link{grasp.GUI}}() in menu Edit / Set limits...
}
\author{
Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton.
}
\seealso{
    \code{\link{grasp.GUI}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
