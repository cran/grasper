\name{grasp.start}
\alias{grasp.start}
\title{Formula Creator}
\description{Starting formula creator, generally called by \code{\link{grasp.model}}}
\usage{grasp.start(cX, sX, df = 4, calcdf = FALSE, trace = TRUE)}
\arguments{
    \item{cX}{Selected predictors to be used in the form of a vector with column numbers}
    \item{sX}{List coontaining the different Xs for each Ys}
    \item{df}{Number of degrees of freedom}
    \item{calcdf}{Let the smooth function "calculate" the degrees of freedom, see \code{\link{gam}} for details.}
    \item{trace}{Verbose output}
}
\details{
    This function creates the starting formula. It isn't of any use by itself. It is called by \code{\link{grasp.model}}
}
\Note{Look at the \code{\link{grasp.model}} code to see how sX is related to cX}
\author{
    Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP for S-Plus written by A. Lehmann, 
    J.R. Leathwich and J. McC Overton. Look at \url{http://www.cscf.ch/grasp} for details and update.
}
\seealso{
    \code{\link{grasp.model}} and \code{\link{grasp.scope}}.
}
\keyword{models} \keyword{regression}
