\name{grasp.cormat}
\alias{grasp.cormat}
\title{GRASP-R correlations map and matrix}
\description{Calculates correlation between PVs and plots a matrix of correlation}
\usage{grasp.cormat(gr.Yi, cols, thin = 1)}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
    \item{cols}{Selected predictors to be used in the form of a vector with column numbers}
    \item{thin}{Leave 1}
}
\details{
    This function calculates correlation between PVs and plots a matrix of correlation. It is a visual check. 
    Too highly correlated PVs have to be eliminated manually, after checking.
}
\author{
    Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, 
    J.R. Leathwich and J. McC Overton. Look at \url{http://www.cscf.ch/grasp} for details and update.
}
\seealso{
    \code{\link{grasp.histo}}, \code{\link{grasp.datamap}}, \code{\link{grasp.RvsP}} and \code{\link{grasp.corlim}}
}
\keyword{models} \keyword{smooth} \keyword{regression} \keyword{plot}
