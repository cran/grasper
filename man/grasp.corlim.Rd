\name{grasp.corlim}
\alias{grasp.corlim}
\title{GRASP-R spatial correlation plot}
\description{Calculates correlations for two variables with differentiation of presences and absences}
\usage{grasp.corlim(gr.Yi, cols, thin = 1)}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
    \item{cols}{Selected predictors to be used in the form of a vector with column numbers}
    \item{thin}{Leave 1}
}
\details{
    This function plots correlation between variables, somehow like the \code{\link{grasp.cormat}} function, but with the difference that presences and absences are differentiated. By looking at this plot before and after applying limits to your dataset, you can see the different distributions of presences and absences.
}
\author{
    Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, 
    J.R. Leathwich and J. McC Overton. Look at \url{http://www.cscf.ch/grasp} for details and updates.
}
\seealso{
    \code{\link{grasp.histo}}, \code{\link{grasp.datamap}}, \code{\link{grasp.RvsP}} and \code{\link{grasp.cormat}}
}
\keyword{models} \keyword{smooth} \keyword{regression} \keyword{plot}
