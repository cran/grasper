\name{grasp.histo}
\alias{grasp.histo}
\title{GraspeR Histograms of VOI on IED}
\description{Plots the histograms of the of the VOI on the IED}
\usage{grasp.histo(gr.Yi, sX, nbar)}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
    \item{sX}{Selected predictors to be used in the form of a vector with column numbers}
    \item{nbar}{Number of bars to be displayed in the hîstogramms}
}
\details{
    This function is for visual check of both response (RV) and predictor variables (PV). First, a simple histogram of the selected explanatory variables is produced. On top of each bar, the number of response observations observed is written, and also drawn in each bar. The dashed line represents the overall proportion of response observations greater than zero (presences) in comparison to the total number of observations. Finally, a black continuous line is added showing whether the proportion of presences in each bar is higher or lower than the overall proportion.
}
\author{
Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton.
}
\seealso{
	\code{\link{grasp.datamap}}, \code{\link{grasp.RvsP}}, \code{\link{grasp.cormat}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
