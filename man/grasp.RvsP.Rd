\name{grasp.RvsP}
\alias{grasp.RvsP}
\title{GRASP-R plot of RV vs. PV}
\description{Plots the reponses (RV) versus the predictor variables (PV)}
\usage{grasp.RvsP(gr.Yi, sX)}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
    \item{sX}{Selected predictors to be used in the form of a vector with column numbers}
}
\details{
    This is a visual check. A smoothing function \code{\link{spline}} is applied to the relationships between the responses and each continuous predictor. The degrees of freedom for the smooth function are the same as for the model (df = 4). To change df, you have to edit the function directly.
}
\author{
	Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton.
}
\seealso{
	\code{\link{grasp.histo}}, \code{\link{grasp.datamap}}, \code{\link{grasp.cormat}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
