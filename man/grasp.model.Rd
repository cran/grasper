\name{grasp.model}
\alias{grasp.model}
\alias{grasp.validate}
\alias{grasp.roc}
\title{GRASP-R model creator}
\description{Creates a model from the given variables}
\usage{grasp.model(gr.Yi, sX, family, trace, df, calcdf}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
    \item{sX}{Selected predictors to be used in the form of a vector with column numbers}
    \item{family}{Family to be used for the model, see details}
    \item{trace}{Verbose}
    \item{df}{Degrees of freedom to be used for each variable}
    \item{calcdf}{If TRUE, the df are calculated by the gam function}
}
\details{
    This function creates the model from the given variables. The family is important. You can choose between \code{\link{gaussian}}, \code{\link{binomial}}, \code{\link{quasibinomial}} and \code{\link{quasipoisson}}. See \code{\link{family}} for details. To create the model, the function uses the \code{\link{gam}} function provided by the mgcv package. The \code{\link{grasp.GUI}} function provides method for selecting the appropriate family based on which type of variable you have.Grasp.model has to be used before the stepwise selection procedure (see \code{\link{grasp.step.gam}}.
}
\author{
	Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton.
}
\seealso{
	\code{\link{grasp.step.gam}}, \code{\link{gam}} and \code{\link{grasp.pred}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
