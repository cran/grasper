\name{grasp.pred}
\alias{grasp.pred}
\alias{grasp.pred.plot}
\title{GRASP-R Predict}
\description{Predicts each RVs using a new set of PVs}
\usage{grasp.pred(gr.Yi)}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
}
\details{
    This function is the most time and ressource consuming, along with the stepwise selection. 
    It predicts each response variables (RVs) using a new set of predictor variables (PVs) contained in XXXpred. 
    The XXXpred variable is initialized by \code{\link{grasp.in}}. The \code{\link{predict.gam}} function from the mgcv package is used.
    The predictions are saved in the variable gr.predmat. The grasp.pred.plot() function plots the results.
}
\Note{
    The function won't work if a model has not been calculated with \code{\link{grasp.model}} before running grasp.pred()
}
\author{
    Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton. Look at \url{http://www.cscf.ch/grasp} for details and update.
}
\seealso{
    \code{\link{grasp.step.gam}}, \code{\link{grasp.model}} and \code{\link{predict.gam}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
