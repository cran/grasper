\name{grasp.summary}
\alias{grasp.summary}
\title{Summary of the Xs and Ys data}
\description{Export data as point file redable with for instance Arcview}
\usage{grasp.summary(gr.Yi = gr.selY)}
\arguments{
    \item{gr.Yi}{Selected responses to be used in the form of a vector with column numbers}
}
\details{
    This function summarizes the data using the \code{\link{summary}}
}
\Note{
    If you defined limits with the \code{\link{grasp.limits}}() function, the summary takes this limits into account.
    You can access the function through \code{\link{grasp.GUI}}(), menu Analysis / Summary.
}
\author{
    Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, 
    J.R. Leathwich and J. McC Overton. Look at \url{http://www.fivaz.ch/grasper/index.html} for details and update.
}
\seealso{
    \code{\link{grasp.limits}} and \code{\link{summary}}.
}
\keyword{models} \keyword{regression}
