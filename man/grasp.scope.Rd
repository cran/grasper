\name{grasp.scope}
\alias{grasp.scope}
\title{Scope list Creator}
\description{Scope list creator, generally called by \code{\link{grasp.model}} and to be used with \code{\link{grasp.step.gam}} in stepwise selection.}
\usage{grasp.scope(sX = gr.selX, df = 4, calcdf = FALSE, trace = TRUE)}
\arguments{
    \item{sX}{List coontaining the different Xs for each Ys}
    \item{df}{Number of degrees of freedom}
    \item{calcdf}{Let the smooth function "calculate" the degrees of freedom, see \code{\link{gam}} for details.}
    \item{trace}{Verbose output}
}
\details{
    This function creates the scope list. It isn't of any use by itself. It is called by \code{\link{grasp.model}} and used by 
    \code{\link{grasp.step.gam}} for stepwise selection.
}
\author{
    Fabien Fivaz \code{\email{fabien.fivaz@bluewin.ch}}. Ported to R from GRASP for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton. Look at \url{http://www.cscf.ch/grasp} for details and update.
}
\seealso{
    \code{\link{grasp.model}}, \code{\link{grasp.start}} and \code{\link{grasp.step.gam}}.
}
\keyword{models} \keyword{regression}
