\name{grasp.step.gam}
\alias{grasp.step.gam}
\alias{grasp.anova.glmlist}
\alias{grasp.anova.gam}
\alias{grasp.stat.anova}
\title{GRASP-R stepwise selection}
\description{Performs a stepwise selection using anova}
\usage{grasp.step.gam(object, scope, scale, direction, trace, keep, steps, limit, test)}
\arguments{
    \item{object}{starting gam object. Use \code{\link{grasp.model}} to create it.}
    \item{scope}{scope list. Use \code{\link{grasp.scope}} to create it.}
    \item{scale}{if test = "Chisq", then scale = 1. If test = "F", scale is calculated. Leave it blank.}
    \item{direction}{Direction of the stepwise selection. Either "both", "backward" or "forward".}
    \item{trace}{If true, the function is verbose and shows details of all the stepwise selection steps.}
    \item{keep}{Leave equal to NULL.}
    \item{steps}{Maximal number of steps to be done.}
    \item{limit}{p-limit for the selection}
    \item{test}{Test to be used for selection, either "Chisq" or "F".}
}
\details{
    This function is central to GRASP-R. It statistically selects significant predictors for each response variable. grasp.step.gam starts by default with the complete model you first created. You have to create a model first with grasp.model, where gr.selX will hold the selected predictors (see \code{\link{grasp.GUI}} for details on graphical selection of variables). The procedure goes in loop to try to eliminate one variable at a time. At each step, the lest significant variable is dropped from the model, and the loop starts again with the new model holding the remaining variables. When direction is "both", the procedure also reintroduce variables in the model. The tests (F or Chisq) are used to decide whether a variable should be kept, dropped or reintroduced. At the end of the procedure, the new model is stored in the variable gam.start. It overwrites the precedent model.
}
\Note{
    The grasp.anova.glmlist(), grasp.anova.gam() and grasp.stat.anova() function are dependent on the grasp.step.gam() function but aren't of any use by themselves!
}
\author{
    Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, J.R. Leathwich and J. McC Overton. Look at \url{http://www.fivaz.ch/grasper/index.html} for details and update.
}
\seealso{
    \code{\link{grasp.model}},\code{\link{grasp.pred}} and \code{\link{stat.anova}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
