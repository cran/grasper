\name{grasp.in}
\alias{grasp.in}
\title{Initialize GraspeR variables}
\description{
Initializes the main objects that are used throughout the GraspeR session
}
\usage{grasp.in(Ymat, Xmat, Xpred)}
\arguments{
    \item{Ymat}{name of the data frame of variables of interest.}
    \item{Xmat}{name of the data frame of environmental drivers.}
    \item{Xpred}{name of the data frame of new environmental drivers to predict from}
}
\details{
    Grasp.in does several steps. It loads the required libraries (mgcv, modreg and MASS) and copies Ymat to YYY, Xmat to XXX and Xpred to XXXpred. 
    Then it creates a first model using random variables (note that it overwrites all previous models). It also creates the weights (WEIGHTS data frame). 
    After initialisation, you can change the weights by editing the WEIGHTS variable. It also intialises a few other objects usefull to GraspeR. 
    If you are not aiming at making predictions and therefore not using XXXpred, simply use grasp.in(Ymat, Xmat, Xmat). 
    The File / Import... menu from grasp.GUI automates importation of ascii files and automatically launches grasp.in after importation. 
    If you want to import files using the R prompt, look at the following functions : \code{\link{load}}, \code{\link{read.table}} and \code{\link{scan}}. 
    You can save R objects back to the disk using the command \code{\link{save}}
}
\author{
    Fabien Fivaz \email{fabien.fivaz@bluewin.ch}. Ported to R from GRASP \url{http://www.cscf.ch/grasp/} for S-Plus written by A. Lehmann, 
    J.R. Leathwich and J. McC Overton. Look at \url{http://www.fivaz.ch/grasper/index.html} for details and update.
}
\seealso{
    \code{\link{grasp.GUI}}
}
\keyword{models} \keyword{smooth} \keyword{regression}
