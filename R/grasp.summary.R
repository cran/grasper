"grasp.summary" <-
function(gr.Yi)
{
    cat("********* GraspeR.summary **********", "\n")
    cat(date(), "\n")
    cat("\n")
    cat("Ys...", "\n")
    dimYYY <- dim(YYY)
    cat("Dimension of YYY:", dimYYY, "\n")
    Yname <- names(YYY)[gr.Yi]
    cat("Selected response: ", gr.Yi, "\n")
    cat("Response name: ", Yname, "\n", "\n")
    selection <- gr.modmask[, gr.Yi]
    if (length(gr.Yi) > 1) {
	cat("More than one VOI to summarize, doing lapply()", "\n")
        lapply(gr.Yi, grasp.summary)
    } else {
    	print(summary(YYY[selection, gr.Yi]))
	cat("\n")
	cat("XXX:", "\n")
	print(summary(XXX[selection, gr.selX]))
    }
    cat("\n")       
    cat("************************************", "\n")
}
