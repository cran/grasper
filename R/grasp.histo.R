"grasp.histo" <-
function (gr.Yi, sX = gr.selX, nbar = 10) 
{
    cat("\n")
    cat("*****************************************************", "\n")
    cat("**********           grasp.histo()         **********", "\n")
    cat("**********         GRASP-R Histograms      **********", "\n")
    cat("**********     GRASP by A. Lehmann et al.  **********", "\n")
    cat("**********      Ported to R by F. Fivaz    **********", "\n")
    cat("*****************************************************", "\n")
    cat("\n")
    cat(date(), "\n")
    cat("\n")
    cat("Initializing variables... ")
    x11()
    Yname <- names(YYY)[gr.Yi]
    y <- YYY[gr.modmask[, gr.Yi], gr.Yi]
    XX <- XXX[gr.modmask[, gr.Yi], ]
    first <- TRUE
    cat("done", "\n")
    cat("Response name: ", Yname, "\n")
    cat("Opening graph window... ")
    numgr <- length(gr.selX) - 3
    par(mfrow = c(2, 4), cex = 0.65)
    prop <- length(y[y > 0])/length(y)
    cat("done", "\n")
    cat("Drawing histograms...")
    for (i in sX) {
        if (is.numeric(XX[, i])) {
            Xmin <- round(min(XX[!is.na(XX[, i]), i]), 2)
            Xlag <- (max(XX[!is.na(XX[, i]), i]) - Xmin)/nbar
            Xbreaks <- as.character(Xmin)
            for (nb in 1:(nbar - 1)) {
                Xnew <- round(Xmin + (nb * Xlag), 2)
                Xbreaks <- c(Xbreaks, as.character(Xnew))
            }
            temp <- table(factor(cut(y[!is.na(XX[, i])], c(-1, 1e-05, 500))), cut(XX[!is.na(XX[, i]), i], nbar))
            loc <- barplot(temp, xlab = names(XX[i]), ylab = "count", names = Xbreaks)
            nbp <- as.character(length(y[y > 0]))
            if (prop < 1) {
                par(new = T)
                temp[1, temp[1, ] == 0] <- 1e-08
                fred2 <- (temp[2, ]/(temp[1, ] + temp[2, ])) / prop
                barplot(fred2, axes = FALSE, axisnames = FALSE, density = NULL, space = 0, lwd = 0.5, col = NULL)
                abline(h = 1, lty = 2)
            }
            if (first) {
                title(paste(OPT$TITLE," histograms"))
                first <- FALSE
            }
        }
    }
    cat("done", "\n")
    cat("\n")
    cat("**********        grasp.histo() end        **********", 
        "\n")
    cat("\n")
}
