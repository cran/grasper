"grasp.pred.plot" <-
function(predmat = gr.predmat, gr.Yi = gr.selY, resolution = 1000)
{
    cat("\n")
    cat("*****************************************************", "\n")
    cat("**********       GRASP PREDICTION PLOT     **********", "\n")
    cat("**********         Plots predictions       **********", "\n")
    cat("**********        GRASP by A. Lehmann      **********", "\n")
    cat("**********      Ported to R by F. Fivaz    **********", "\n")
    cat("*****************************************************", "\n")
    cat("\n")
    cat(date(), "\n")
    cat("\n")
    Yname <- names(YYY)[gr.Yi]
    cat("Response name: ", Yname, "\n")
    TITLE <- "GraspeR"
    maxX <- (ceiling(max(XXXpred$x)/resolution) * resolution) + resolution
    minX <- (floor(min(XXXpred$x)/resolution) * resolution)
    maxY <- (ceiling(max(XXXpred$y)/resolution) * resolution) + resolution
    minY <- (floor(min(XXXpred$y)/resolution) * resolution)
    half <- resolution/2
    Nrow <- (maxX - minX)/resolution
    Ncol <- (maxY - minY)/resolution
    if((Nrow * Ncol) > 10000000) {
        cat("ERROR: too many pixels (", Nrow * Ncol, ")>> increase your resolution in the options", "\n")
        return(invisible())
    }
    cat("\n")
    cat("maxX: ", maxX, "\n")
    cat("minX: ", minX, "\n")
    cat("maxY: ", maxY, "\n")
    cat("minY: ", minY, "\n")
    cat("Nrow: ", Nrow, "\n")
    cat("Ncol: ", Ncol, "\n")
    rangeX <- maxX - minX
    rangeY <- maxY - minY
    par(mfrow = c(1,1), mai = c(0.5, 0.5, 0.5, 0.5))
    fred0 <- (predmat[, gr.Yi] != -99.9)
    fred1 <- XXXpred$x
    fred2 <- XXXpred$y
    fred1 <- round((fred1[fred0] - (minX - half))/resolution)
    fred2 <- round((fred2[fred0] - (minY + half))/resolution)
    fred3 <- predmat[fred0, gr.Yi]
    temp <- as.data.frame(cbind(fred1, fred2, fred3))
    cat("\n")
    cat("Temp:", "\n")
    print(temp[1:10,  ])
    temp.temp <- temp
    noduplicates <- all(!(duplicated(paste(temp$fred1, temp$fred2, sep = "x"))))
    cat("\n")
    if(noduplicates)
        cat("There are no duplicated X and Y coordinates in prediction set !", "\n")
    else if(dim(temp)[1] < 10000) {
        temp <- aggregate(temp, by = list(temp$fred1, temp$fred2), mean)
        cat("AGGREGATION: The mean of predicted value found for each combination of Xs and Ys is returned !!!", "\n")
    }
    else cat("AGGREGATION (>10000 predictions): Only the first predicted value found for each combination of Xs and Ys is returned !!!", "\n")
    fred1 <- temp$fred1
    fred2 <- temp$fred2
    fred3 <- temp$fred3
    map <- NULL
    map <- matrix(NA, nrow = Nrow, ncol = Ncol)
    for (r in 1:length(temp[,1])) {
        map[fred1[r],fred2[r]] <- fred3[r]
    }
    plot(fred1, fred2, pch = " ", ylim = c(0, Ncol + 0.1 * Ncol), xlim = c(0, Nrow + 0.1 * Nrow))
    image(map, col = heat.colors(12))
    title(paste(TITLE, " ", "\n Grid resolution : ", 1000), cex = 0.6)
    cat("\n")
    cat("**********     GRASP PREDICTION PLOT END   **********", "\n")
    cat("\n")
}
