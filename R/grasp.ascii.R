"grasp.ascii" <-
function(gr.Yi = gr.selY, var = gr.predmat, resolution = 1000)
{
    assign("gr.Yi", gr.Yi, pos = 1)
    if (length(gr.Yi) > 2) {
	stop("This function accepts only one varible in gr.Yi. Use lapply(gr.Yi, grasp.ascii) if you want to run it many times!")
    } else {
	Y <- as.numeric(gr.Yi)
    }
    Yname <- names(YYY)[gr.Yi]
    cat("\n")
    cat(" vvvvvvvvvv GRASP ASCII vvvvvvvvvv ", "\n")
    cat(date(), "\n")
    cat("RESPONSE NAME: ", Yname, "\n")
    maxX <- (ceiling(max(XXXpred$x)/resolution) * resolution) + resolution
    minX <- (floor(min(XXXpred$x)/resolution) * resolution)
    maxY <- (ceiling(max(XXXpred$y)/resolution) * resolution) + resolution
    minY <- (floor(min(XXXpred$y)/resolution) * resolution)
    half <- resolution/2
    cat("maxX: ", maxX, "\n")
    cat("minX: ", minX, "\n")
    cat("maxY: ", maxY, "\n")
    cat("minY: ", minY, "\n")
    Nrow <- (maxX - minX)/resolution
    Ncol <- (maxY - minY)/resolution
    cat("nrow =", Nrow, "and ncol =", Ncol, "\n")
    filename <- paste("pred_", names(YYY[gr.Yi]), ".asc", sep = "")
    fred1 <- round((XXXpred$x - (minX - half))/resolution)
    fred2 <- round(Ncol - ((XXXpred$y - (minY + half))/resolution))
    fred3 <- zapsmall(var[, Y], 4)
    map <- matrix(-99.9, nrow = Nrow, ncol = Ncol)
    temp <- as.data.frame(cbind(fred1, fred2, fred3))
    print(temp[1:10,  ])
    noduplicates <- all(!(duplicated(paste(temp$fred1, temp$fred2, sep = "x"))))
    if(noduplicates)
        cat("INFO: There are no duplicated X and Y coordinates in prediction set !", "\n")
    else if(dim(temp)[1] < 10000) {
        temp <- aggregate(temp, by = list(temp$fred1, temp$fred2), mean)
        cat("AGGREGATION: The mean of predicted value found for each combination of Xs and Ys is returned !!!", "\n")
    }
    else cat("AGGREGATION (>10000 predictions): Only the first predicted value found for each combination of Xs and Ys is returned !!!", "\n")
    fred1 <- temp$fred1
    fred2 <- temp$fred2
    fred3 <- temp$fred3
    for (r in 1:nrow(temp)) {
        map[fred1[r],fred2[r]] <- fred3[r]
    }
    write(paste("ncols", Nrow), file = filename)
    write(paste("nrows", Ncol), file = filename, append = TRUE)
    write(paste("xllcorner", minX), file = filename, append = TRUE)
    write(paste("yllcorner", minY), file = filename, append = TRUE)
    write(paste("cellsize", resolution), file = filename, append = TRUE)
    write("NODATA_value -99.9", file = filename, append = TRUE)
    write(map, file = filename, ncol = Ncol, append = TRUE)
    cat("prediction exported to: ", "\n")
    cat(filename, "\n")
    assign("map", map, pos=1)
    cat("\n")
    cat(" ********** GRASP ASCII END ********** ", "\n")
}
