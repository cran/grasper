"grasp.GRASS" <-
function (layername, Y = gr.selY, predmat = gr.predmat) 
{
    assign("gr.Yi", Y, pos = 1)
    Yname <- names(YYY)[gr.Yi]
    cat("\n")
    cat(" vvvvvvvvvv GraspeR export to GRASS vvvvvvvvvv ", "\n")
    cat(date(), "\n")
    cat("RESPONSE NAME: ", Yname, "\n")
    cat("Layer name: ", layername, "\n")
    require(GRASS)
    G <- gmeta()
    summary(G)
    resolution <- 1000
    maxX <- (ceiling(max(XXXpred$x)/resolution) * resolution) + 
        resolution
    minX <- (floor(min(XXXpred$x)/resolution) * resolution)
    maxY <- (ceiling(max(XXXpred$y)/resolution) * resolution) + 
        resolution
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
    fred3 <- zapsmall(predmat[, Y])
    cat("nrow =", Nrow, "and ncol =", Ncol, "\n")
    mapGRASS <- matrix(NA, nrow = Nrow, ncol = Ncol)
    temp <- as.data.frame(cbind(fred1, fred2, fred3))
    print(temp[1:10, ])
    noduplicates <- all(!(duplicated(paste(temp$fred1, temp$fred2, 
        sep = "x"))))
    if (noduplicates) 
        cat("INFO: There are no duplicated X and Y coordinates in prediction set !", 
            "\n")
    else if (dim(temp)[1] < 10000) {
        temp <<- aggregate(temp, by = list(temp$fred1, temp$fred2), 
            mean)
        cat("AGGREGATION: The mean of predicted value found for each combination of Xs and Ys is returned !!!", 
            "\n")
    }
    else cat("AGGREGATION (>10000 predictions): Only the first predicted value found for each combination of Xs and Ys is returned !!!", 
        "\n")
    cat("Creating freds...")
    fred1 <- temp$fred1
    fred2 <- temp$fred2
    fred3 <- temp$fred3
    cat("done\n")
    cat("Doing temp...")
    assign("temp", temp, pos = 1)
    for (r in 1:nrow(temp)) {
        mapGRASS[fred1[r], fred2[r]] <- fred3[r]
    }
    cat("done\n")
    cat("Converting mapGRASS...")
    mapGRASS <- as.vector(mapGRASS)
    mapGRASS <- mapGRASS * 100
    mapGRASS <<- as.integer(mapGRASS)
    assign("mapGRASS", mapGRASS, pos = 1)
    cat("done\n")
    cat("rast.put(G, lname=layername, mapGRASS)")
    cat("\n")
    cat(" ********** GRASP ASCII END ********** ", "\n")
}
