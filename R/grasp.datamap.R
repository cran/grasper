"grasp.datamap" <-
function (gr.Yi) 
{
    cat("\n")
    cat("*****************************************************", 
        "\n")
    cat("**********       GRASP-R grasp.datamap()   **********", 
        "\n")
    cat("********** Graph of datamap with pres/abs  **********", 
        "\n")
    cat("**********        GRASP by A. Lehmann      **********", 
        "\n")
    cat("**********      Ported to R by F. Fivaz    **********", 
        "\n")
    cat("*****************************************************", 
        "\n")
    cat("\n")
    cat(date(), "\n")
    cat("\n")
    x11()
    Yname <- names(YYY)[gr.Yi]
    cat("RESPONSE NAME: ", Yname, "\n")
    longitude <- XXX$x
    latitude <- XXX$y
    rangex <- max(longitude) - min(longitude)
    rangey <- max(latitude) - min(latitude)
    XMIN <- min(longitude) - 0.1 * rangex
    XMAX <- max(longitude) + 0.1 * rangex
    YMIN <- min(latitude) - 0.1 * rangey
    YMAX <- max(latitude) + 0.1 * rangey
    par(mfrow = c(1, 1), cex = 0.8, mai = c(0.75, 0.75, 0.75, 
        0.75))
    plot(longitude[YYY[, gr.Yi] > 0], latitude[YYY[, gr.Yi] > 
        0], pch = 1, cex = 1, xlim = c(XMIN, XMAX), ylim = c(YMIN, 
        YMAX), bty = "n", col = 2)
    points(longitude[YYY[, gr.Yi] == 0][gr.modmask[, gr.Yi] == 
        TRUE], latitude[YYY[, gr.Yi] == 0][gr.modmask[, gr.Yi] == 
        TRUE], pch = 1, col = 1, cex = 0.6)
    nbp <- as.character(length((YYY[(YYY[, gr.Yi] > 0), gr.Yi])))
    nba <- as.character(length((YYY[(YYY[, gr.Yi] == 0), gr.Yi])))
    legend(x = c(0, 0), y = c(XMAX, YMAX), legend = c(paste("Present: ", 
        nbp), paste("Absent: ", nba)), pch = c("+ . "), bty = "n", 
        cex = 0.6)
    title(main = paste(OPT$TITLE, " Datamap"), cex = 0.5)
    cat("\n")
    cat("**********        R-GRASP DATAMAP END      **********", 
        "\n")
    cat("\n")
}
