"grasp.corlim" <-
function(gr.Yi = gr.selY, cols = gr.selX, thin = 1)
{
    x11()
    Yname <- names(YYY)[gr.Yi]
    cat("\n")
    cat("*********** GRASP COVARIATE SPACE PLOT ***********", "\n")
    cat(date(), "\n")
    cat("\n")
    cat("RESPONSE NAME: ", Yname, "\n")
    #data <- XXX
    data <- XXX[gr.modmask[, gr.Yi],  ]
    assign("data", data, pos=1)
    gr.selXCOR <- gr.selXCOR
    panel <- function(x, y)
    {
        ind <- YYY[,gr.Yi]==1
        points(x, y)
        points(x[ind], y[ind], col = 2)
    }
    ndata <- thin * nrow(data)
    index <- ((1:ndata) * 1)/thin
    thindata <- data[index,  ]
    pairs(thindata[, cols], panel = panel)
    text(0, 1.17, paste("GRASP: ", " ", Yname), adj = 0, cex = 0.7)
    cat("Red points are presences (1), black points absences(0)", "\n")
    cat("\n")
    cat(" ********** GRASP COAVRIATE SPACE PLOT END ********** ", "\n")
    cat("\n")
    cat("\n")
}
