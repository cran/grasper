"grasp.GRASS" <-
function (layername) 
{
    Yname <- names(YYY)[gr.Yi]
    cat("\n")
    cat("********** GraspeR export to GRASS **********", "\n")
    cat("As I had problems with the function rast.put,", "\n")
    cat("I decided to export data first to a file,", "\n")
    cat("then reimport it in GRASS...", "\n")
    cat("*********************************************", "\n")
    cat(date(), "\n")
    cat("RESPONSE NAME: ", Yname, "\n")
    cat("Layer name: ", layername, "\n")
    cat("\n")
    grasp.ascii()
    a = paste("r.in.arc input=", filename, " output=", layername, sep = "")
    system(a)
    b = paste("d.rast ", layername, sep = "")
    system(b)
    cat("*********************************************", "\n")
}
