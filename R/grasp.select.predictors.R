"grasp.select.predictors" <-
function () 
{
    predsel.close <- function(...) {
        tkdestroy(predsel)
    }
    apply.predictors <- function() {
        pred.sel <- tkcurselection(frame2.lstbox1)
        pred.sel <- tclvalue(pred.sel)
        write(pred.sel, file = "resp")
        pred.sel <- scan(file = "resp")
        gr.selX <- pred.sel + 4
        assign("gr.selX", gr.selX, pos = 1)
        assign("gr.Xi", gr.selX, pos = 1)
        assign("SelX", gr.selX, pos = 1)
        for (i in gr.selX) {
            Yname <- names(XXX[i])
            print(Yname)
        }
    }
    predsel <- tktoplevel()
    tktitle(predsel) <- "R-GRASP Select"
    frame1 <- tkframe(predsel, relief = "groove", borderwidth = 2)
    frame2 <- tkframe(predsel, relief = "groove", borderwidth = 2)
    frame3 <- tkframe(predsel, relief = "groove", borderwidth = 2)
    frame1.label <- tklabel(frame1, text = "Select predictors", 
        justify = "left", wraplength = 200)
    tkpack(frame1, frame1.label, fill = "x")
    frame2.yscroll <- tkscrollbar(frame2, repeatinterval=5, command=function(...)tkyview(frame2.lstbox1,...))
    frame2.lstbox1 <- tklistbox(frame2, selectmode = "multiple", 
        exportselection = FALSE,yscrollcommand=function(...)tkset(frame2.yscroll,...))
    for (i in c(4:length(XXX))) {
        tkinsert(frame2.lstbox1, "end", names(XXX)[i])
    }
    gr.selX0 <- gr.selX - 4
    for (i in gr.selX0) tkselection.set(frame2.lstbox1, i)
    tkgrid(frame2.lstbox1, frame2.yscroll)
    tkgrid.configure(frame2.yscroll, rowspan=4, sticky="nsw")
    tkpack(frame2, fill = "y")
    apply.but <- tkbutton(frame3, text = "Apply", command = apply.predictors)
    close.but <- tkbutton(frame3, text = "Close", command = predsel.close)
    tkgrid(apply.but, close.but, sticky = "w")
    tkpack(frame3, fill = "x")
}
