"grasp.select.responses" <-
function () 
{
   
respsel.close <- function(...)
{
    tkdestroy(respsel)
}

apply.response <- function()
{
    resp.sel <- tkcurselection(frame4.lstbox2)
    resp.sel <- tclvalue(resp.sel)
    write(resp.sel, file = "resp")
    resp.sel <- scan(file = "resp")
    gr.selY <- resp.sel + 1
    assign("gr.selY", gr.selY, pos = 1)
    assign("gr.Yi", gr.selY, pos = 1)
    assign("OPT$SELY", gr.selY, pos = 1)
    for (i in gr.selY) {
        Yname <- names(YYY[i])
        print(Yname)
    }
}

    respsel <- tktoplevel()
    tktitle(respsel) <- "R-GRASP Select"
    
    frame1 <- tkframe(respsel, relief = "groove", borderwidth = 2)
    frame4 <- tkframe(respsel, relief = "groove", borderwidth = 2)
    frame3 <- tkframe(respsel, relief = "groove", borderwidth = 2)

    frame1.label <- tklabel(frame1, text = "Select response variables", justify = "left", wraplength = 200)
    tkpack(frame1, frame1.label, fill = "x")

    frame4.lstbox2 <- tklistbox(frame4, selectmode = "multiple", exportselection=FALSE)
    for (i in c(1:length(YYY))) {
        tkinsert(frame4.lstbox2, 'end', names(YYY)[i])
    }
    tkpack(frame4, frame4.lstbox2, fill = "x")

    apply.but <- tkbutton(frame3, text = "Apply", command = apply.response)
    close.but <- tkbutton(frame3, text = "Close", command = respsel.close)
    tkgrid(apply.but, close.but, sticky = "w")
    tkpack(frame3, fill = "x")
}
