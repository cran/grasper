"grasp.limits.GUI" <-
function () 
{
    limits.close <- function(...) {
        tkdestroy(limits)
    }
    apply.limits <- function() {
        nlim <- tkcurselection(frame2.lstbox1)
        nlim <- tclvalue(nlim)
        write(nlim, file = "lim")
        nlim <- scan(file = "lim")
        sX.loc <- nlim + 1
        npast.loc <- as.numeric(tclvalue(npast))
        grasp.limits(gr.selY, sX = sX.loc, npast = npast.loc)
        assign("gr.selim", sX.loc, pos = 1)
        cat("Limits applied!", "\n")
    }
    limits <- tktoplevel()
    tktitle(limits) <- "GraspeR: Set limits"
    npast <- tclVar("10")
    frame1 <- tkframe(limits, relief = "groove", borderwidth = 2)
    frame2 <- tkframe(limits, relief = "groove", borderwidth = 2)
    frame3 <- tkframe(limits, relief = "groove", borderwidth = 2)
    frame4 <- tkframe(limits, relief = "groove", borderwidth = 2)
    frame1.label1 <- tklabel(frame1, text = "Set limits", justify = "left", 
        wraplength = 200, font = "Arial 11")
    tkgrid(frame1.label1, columnspan = 2)
    frame1.label2 <- tklabel(frame1, text = "This function restricts the data within limits defined for the below selected response variables (min. 2) by keeping the below given number of zeroes observations on each side of the last presence along selected predictors.", 
        font = "Arial 9", wraplength = 200)
    tkgrid(frame1.label2, columnspan = 2)
    tkpack(frame1, fill = "x")
    frame4.label3 <- tklabel(frame4, text = "npast", font = "Arial 9")
    frame4.entry1 <- tkentry(frame4, textvariable = npast, font = "Arial 9")
    tkgrid(frame4.label3, frame4.entry1)
    tkpack(frame4, fill = "x")
    frame2.yscroll <- tkscrollbar(frame2)
    frame2.lstbox1 <- tklistbox(frame2, selectmode = "multiple", 
        exportselection = FALSE)
    tkconfigure(frame2.lstbox1, yscrollcommand = paste(.Tk.ID(frame2.yscroll), 
        "set"))
    tkconfigure(frame2.yscroll, command = paste(.Tk.ID(frame2.lstbox1), 
        "yview"))
    for (i in c(1:length(XXX))) {
        tkinsert(frame2.lstbox1, "end", names(XXX)[i])
    }
    gr.selim0 <- gr.selim - 1
    for (i in gr.selim0) tkselection.set(frame2.lstbox1, i)
    tkgrid(frame2.lstbox1, frame2.yscroll)
    tkpack(frame2, fill = "y")
    apply.but <- tkbutton(frame3, text = "Apply", command = apply.limits)
    close.but <- tkbutton(frame3, text = "Close", command = limits.close)
    tkgrid(apply.but, close.but, sticky = "w")
    tkpack(frame3, fill = "x")
}
