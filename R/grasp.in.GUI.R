"grasp.in.GUI" <-
function () 
{

graspin.close <- function(...)
{
    tkdestroy(graspin)
}

grasp.init <- function(...)
{
    YYYvar.local <- as.character(tclvalue(YYYvar))
    YYYparse <- eval(parse(text = YYYvar.local))
    XXXvar.local <- as.character(tclvalue(XXXvar))
    XXXparse <- eval(parse(text = XXXvar.local))
    XXXpredvar.local <- as.character(tclvalue(XXXpredvar))
    XXXpredparse <- eval(parse(text = XXXpredvar.local))
    cat("YYY = ", YYYvar.local, "\n")
    cat("XXX = ", XXXvar.local, "\n")
    cat("XXXpred = ", XXXpredvar.local, "\n")
    grasp.in(YYYparse, XXXparse, XXXpredparse)
    cat("initializing done", "\n")
}

graspin <- tktoplevel()
tktitle(graspin) <- "GraspeR in"

YYYvar <- tclVar("YYY")
XXXvar <- tclVar("XXX")
XXXpredvar <- tclVar("XXXpred")

# GUI for data initializing

frame1 <- tkframe(graspin, relief = "groove", borderwidth = 2)
frame2 <- tkframe(graspin, relief = "groove", borderwidth = 2)
frame3 <- tkframe(graspin, relief = "groove", borderwidth = 2)

# Frame 1
frame1.label1 <- tklabel(frame1, text = "GraspeR.in - Data initializing")
frame1.label2 = tklabel(frame1, text = "Warning: Apply will reinitialize all your precedent choices...!", wraplength = 200)
tkpack(frame1, frame1.label1, frame1.label2, fill = "x")

# Frame2
frame2.label1 <- tklabel(frame2, text = "YYY:")
frame2.label2 <- tklabel(frame2, text = "XXX:")
frame2.label3 <- tklabel(frame2, text = "XXXpred:")

frame2.entry1 <- tkentry(frame2, textvariable = YYYvar, width = 10)
frame2.entry2 <- tkentry(frame2, textvariable = XXXvar, width = 10)
frame2.entry3 <- tkentry(frame2, textvariable = XXXpredvar, width = 10)

tkgrid(frame2.label1, frame2.entry1, sticky = "w")
tkgrid(frame2.label2, frame2.entry2, sticky = "w")
tkgrid(frame2.label3, frame2.entry3, sticky = "w")

tkpack(frame2, fill = "x")

# Frame 3
frame3.but1 <- tkbutton(frame3, text = "Apply", padx = 20, command = grasp.init)
frame3.but2 <- tkbutton(frame3, text = "Close", padx = 20, command = graspin.close)

tkgrid(frame3.but1, frame3.but2, sticky = "w")

tkpack(frame3)
}
