"grasp.options.GUI" <-
function (...) 
{
# Defining several functions

options.apply <- function(...)
{
# Function creating and assigning the OPTIONS$... variables
apply.ok <- TRUE
assign("apply.ok", apply.ok, pos = 1)
OPT$TITLE <- as.character(tclvalue(OPTIONS.TITLE))
OPT$LAYOUT <- eval(parse(text = as.character(tclvalue(OPTIONS.LAYOUT))))
OPT$NBBARS <- as.integer(tclvalue(OPTIONS.NBBARS))
OPT$WEIGHTS <- as.character(tclvalue(OPTIONS.WEIGHTS))
OPT$RESOLUTION <- as.numeric(tclvalue(OPTIONS.RESOLUTION))
print(OPT)
assign("OPT", OPT, pos = 1)
cat("Options set!", "\n")
}

options.default <- function(...)
{
# Function reseting options
OPT.DEFAULT <- NULL
OPT <- OPT.DEFAULT
assign("OPT", OPT, pos = 1)
cat("Options set to default", "\n")
}

options.close <- function()
{
#Close Options Window
    tkdestroy(OPTIONS)
}

# Opening new GUI window and setting title
OPTIONS <- tktoplevel()
tktitle(OPTIONS) <- "R-GRASP options"

# Defining variables
apply.ok <- FALSE
assign("apply.ok", apply.ok, pos = 1)

# Setting default for options
OPTIONS.TITLE <- tclVar("R-GRASP: ")
OPTIONS.LAYOUT <- tclVar("c(3,3)")
OPTIONS.NBBARS <- tclVar(10)
OPTIONS.WEIGHTS <- tclVar("WEIGHTS")
OPTIONS.RESOLUTION <- tclVar(1000)
OPTIONS.SELX <- tclVar("c(4,5,6,7,8,9,10,11)")

# Defining frames
frame1 <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)
frame2 <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)
frame3 <- tkframe(OPTIONS, relief = "groove", borderwidth = 2)

# Defining objects in frame1
frame1.label1 <- tklabel(frame1, text = "GraspeR Options", font = "Arial 11")
frame1.label2 <- tklabel(frame1, text = "This window lets you define the options", font = "arial 10")
tkpack(frame1, frame1.label1, frame1.label2, fill = "x")

# Defining objects in frame2
frame2.main <- tklabel(frame2, text = "General options", font = "arial 10", justify = "center")

frame2.label1 <- tklabel(frame2, text = "Title", font = "arial 10")
frame2.label2 <- tklabel(frame2, text = "Layout", font = "arial 10")
frame2.label3 <- tklabel(frame2, text = "Nb. of bars", font = "arial 10")
frame2.label4 <- tklabel(frame2, text = "Weights", font = "arial 10")
frame2.label5 <- tklabel(frame2, text = "Resolution", font ="arial 10")

frame2.entry1 <- tkentry(frame2, textvariable = OPTIONS.TITLE, width = 20, justify = "left")
frame2.entry2 <- tkentry(frame2, textvariable = OPTIONS.LAYOUT, width = 20, justify = "left")
frame2.entry3 <- tkentry(frame2, textvariable = OPTIONS.NBBARS, width = 20, justify = "left")
frame2.entry4 <- tkentry(frame2, textvariable = OPTIONS.WEIGHTS, width = 20, justify = "left")
frame2.entry5 <- tkentry(frame2, textvariable = OPTIONS.RESOLUTION, width = 20, justify = "left")

tkgrid(frame2.main, columnspan = 2, sticky = "w")
tkgrid(frame2.label1, frame2.entry1, sticky = "w")
tkgrid(frame2.label2, frame2.entry2, sticky = "w")
tkgrid(frame2.label3, frame2.entry3, sticky = "w")
tkgrid(frame2.label4, frame2.entry4, sticky = "w")
tkgrid(frame2.label5, frame2.entry5, sticky = "w")

tkpack(frame2, fill = "x")

# Defining objects in frame3
apply.but <- tkbutton(frame3,text = "Apply", command = options.apply, padx = 40, font = "arial 10")
defaults.but <- tkbutton(frame3, text = "Defaults", command = options.default, padx=40, font = "arial 10")
close.but <- tkbutton(frame3, text = "Close", command = options.close, padx = 40, font = "arial 10")
tkgrid(apply.but, defaults.but, close.but, sticky = "w")
tkpack(frame3, fill = "x")
}
