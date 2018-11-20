#'copyMusoExampleTo
#'
#'With this function you can copy RBBGCMuso example library whereever you want
#'
#'@param example The name of the example file, if it is NULL tcl/tk menu will pop up to select.
#'@return List of two matrices: A and B matrix for musoRandomizer
#'@import stats
#'@import magrittr
#'@import dplyr
#'@export

copyMusoExamleTo <- function(example = NULL, destination = NULL){
    WindowsP <- Sys.info()[1] == "Windows"

    chooseExample <- function(){
            choiceWin <- tcltk::tktoplevel()
            tcltk::tclRequire("BWidget")
            tcltk::tktitle(choiceWin) <- "Choose an example!"
            tcltk::tcl("wm","geometry",choiceWin,"200x50")
            tcl("wm", "attributes", base, topmost=TRUE)
            choiceValues <-  basename(list.dirs(system.file("examples","",package = "RBBGCMuso"),recursive = FALSE))
            choices <- tcltk::tkwidget(choiceWin,"ComboBox",
                                       editable = FALSE, values = choiceValues,
                                       textvariable = tcltk::tclVar(choiceValues[1]))
            tcltk::tkpack(choices)
            choiceValue <- NA
            closeSelection <- tcltk::tkwidget(choiceWin,"button",text ="Select", command =function (){
                choiceValue <<- tcltk::tclvalue(tcltk::tcl(choices,"get"))
                tcltk::tkdestroy(choiceWin)
            })
            
            tcltk::tkpack(closeSelection)
            while(as.numeric(tcltk::tclvalue(tcltk::tcl("winfo","exists",choiceWin)))){
                
            }
            return(choiceValue)
    }


    
    if(is.null(example)){
        cExample<-paste0(system.file("examples","",package = "RBBGCMuso"),"/",chooseExample())       
    }
    
    if(is.null(destination)){
        destination<-tcltk::tk_choose.dir(getwd(), "Choose folder to copy the examples!")
    }
    
    currDir <- getwd()
    setwd(cExample)
    if(!WindowsP){
        file.copy("./bin/muso", destination)
    } else {
        file.copy("./bin/muso.exe", destination)
        file.copy("./bin/cygwin1.dll", destination)
    }
        file.copy(grep("bin", list.files(), value = TRUE, invert = TRUE),destination)
     setwd(currDir)
}
## choiceWin <- tcltk::tktoplevel()
## tclRequire("BWidget")
## 
## choiceValues <-  basename(list.dirs("~/Documents/projects",recursive = FALSE))
## choices <- tkwidget(choiceWin,"ComboBox", editable = FALSE, values = choiceValues, textvariable = tclVar(choiceValues[1]))
## tkpack(choices)
## filename<- tclvalue(tcl(choices,"get"))
## filename
