#'copyMusoExampleTo
#'
#'With this function you can copy RBBGCMuso example library whereever you want
#'
#'@param example The name of the example file, if it is NULL tcl/tk menu will pop up to select.
#'@param destination The destination where the example files will be copied.
#'@import tcltk
#'@export

copyMusoExamleTo <- function(example = NULL, destination = NULL){
  WindowsP <- Sys.info()[1] == "Windows"
  
  chooseExample <- function(){
    choiceWin <- tktoplevel()
    tclRequire("BWidget")
    tktitle(choiceWin) <- "Choose an example!"
    tcl("wm","geometry",choiceWin,"200x50")
    tcl("wm", "attributes", choiceWin, topmost=TRUE)
    choiceValues <-  basename(list.dirs(system.file("examples","",package = "RBBGCMuso"),recursive = FALSE))
    choices <- tkwidget(choiceWin,"ComboBox",
                               editable = FALSE, values = choiceValues,
                               textvariable = tclVar(choiceValues[1]))
    tcltk::tkpack(choices)
    choiceValue <- NA
    closeSelection <- tkwidget(choiceWin,"button",text ="Select", command =function (){
      choiceValue <<- tclvalue(tcl(choices,"get"))
      tkdestroy(choiceWin)
    })
    
    tcltk::tkpack(closeSelection)
    while(as.numeric(tclvalue(tcl("winfo","exists",choiceWin)))){
      
    }
    return(choiceValue)
  }
  
  
  
  if(is.null(example)){
    cExample<-paste0(system.file("examples","",package = "RBBGCMuso"),"/",chooseExample())       
  }
  
  if(is.null(destination)){
    destination<-tk_choose.dir(getwd(), "Choose folder to copy the examples!")
  }
  
  currDir <- getwd()
  setwd(cExample)
  if(!WindowsP){
      file.copy(grep("(exe|dll)$", list.files(), value = TRUE, invert = TRUE),destination)
  } else {
      file.copy(grep("^muso$", list.files(), value = TRUE, invert = TRUE),destination)    
  }
  setwd(currDir)
}
