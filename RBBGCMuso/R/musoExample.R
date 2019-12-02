#'copyMusoExampleTo
#'
#'This function enables the user to download a complete, working file set to quickly start using Biome-BGCMuSo through RBBGCMuso (or in standalone mode). The user has to specify the target directory for the files. The file set contains the model executable (muso.exe in Windows), the INI files that drive the model, and other files like meteorology input, ecophysiological constants file (EPC), and other ancillary files (CO2 concentration, parameter range definition file called parameters.csv). Note that we strongly recommend to read the User's Guide of Biome-BGCMuSo to clarify the meaning of the input files. The input files (s.ini, n.ini, maize.epc, meteorology files) are simple text files, so the user can read (and modify) them with his/her favourite text editor (like Editpad Lite, vim, emacs). Note that some files use UNIX/Linux style text which means that the text will not be readable using the Windows Notepad. 
#'
#'@param example This is the name of the example file. If it is not set then a simple graphical user interface (tcl/tk menu) will open to select the target dataset (which is typically an experimental site). In the list hhs means the Hegyhatsal eddy covariance site in Hungary. 
#'@param destination The destination where the example files will be copied.
#'@export

copyMusoExampleTo <- function(example = NULL, destination = NULL){
  WindowsP <- Sys.info()[1] == "Windows"
  
  chooseExample <- function(){
    choiceWin <- tcltk::tktoplevel()
    tcltk::tclRequire("BWidget")
    tcltk::tktitle(choiceWin) <- "Choose an example!"
    tcltk::tcl("wm","geometry",choiceWin,"200x50")
    tcltk::tcl("wm", "attributes", choiceWin, topmost=TRUE)
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
  } else {
    cExample <- paste0(system.file("examples","",package = "RBBGCMuso"),"/","hhs") 
  }
  
  if(is.null(destination)){
    destination<-tcltk::tk_choose.dir(getwd(), "Choose folder to copy the examples!")
  }
  
  currDir <- getwd()
  setwd(cExample)
  if(!WindowsP){
      file.copy(grep("(exe|dll)$", list.files(), value = TRUE, invert = TRUE),destination)
  } else {
      file.copy(grep("^muso$", list.files(), value = TRUE, invert = TRUE),destination)    
  }
  setwd(destination)
}
