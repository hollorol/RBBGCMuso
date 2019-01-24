#' runMuso 
#'
#' This function runs the Biome-BGCMuSo model (with option to change the EPC file), then it reads its output file in a well-structured way. As the result is passed to R, the results can be easily post-processed in R environment. 
#' 
#' @author Roland HOLL\'{O}S
#' @param settings RBBGCMuso uses variables that define the entire simulation environment. Those environment variables include the name of the INI files, the name of the meteorology files, the path to the model executable and its file name, the entire output list, the entire output variable matrix, the dependency rules for the EPC parameters etc. Using the runMuso function RBBGCMuso can automatically create those environment variables by inspecting the files in the working directory (this happens through the setupMuso function). It means that by default model setup is performed automatically in the background and the user has nothing to do. With this settings parameter we can force runMuso to skip automatic environment setup as we provide the environment settings to runMuso. In a typical situation the user can skip this option.
#' @param timee The required timesteps in the modell output. It can be "d", if it is daily, "m", if it's monthly, "y", it it is yearly. I recommend to use daily data, the yearly and monthly data is not well-tested yet. 
#' @param debugging If it is TRUE, it copies the log file to a Log directory to store it, if it is stamplog it contatenate a number before the logfile, which is one more than the maximum of the represented ones in the LOG directory. If it is true or stamplog it collects the "wrong" logfiles
#' @param keepEpc If TRUE, it keeps the epc file and stamp it, after these copies it to the EPCS directory. If debugging True or false, it copies the wrong epc files to the wrong epc directory.
#' @param export if it is yes or you give a filename here, it converts the ouxtput to the specific extension. For example, if you set export to "example.csv", it converts the output to "csv", if you set it to "example.xls" it converts to example.xls with the xlsx package. If it is not installed it gives back a warning message and converts it to csv.
#' @param silent If you set it TRUE all off the modells output to the screen will be suppressed. It can be usefull, because it increases the model-speed.
#' @param aggressive It deletes every possible modell-outputs from the previous modell runs.
#' @param parameters In the settings variable you have set the row indexes of the variables, you wish to change. In this parameter you can give an exact value for them in a vector like: c(1,2,3,4)
#' @param logfilename If you want to set a specific name for your logfiles you can set this via logfile parameter
#' @param leapYear  Should the function do a leapyear correction on the outputdata? If TRUE, then the 31.12 day will be doubled.
#' @param keepBinary In default RBBGCMuso to keep  working area as clean as possible, deletes all the regular output files. The results are directly printed to the standard output, but you can redirect it, and save it to a variable, or you can export your results to the desired destination in a desired format. Whith this variable you can enable to keep the binary output files. If you want to set the location of the binary output, please take a look at the binaryPlace argument.
#' @param binaryPlace The place of the binary output files.
#' @param fileToChange You can change any line of the epc or the ini file, you just have to specify with this variable which file you van a change. Two options possible: "epc", "ini"
#' @param skipSpinup If TRUE, calibMuso wont do spinup simulation
#' @param prettyOut date ad Date type, separate year, month, day vectors
#' @return No return, outputs are written to file 
#' @usage calibMuso(settings,parameters=NULL, timee="d", debugging=FALSE, logfilename=NULL,
#' keepEpc=FALSE, export=FALSE, silent=FALSE, aggressive=FALSE, leapYear=FALSE)
#' @import utils
#' @export
runMuso <- function(settings=NULL, calibrationPar=NULL,
                      parameters=NULL, outVars = NULL, timee="d",
                      debugging=FALSE, logfilename=NULL,
                      keepEpc=FALSE, export=FALSE,
                      silent=FALSE, aggressive=FALSE,
                      leapYear=FALSE,keepBinary=FALSE,
                      binaryPlace="./", fileToChange="epc",
                      skipSpinup = TRUE, modifyOriginal =FALSE, prettyOut = FALSE){
    calibMuso(settings, calibrationPar, parameters, outVars, timee,
                      debugging, logfilename, keepEpc, export, silent, aggressive,
                      leapYear,keepBinary, binaryPlace, fileToChange,
                      skipSpinup, modifyOriginal, prettyOut)
}
