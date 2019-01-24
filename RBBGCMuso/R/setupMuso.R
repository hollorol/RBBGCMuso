#' setupMuso
#'
#' The setupMuso is fundamental for the Biome-BGCMuSo model related other functions like runMuso, spinupMuso, normalMuso, rungetMuso, as it sets the model's environment. The function reads the INI files from a given directory, analyzes them with error checking, and creates a data structure in R that contains the complete information content for the simulation.  
#'
#' @author Roland HOLLOS
#' @param parallel Set this variable to TRUE if you would like to implement parallel execution of the model 
#' @param executable This parameter stores the location (directory) of the model-executable file. In normal usage, you don't have to set this parameter, because the RBBGCMuso package always contains the latest model executable. In spite of this, if you would like to use this package for model development or just want to use different model version (for example for comparison), you will find this option useful
#' @param calibrationPar You might want to change some parameters in your EPC file before running the model. setupMuso offers possibility for this without editing the EPC file. In this situation you have to select the appropirate model parameters first. You can refer to these parameters with the number of the line in the EPC file. Indexing of lines start from one. You should use a vector for this referencing like c(1,5,8)
#' @param outputLoc With this parameter the user can specify the directory for the model output. The syntax is simple, for example: outputLoc="/place/of/the/outputs/" or outputLoc="C:/my_model_directory/". Note that this output directory is specified by the user within the INI file, which means that the outputLoc parameter overrides the INI settings if specified.
#' @param inputLoc Usually this is the root (or base) directory where the user stores the INI files for the model. If the working directory is set by the user, this parameter can be skipped. 
#' @param metInput Via the metInput parameter the user can specify the location of the input meteorological files. By default the package reads this information from the INI files.
#' @param CO2Input Via the CO2Input parameter the user can specify the location of the CO2 data file. By default the package reads this information from the INI files.
#' @param plantInput Via the plantInput parameter, the user can specify the location of the the file that contains the planting information. By default the package reads this information from the INI files.
#' @param thinInput Via the thinInput parameter,the user can specify the location of the file that contains the thinning information. By default the package reads this information from the INI files.
#' @param mowInput Via the mowInput parameter, the user can specify the location of the file that contains the mowing (i.e. grass cutting) information. By default the package reads this information from the INI files.
#' @param grazInput Via the grazInput parameter, the user can specify the location of the file that contains the grazing information. By default the package reads this information from the INI files.
#' @param harvInput Via the harvInput parameter, the user can specify the location of the file that contains the harvesting information. By default the package reads this information from the INI files.
#' @param plougInput Via the plougInput parameter, the user can specify the location of the file that contains the ploughing information. By default the package reads this information from the INI files.
#' @param fertInput Via the fertInput parameter, ythe user can specify the location of the file that contains the fertilizing information. By default the package reads this information from the INI files.
#' @param irrInput Via the irrInput parameter, the user can specify the location of the file that contains the irrigation information. By default the package reads this information from the INI files.
#' @param nitInput Via the nitInput parameter, the user can specify the location of the file that contains the nitrogen deposition data. By default the package reads this information from the INI files.
#' @param iniInput Via the iniInput parameter, the user can specify the location of the INI files. By default the package reads the INI files from the working directory. 
#' @param epcInput Via the epcInput parameter, the user can specify the location of the EPC data file. By default the package reads this information from the INI files.
#' @param modelOutputs This parameter contains the list of the codes that defines the required model output variables. Check the Biome-BGCMuS website for the complete list of possible output variables at http://agromo.agrar.mta.hu/bbgc/download.html 
#' @usage setupMuso(executable=NULL, parallel = F, calibrationPar =c(1),
#' outputLoc=NULL, inputLoc=NULL,
#' metInput=NULL, CO2Input=NULL,
#' plantInput=NULL, thinInput=NULL,
#' mowInput=NULL, grazInput=NULL,
#' harvInput=NULL, plougInput=NULL,
#' fertInput=NULL, irrInput=NULL,
#' nitInput=NULL, iniInput=NULL, epcInput=NULL)
#' @return The output is a the model settings list wich contains the following elements:
#' executable, calibrationPar, outputLoc, outputName, inputLoc, iniInput, metInput, epcInput,thinInput,CO2Input, mowInput, grazInput, harvInput, plougInput, fertInput,rrInput, nitInput, inputFiles, numData, startyear, numYears, outputVars
#' @export

setupMuso <- function(executable=NULL,
                      parallel = F,
                      calibrationPar =c(1),
                      outputLoc=NULL,
                      modelOutputs=NULL,
                      inputLoc=NULL,
                      metInput=NULL,
                      CO2Input=NULL,
                      plantInput=NULL,
                      thinInput=NULL,
                      mowInput=NULL,
                      grazInput=NULL,
                      harvInput=NULL,
                      plougInput=NULL,
                      fertInput=NULL,
                      irrInput=NULL,
                      nitInput=NULL,
                      iniInput=NULL,
                      epcInput=NULL,
                      mapData=NULL,
                      leapYear=FALSE,
                      version=5
                      ){

    Linuxp <-(Sys.info()[1]=="Linux")
    writep <- 0

    if(is.null(mapData)&version==4){
        mData <- mMapping4
    }
    
    inputParser <- function(string,fileName,counter,value=TRUE){
        unlist(strsplit(grep(string,fileName,value=TRUE),"[\ \t]"))[counter]
    }

    outMaker <- function(inputVar,grepString,filep){
        tempVar <- eval(parse(text=inputVar))
        if(is.null(tempVar)){
            writep <<- writep+1
            if(filep)
            {
                tempVar["spinup"] <- file.path(inputLoc,inputParser(string=grepString,fileName=iniFiles$spinup,counter=1,value=TRUE))
                tempVar["normal"] <- file.path(inputLoc,inputParser(string=grepString,fileName=iniFiles$normal,counter=1,value=TRUE))
            } else {
                tempVar["spinup"] <- inputParser(string=grepString,fileName=iniFiles$spinup,counter=1,value=TRUE)
                tempVar["normal"] <- inputParser(string=grepString,fileName=iniFiles$normal,counter=1,value=TRUE)
                
            }

        } else {
            iniFiles$spinup[grep(grepString,iniFiles$spinup)] <<- paste0(tempVar[1],"\t ",grepString)
            
            if(length(tempVar)==2){
                iniFiles$normal[grep(" grepString",iniFiles$normal)] <<- paste0(tempVar[2],"\t ",grepString)
            }
        }
        return(tempVar)
    }
    
    if(is.null(inputLoc)){
        inputLoc<- normalizePath("./")
    } else{
        inputLoc <- normalizePath(inputLoc)
    }
    
    #iniChangedp <- FALSE

    if(is.null(iniInput)){
        spinups<-grep("s.ini$",list.files(inputLoc),value=TRUE)
        normals<-grep("n.ini$",list.files(inputLoc),value=TRUE)

        if(length(spinups)==1){
            iniInput[1]<-file.path(inputLoc,spinups)
        } else {stop("There are multiple or no spinup ini files, please choose")}
        

        if(length(normals)==1){
            iniInput[2]<-file.path(inputLoc,normals)
        } else {stop("There are multiple or no normal ini files, please choose")}

    }

    ##read the ini files for the further changes

    iniFiles<-lapply(iniInput, function (x) readLines(x,-1))
    iniFiles[[1]] <- gsub("\\","/", iniFiles[[1]],fixed=TRUE) #replacing \ to /
    iniFiles[[2]] <- gsub("\\","/", iniFiles[[2]],fixed=TRUE) #replacing \ to /
    names(iniFiles) <- c("spinup","normal")


    
    inputs <- lapply(1:nrow(grepHelper), function (x) {

        outMaker(grepHelper[x,1],grepHelper[x,2],grepHelper[x,3])
        
    })   
    names(inputs) <- grepHelper$inputVar   
    ## grepHelper is in sysdata.rda it is a table like this:
    ##
    ## inputVar         string      isFile  
    ## 1    epcInput  EPC file name TRUE 
    ## 2    metInput  met file name TRUE
    ## 3    CO2Input       CO2 file TRUE
    ## 4    nitInput     N-dep file TRUE
    ## 5   thinInput    do THINNING FALSE
    ## 6  plantInput    do PLANTING FALSE
    ## 7    mowInput      do MOWING FALSE
    ## 8   grazInput     do GRAZING FALSE
    ## 9   harvInput  do HARVESTING FALSE
    ## 10 plougInput   do PLOUGHING FALSE
    ## 11  fertInput do FERTILIZING FALSE
    ## 12   irrInput  do IRRIGATION FALSE
   # return(inputs) debug element 

    
    if(is.null(mapData)){
        
        outIndex<-grep("DAILY_OUTPUT",iniFiles[[2]])+1
        numVar<-as.numeric(unlist(strsplit(iniFiles[[2]][outIndex],"[\ \t]"))[1])
        dailyVarCodes<-tryCatch(iniFiles[[2]][(outIndex+1):(outIndex+numVar)],
                                error = function(e){
        stop("Cannot read indexes of output variables from the normal ini file, please make sure you have not skiped a line after the flag: \"DAILY_OUTPUT\"")    
        })
        dailyVarnames<-lapply(dailyVarCodes, function(x) musoMapping(unlist(strsplit(x,"[\ \t]"))[1]))

        outIndex<-grep("ANNUAL_OUTPUT",iniFiles[[2]])+1
        numVar<-as.numeric(unlist(strsplit(iniFiles[[2]][outIndex],"[\ \t]"))[1])
        annualVarCodes<-iniFiles[[2]][(outIndex+1):(outIndex+numVar)]
        annualVarnames<-lapply(annualVarCodes, function(x) musoMapping(unlist(strsplit(x,"[\ \t]"))[1]))
        outputVars<-list(dailyVarnames,annualVarnames)} else {

                                                          c<-grep("DAILY_OUTPUT",iniFiles[[2]])+1
                                                          numVar<-as.numeric(unlist(strsplit(iniFiles[[2]][c],"[\ \t]"))[1])
                                                          dailyVarCodes<-iniFiles[[2]][(c+1):(c+numVar)]
                                                          dailyVarnames<-lapply(dailyVarCodes, function(x) musoMapping(unlist(strsplit(x,"[\ \t]"))[1],mapData))

                                                          c<-grep("ANNUAL_OUTPUT",iniFiles[[2]])+1
                                                          numVar<-as.numeric(unlist(strsplit(iniFiles[[2]][c],"[\ \t]"))[1])
                                                          annualVarCodes<-iniFiles[[2]][(c+1):(c+numVar)]
                                                          annualVarnames<-lapply(annualVarCodes, function(x) musoMapping(unlist(strsplit(x,"[\ \t]"))[1],mapData))
                                                          outputVars<-list(dailyVarnames,annualVarnames)


                                                          
                                                      }
    
    
    
    if(is.null(executable)){
        if(Linuxp){
            executable<-file.path(inputLoc,"muso")
        } else {
            executable<-file.path(inputLoc,"muso.exe")
        }
    } else {        
        file.copy(executable,inputLoc)
    }
    outputName <- character(2)
    outputName[1] <- basename(unlist(strsplit(iniFiles[[1]][grep("OUTPUT_CONTROL",iniFiles[[1]])+1],"[\ \t]"))[1])
    outputName[2] <- basename(unlist(strsplit(iniFiles[[2]][grep("OUTPUT_CONTROL",iniFiles[[2]])+1],"[\ \t]"))[1])
    ##  outputName <- unlist(strsplit(grep("output",grep("prefix",iniFiles[[2]],value=TRUE),value=TRUE),"[\ \t]"))[1]
    ##THIS IS AN UGLY SOLUTION, WHICH NEEDS AN UPGRADE!!! FiXED (2017.09.11)
    ## outputName <- unlist(strsplit(grep("prefix for output files",iniFiles[[2]],value=TRUE),"[\ \t]"))[1]
    if(is.null(outputName)){
        stop("I cannot find outputName in your default ini file \n Please make sure that the line wich contains the name also contains the prefix and the output keywords!")
        
    }
    ##    outputName<-unlist(read.table(iniInput[2],skip=93,nrows = 1))[1]


    if(is.null(outputLoc)){
        ##  outputLoc<-paste((rev(rev(unlist(strsplit(outputName,"/")))[-1])),collapse="/")
        outputLoc <- dirname(unlist(strsplit(iniFiles[[2]][grep("OUTPUT_CONTROL",iniFiles[[2]])+1],"[\ \t]"))[1])
        if(substr(outputLoc,start = 1,stop = 1)!="/"){
            ##if the outputName is not absolute path make it absolute
            outputLoc <- file.path(inputLoc,outputLoc)
        } 
    } else {
        outputLoc <- normalizePath(outputLoc)
    }

    
    
    inputFiles<-c(iniInput,epcInput,metInput)
    numData<-rep(NA,3)
    numYears <-  as.numeric(unlist(strsplit(iniFiles[[2]][grep("TIME_DEFINE",iniFiles[[2]])+2],"[\ \t]"))[1])
    ##    numYears<-unlist(read.table(iniInput[2],skip = 14,nrows = 1)[1])
    numValues <-  as.numeric(unlist(strsplit(iniFiles[[2]][grep("DAILY_OUTPUT",iniFiles[[2]])+1],"[\ \t]"))[1])
    ## numValues will be replaced to numVar
    ## numValues<-unlist(read.table(iniInput[2],skip=102,nrows = 1)[1])
    startYear <- as.numeric(unlist(strsplit(iniFiles[[2]][grep("TIME_DEFINE",iniFiles[[2]])+3],"[\ \t]"))[1])
    numData[1] <- numValues * sumDaysOfPeriod(startYear,numYears,corrigated=leapYear)

    numData[2] <- numYears * numValues*12
    numData[3] <- numYears * numValues

    ##Writing out changed ini-file

    writeLines(iniFiles[[1]],iniInput[1])
    writeLines(iniFiles[[2]],iniInput[2])

    if(!is.null(modelOutputs)){
        outVarChanges <- putOutVars(iniFile = iniInput[2],outputVars = modelOutputs, modifyOriginal = TRUE)
        numData <- round(numDate*outVarChanges[[2]])
        outputVars[[1]] <-outVarChanges[[1]] 
    }
    

    suppressWarnings(file.remove(paste0(file.path(outputLoc,outputName[1]),".log")))
    ## I use file.path additionally because We do not know if outputLoc ends or not to "/"
    suppressWarnings(file.remove(paste0(file.path(outputLoc,outputName[2]),".log")))
    
    settings = list(executable = executable,
                    calibrationPar = calibrationPar,
                    outputLoc=outputLoc,
                    outputNames=outputName,
                    inputLoc=inputLoc,
                    iniInput=iniInput,
                    metInput=inputs$metInput,
                    epcInput=inputs$epcInput,
                    thinInput=inputs$thinInput,
                    CO2Input=inputs$CO2Input,
                    mowInput=inputs$mowInput,
                    grazInput=inputs$grazInput,
                    harvInput=inputs$harvInput,
                    plougInput=inputs$plougInput,
                    fertInput=inputs$fertInput,
                    irrInput=inputs$irrInput,
                    nitInput=inputs$nitInput,
                    inputFiles=inputFiles,
                    numData=numData,
                    startYear=startYear,
                    numYears=numYears,
                    outputVars=outputVars
                    )
    
    if(writep!=nrow(grepHelper)){
        writeLines(iniFiles[[1]],iniInput[[1]])
        if(inputs$epcInput[1]!=inputs$epc$Input[2]){ #Change need here
            writeLines(iniFiles[[2]],iniInput[[2]])      
        }
    }
    return(settings)

}

