#' setupMuso
#'
#' This funcion is fundamental for the BiomBGC-MuSo modell related functions like spinupMuso, normalMuso, rungetMuso, because it sets the modells environment.
#'
#' @author Roland Hollos
#' @param parallel Do you want to run multiple modell paralelly, if yes, set this variable to TRUE
#' @param executable This parameter stores the place of the modell-executable file. In normal usage, you don't have to be set this, because a RBBgcmuso package contains allways the latest modell executable. In spite of this, if you would like to use this package for modell development or just want to use different models (for example for comparison), you will find it useful
#' @param calibrationPar You may want to change some parameters in your epc file, before you run the modell. You have to select the appropirate modell parameters. You can refence to these with the number of the line in the epc file where the variables are. It indexes from one. You should use a vector for this, like: c(1,5,8)
#' @param outputLoc Where should the modell puts its outputs. You should give a location for it via this variable, for example: outputLoc="/place/of/the/outputs/"
#' @param inputLoc Usually it is the root directory, where you put the iniFiles for the modell
#' @param metInput Via metInput parameter, you can tell the modell where are the meteorological files. As default it reads this from the iniFiles.
#' @param CO2Input Via CO2 parameter, you can tell the modell where are the CO2 data files. As default it reads this from the iniFiles.
#' @param plantInput Via planting parameter, you can tell the modell where are the data files, which contains the planting informations. As default it reads this from the iniFiles.
#' @param thinInput Via thining parameter, you can tell the modell where are the data files, which contains the thining informations. As default it reads this from the iniFiles.
#' @param mowInput Via mowing parameter, you can tell the modell where are the data files, which contains the mowing informations. As default it reads this from the iniFiles.
#' @param grazInput Via grazing parameter, you can tell the modell where are the data files, which contains the grazing informations. As default it reads this from the iniFiles.
#' @param harvInput Via harvesting parameter, you can tell the modell where are the data files, which contains the harvesting informations. As default it reads this from the iniFiles.
#' @param plougInput Via ploughing parameter, you can tell the modell where are the data files, which contains the ploughing informations. As default it reads this from the iniFiles.
#' @param fertInput Via fertilizing parameter, you can tell the modell where are the fertilizing data files, which contains the fertilizing informations. As default it reads this from the iniFiles.
#' @param irrInput Via irrigation parameter, you can tell the modell where are the data files, which contains the irrigation informations. As default it reads this from the iniFiles.
#' @param nitInput Via this parameter, you can tell the modell where are the NO2 data files. As default it reads this from the iniFiles.
#' @param iniInput Via this parameter, you can tell the modell where are the ini files. As default it reads this from the iniFiles.
#' @param epcInput Via this parameter, you can tell the modell where are the epc data files. As default it reads this from the iniFiles.
#' @usage setupMuso(executable=NULL, parallel = F, calibrationPar =c(1),
#' outputLoc=NULL, inputLoc=NULL,
#' metInput=NULL, CO2Input=NULL,
#' plantInput=NULL, thinInput=NULL,
#' mowInput=NULL, grazInput=NULL,
#' harvInput=NULL, plougInput=NULL,
#' fertInput=NULL, irrInput=NULL,
#' nitInput=NULL, iniInput=NULL, epcInput=NULL)
#' @return The output is a the modell setting list wich contains the following elements:
#' executable, calibrationPar, outputLoc, outputName, inputLoc, iniInput, metInput, epcInput,thinInput,CO2Input, mowInput, grazInput, harvInput, plougInput, fertInput, irrInput, nitInput, inputFiles, numData, startyear, numYears, outputVars
#' @export
setupMuso <- function(executable=NULL,
                      parallel = F,
                      calibrationPar =c(1),
                      outputLoc=NULL,
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
        
        c<-grep("DAILY_OUTPUT",iniFiles[[2]])+1
        numVar<-as.numeric(unlist(strsplit(iniFiles[[2]][c],"[\ \t]"))[1])
        dailyVarCodes<-iniFiles[[2]][(c+1):(c+numVar)]
        dailyVarnames<-lapply(dailyVarCodes, function(x) musoMapping(unlist(strsplit(x,"[\ \t]"))[1]))

        c<-grep("ANNUAL_OUTPUT",iniFiles[[2]])+1
        numVar<-as.numeric(unlist(strsplit(iniFiles[[2]][c],"[\ \t]"))[1])
        annualVarCodes<-iniFiles[[2]][(c+1):(c+numVar)]
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
        stop("I cannot find outputName in your default ini file \n Please make sure that the line wich contains the name also contains the prefix and the outmut keywords!")
        
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
    numYears <-  as.numeric(unlist(strsplit(grep("simulation years",iniFiles[[2]],value=TRUE),"[\ \t]"))[1])
    ##    numYears<-unlist(read.table(iniInput[2],skip = 14,nrows = 1)[1])
    numValues <-  as.numeric(unlist(strsplit(grep("number of daily output variables",iniFiles[[2]],value=TRUE),"[\ \t]"))[1])
    ## numValues will be replaced to numVar
    ## numValues<-unlist(read.table(iniInput[2],skip=102,nrows = 1)[1])
    startyear <- as.numeric(unlist(strsplit(grep("first simulation year",iniFiles[[2]],value=TRUE),"[\ \t]"))[1])
    numData[1] <- numValues * sumDaysOfPeriod(startyear,numYears,corrigated=leapYear)
        
    numData[2] <- numYears * numValues*12
    numData[3] <- numYears * numValues

    ##Writing out changed ini-file

    writeLines(iniFiles[[1]],iniInput[1])
    writeLines(iniFiles[[2]],iniInput[2])

    suppressWarnings(file.remove(file.path(outputLoc,outputNames[1])))
    suppressWarnings(file.remove(file.path(outputLoc,outputNames[2])))
    
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
                    startyear=startyear,
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

