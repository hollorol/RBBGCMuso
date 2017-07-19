#' setupMuso
#'
#' This funcion is fundamental for the BiomBGC-MuSo modell related functions like spinupMuso, normalMuso, rungetMuso, because it sets the modells environment.
#'
#' @author Roland Hollos
#' @param parallel Do you want to run multiple modell paralelly, if yes, set this variable to TRUE
#' @param executable This parameter stores the place of the modell-executable file. In normal usage, you don't have to be set this, because a RBBgcmuso package contains allways the latest modell executable. In spite of this, if you would like to use this package for modell development or just want to use different models (for example for comparison), you will find it useful
#' @param calibrationpar You may want to change some parameters in your epc file, before you run the modell. You have to select the appropirate modell parameters. You can refence to these with the number of the line in the epc file where the variables are. It indexes from one. You should use a vector for this, like: c(1,5,8)
#' @param outputloc Where should the modell puts its outputs. You should give a location for it via this variable, for example: outputloc="/place/of/the/outputs/"
#' @param inputloc Usually it is the root directory, where you put the inifiles for the modell
#' @param metinput Via metinput parameter, you can tell the modell where are the meteorological files. As default it reads this from the inifiles.
#' @param CO2input Via CO2 parameter, you can tell the modell where are the CO2 data files. As default it reads this from the inifiles.
#' @param plantinput Via planting parameter, you can tell the modell where are the data files, which contains the planting informations. As default it reads this from the inifiles.
#' @param thininput Via thining parameter, you can tell the modell where are the data files, which contains the thining informations. As default it reads this from the inifiles.
#' @param mowinput Via mowing parameter, you can tell the modell where are the data files, which contains the mowing informations. As default it reads this from the inifiles.
#' @param grazinput Via grazing parameter, you can tell the modell where are the data files, which contains the grazing informations. As default it reads this from the inifiles.
#' @param harvinput Via harvesting parameter, you can tell the modell where are the data files, which contains the harvesting informations. As default it reads this from the inifiles.
#' @param plouginput Via ploughing parameter, you can tell the modell where are the data files, which contains the ploughing informations. As default it reads this from the inifiles.
#' @param fertinput Via fertilizing parameter, you can tell the modell where are the fertilizing data files, which contains the fertilizing informations. As default it reads this from the inifiles.
#' @param irrinput Via irrigation parameter, you can tell the modell where are the data files, which contains the irrigation informations. As default it reads this from the inifiles.
#' @param nitinput Via this parameter, you can tell the modell where are the NO2 data files. As default it reads this from the inifiles.
#' @param ininput Via this parameter, you can tell the modell where are the ini files. As default it reads this from the inifiles.
#' @param epcinput Via this parameter, you can tell the modell where are the epc data files. As default it reads this from the inifiles.
#' @usage setupMuso(executable=NULL, parallel = F, calibrationpar =c(1),
#' outputloc=NULL, inputloc=NULL,
#' metinput=NULL, CO2input=NULL,
#' plantinput=NULL, thininput=NULL,
#' mowinput=NULL, grazinput=NULL,
#' harvinput=NULL, plouginput=NULL,
#' fertinput=NULL, irrinput=NULL,
#' nitinput=NULL, ininput=NULL, epcinput=NULL)
#' @return The output is a the modell setting list wich contains the following elements:
#' executable, calibrationpar, outputloc, outputname, inputloc, ininput, metinput, epcinput,thininput,CO2input, mowinput, grazinput, harvinput, plouginput, fertinput, irrinput, nitinput, inputfiles, numdata, startyear, numyears, outputvars
setupMuso <- function(executable=NULL,
                  parallel = F,
                  calibrationpar =c(1),
                  outputloc=NULL,
                  inputloc=NULL,
                  metinput=NULL,
                  CO2input=NULL,
                  plantinput=NULL,
                  thininput=NULL,
                  mowinput=NULL,
                  grazinput=NULL,
                  harvinput=NULL,
                  plouginput=NULL,
                  fertinput=NULL,
                  irrinput=NULL,
                  nitinput=NULL,
                  ininput=NULL,
                  epcinput=NULL
                  ){

    Linuxp <-(Sys.info()[1]=="Linux")

    if(is.null(inputloc)){
        inputloc<- "./"
    } else {
    inp <- unlist(strsplit(inputloc,"")) #This is the charactervector of the given imput location

    if(inp[length(inp)]!="/"){
        inp<-c(inp,"/")
        inputloc <- paste(inp,collapse = "")
        rm(inp)
    }# If inp not ends in / paste one at the end, then make a string, that will be the new inputloc

    ##Example: "a/b/c ==> a/b/c/"
    }
    inichangedp <- FALSE

    if(is.null(ininput)){
        spinups<-grep("s.ini$",list.files(inputloc),value=TRUE)
        normals<-grep("n.ini$",list.files(inputloc),value=TRUE)

        if(length(spinups)==1){
            ininput[1]<-paste(inputloc,spinups,sep="")
        } else {return(print("There are multiple or no spinup files, please choose"))}
        

        if(length(normals)==1){
            ininput[2]<-paste(inputloc,normals,sep="")
        } else {return(print("There are multiple or no normal files, please choose"))}

    }

    ##read the ini files for the further changes

    inifiles<-lapply(ininput, function (x) readLines(x,-1))
    inifiles[[1]] <- gsub("\\","/", inifiles[[1]],fixed=TRUE) #replacing \ to /
    inifiles[[2]] <- gsub("\\","/", inifiles[[2]],fixed=TRUE) #replacing \ to /
    
    if(is.null(epcinput)){
        epcflag=TRUE
        epcinput[1] <-  unlist(strsplit(grep(" EPC file name",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        epcinput[2] <-  unlist(strsplit(grep(" EPC file name",inifiles[[2]],value=TRUE),"[\ \t]"))[1]
    } else {
        inifiles[[1]][grep(" EPC file name",inifiles[[1]])]<-paste(epcinput[1],"\t EPC file name",sep="")

        if(length(epcinput)==2){
            inifiles[[2]][grep(" EPC file name",inifiles[[2]])]<-paste(epcinput[2],"\t EPC file name",sep="")
        }
    }

    if(is.null(metinput)){
        metflag=TRUE
        metinput[1] <-  unlist(strsplit(grep(" met file name",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        metinput[2] <-  unlist(strsplit(grep(" met file name",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep(" met file name",inifiles[[1]])]<-paste(metinput[1],"\t met file name",sep="")

        if(length(metinput)==2){
            inifiles[[2]][grep(" met file name",inifiles[[2]])]<-paste(metinput[2],"\t EPC file name",sep="")
        }}
    
    if(is.null(CO2input)){
        CO2flag=TRUE
        CO2input[1] <-  unlist(strsplit(grep(" CO2 file",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        CO2input[2] <-  unlist(strsplit(grep(" CO2 file",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep(" CO2 file",inifiles[[1]])]<-paste(CO2input[1],"\t CO2 file",sep="")

        if(length(CO2input)==2){
            inifiles[[2]][grep(" CO2 file",inifiles[[2]])]<-paste(CO2input[2],"\t CO2 file",sep="")
        }}
    
    if(is.null(nitinput)){
        nitflag=TRUE
        nitinput[1] <-  unlist(strsplit(grep("N-dep file",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        nitinput[2] <-  unlist(strsplit(grep("N-dep file",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("N-dep file",inifiles[[1]])]<-paste(nitinput[1],"N-dep file",sep="N-dep file")

        if(length(epcinput)==2){
            inifiles[[2]][grep("N-dep file",inifiles[[2]])]<-paste(nitinput[2],"N-dep file",sep="")
        }}

    if(is.null(thininput)){
        thinflag=TRUE
        thininput[1] <-  unlist(strsplit(grep("do THINNING",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        thininput[2] <-  unlist(strsplit(grep("do THINNING",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("do THINNING",inifiles[[1]])]<-paste(thininput[1],"do THINNING",sep="")

        if(length(thininput)==2){
            inifiles[[2]][grep("do THINNING",inifiles[[2]])]<-paste(thininput[2],"do THINNING",sep="")
        }}
    
    if(is.null(plantinput)){
        plantflag=TRUE
        plantinput[1] <-  unlist(strsplit(grep("do PLANTING",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        plantinput[2] <-  unlist(strsplit(grep("do PLANTING",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("do PLANTING",inifiles[[1]])]<-paste(plantinput[1],"do PLANTING",sep="")

        if(length(plantinput)==2){
            inifiles[[2]][grep("do PLANTING",inifiles[[2]])]<-paste(plantinput[2],"do PLANTING",sep="")
        }}

    if(is.null(mowinput)){
        mowflag=TRUE
        mowinput[1] <-  unlist(strsplit(grep("do MOWING",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        mowinput[2] <-  unlist(strsplit(grep("do MOWING",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("do MOWING",inifiles[[1]])]<-paste(mowinput[1],"do MOWING",sep="")

        if(length(mowinput)==2){
            inifiles[[2]][grep("do MOWING",inifiles[[2]])]<-paste(mowinput[2],"do MOWING",sep="")
        }}

    if(is.null(grazinput)){
        grazflag=TRUE
        grazinput[1] <-  unlist(strsplit(grep("do GRAZING",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        grazinput[2] <-  unlist(strsplit(grep("do GRAZING",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("do GRAZING",inifiles[[1]])]<-paste(grazinput[1],"do GRAZING",sep="")

        if(length(grazinput)==2){
            inifiles[[2]][grep("do GRAZING",inifiles[[2]])]<-paste(grazinput[2],"do GRAZING",sep="")
        }}
    
    if(is.null(harvinput)){
        harvflag=TRUE
        harvinput[1] <-  unlist(strsplit(grep("do HARVESTING",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        harvinput[2] <-  unlist(strsplit(grep("do HARVESTING",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("do HARVESTING",inifiles[[1]])]<-paste(harvinput[1],"do HARVESTING",sep="")

        if(length(harvinput)==2){
            inifiles[[2]][grep("do HARVESTING",inifiles[[2]])]<-paste(harvinput[2],"do HARVESTING",sep="")
        }}
    
    if(is.null(plouginput)){
        plougflag=TRUE
        plouginput[1] <-  unlist(strsplit(grep("do PLOUGHING",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        plouginput[2] <-  unlist(strsplit(grep("do PLOUGHING",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("do PLOUGHING",inifiles[[1]])]<-paste(plouginput[1],"do PLOUGHING",sep="")

        if(length(plouginput)==2){
            inifiles[[2]][grep("do PLOUGHING",inifiles[[2]])]<-paste(plouginput[2],"do PLOUGHING",sep="")
        }}
    
    if(is.null(fertinput)){
        fertflag=TRUE
        fertinput[1] <-  unlist(strsplit(grep("do FERTILIZING",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        fertinput[2] <-  unlist(strsplit(grep("do FERTILIZING",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("do FERTILIZING",inifiles[[1]])]<-paste(fertinput[1],"do FERTILIZING",sep="")

        if(length(fertinput)==2){
            inifiles[[2]][grep("do FERTILIZING",inifiles[[2]])]<-paste(fertinput[2],"do FERTILIZING",sep="")
        }}
    
    if(is.null(irrinput)){
        irrflag=TRUE
        irrinput[1] <-  unlist(strsplit(grep("do IRRIGATION",inifiles[[1]],value=TRUE),"[\ \t]"))[1]
        irrinput[2] <-  unlist(strsplit(grep("do IRRIGATION",inifiles[[2]],value=TRUE),"[\ \t]"))[1] 
    } else {
        inifiles[[1]][grep("do IRRIGATION",inifiles[[1]])]<-paste(irrinput[1],"do IRRIGATION",sep="")

        if(length(irrinput)==2){
            inifiles[[2]][grep("do IRRIGATION",inifiles[[2]])]<-paste(irrinput[2],"do IRRIGATION",sep="")
        }}

    c<-grep("DAILY_OUTPUT",inifiles[[2]])+1
    numVar<-as.numeric(unlist(strsplit(inifiles[[2]][c],"[\ \t]"))[1])
    dailyVarCodes<-inifiles[[2]][(c+1):(c+numVar)]
    dailyVarnames<-lapply(dailyVarCodes, function(x) musoMapping(unlist(strsplit(x,"[\ \t]"))[1]))

    c<-grep("ANNUAL_OUTPUT",inifiles[[2]])+1
    numVar<-as.numeric(unlist(strsplit(inifiles[[2]][c],"[\ \t]"))[1])
    annualVarCodes<-inifiles[[2]][(c+1):(c+numVar)]
    annualVarnames<-lapply(annualVarCodes, function(x) musoMapping(unlist(strsplit(x,"[\ \t]"))[1]))
    outputvars<-list(dailyVarnames,annualVarnames)
    
    
    if(is.null(executable)){
        if(Linuxp){
            executable<-paste(inputloc,"muso",sep="")
        } else {
            executable<-paste(inputloc,"muso.exe",sep="")
        }
    } else {        
        file.copy(executable,inputloc)
        if(Linuxp){
            executable<-paste(inputloc,"muso",sep="")
        } else {
            executable<-paste(inputloc,"muso.exe",sep="")
        }
        
    }
5+4
     outputname <- unlist(strsplit(grep("output",grep("prefix",inifiles[[2]],value=TRUE),value=TRUE),"[\ \t]"))[1]
    ##THIS IS AN UGLY SOLUTION, WHICH NEEDS AN UPGRADE!!!
    ## outputname <- unlist(strsplit(grep("prefix for output files",inifiles[[2]],value=TRUE),"[\ \t]"))[1]
    if(is.null(outputname)){
        cat("I cannot find outputname, in your default ini file \n Please make sure that the line wich contains the name alse contains the prefix and the outmut keywords!")
        
}
##    outputname<-unlist(read.table(ininput[2],skip=93,nrows = 1))[1]


    if(is.null(outputloc)){
    ##  outputloc<-paste((rev(rev(unlist(strsplit(outputname,"/")))[-1])),collapse="/")
        outputloc <- dirname(outputname)
    }

    
    
    inputfiles<-c(ininput,epcinput,metinput)
    numdata<-rep(NA,3)
    numyears <-  as.numeric(unlist(strsplit(grep("simulation years",inifiles[[2]],value=TRUE),"[\ \t]"))[1])
    ##    numyears<-unlist(read.table(ininput[2],skip = 14,nrows = 1)[1])
    numvalues <-  as.numeric(unlist(strsplit(grep("number of daily output variables",inifiles[[2]],value=TRUE),"[\ \t]"))[1])
    ## numvalues<-unlist(read.table(ininput[2],skip=102,nrows = 1)[1])
    startyear <- as.numeric(unlist(strsplit(grep("first simulation year",inifiles[[2]],value=TRUE),"[\ \t]"))[1])
    numdata[1]<-numyears*numvalues*365
    numdata[2]<-numyears*numvalues*12
    numdata[3]<-numyears*numvalues

    ##Writing out changed ini-file

    writeLines(inifiles[[1]],ininput[1])
    writeLines(inifiles[[2]],ininput[2])
    
    settings = list(executable = executable,
                    calibrationpar = calibrationpar,
                    outputloc=outputloc,
                    outputnames=outputname,
                    inputloc=inputloc,
                    ininput=ininput,
                    metinput=metinput,
                    epcinput=epcinput,
                    thininput=thininput,
                    CO2input=CO2input,
                    mowinput=mowinput,
                    grazinput=grazinput,
                    harvinput=harvinput,
                    plouginput=plouginput,
                    fertinput=fertinput,
                    irrinput=irrinput,
                    nitinput=nitinput,
                    inputfiles=inputfiles,
                    numdata=numdata,
                    startyear=startyear,
                    numyears=numyears,
                    outputvars=outputvars
                    )
    
    if(!(epcflag&CO2flag&nitflag&thinflag&plantflag&mowflag&grazflag&harvflag&plougflag&fertflag&irrflag)){
        writeLines(inifiles[[1]],ininput[[1]])
        if(epcinput[1]!=epcinput[2]){
            writeLines(inifiles[[2]],ininput[[2]])      
        }
    }
    return(settings)

}

