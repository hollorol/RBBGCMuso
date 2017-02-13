#' This runs the BBGC-MuSo model
#' @author Roland Hollós
#' @param calibrationpar vector with line numbers
#' @return No return, outputs are written to file
setup <- function(executable=NULL,
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
    }
    
    inp <- unlist(strsplit(inputloc,"")) #This is the charactervector of the given imput location

    if(inp[length(inp)]!="/"){
        inp<-c(inp,"/")
        inputloc <- paste(inp,collapse = "")
        rm(inp)
    }# If inp not ends in / paste one at the end, then make a string, that will be the new inputloc

    ##Example: "a/b/c ==> a/b/c/"
    
    if(is.null(outputloc)){
        outputloc<-inputloc
    }

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

    outputname <- unlist(strsplit(grep("prefix for output files",inifiles[[2]],value=TRUE),"[\ \t]"))[1]
##    outputname<-unlist(read.table(ininput[2],skip=93,nrows = 1))[1]
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

