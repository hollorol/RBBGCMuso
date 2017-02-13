plotMuso <- function(settings,
                     variable,
                     ##compare,
                     ##plotname,
                     parameters="ECOPHYS",
                     timee="d",
                     silent=TRUE,
                     debugging=FALSE,
                     keepEpc=FALSE,
                     logfilename=NULL,
                     export=FALSE){

    
    musoData <- rungetMuso(settings=settings,
                           silent=silent,
                           timee=timee,
                           parameters=parameters,
                           debugging=debugging,
                           keepEpc=keepEpc,
                           logfilename=logfilename,
                           export=export) 

    xlab_muso<- switch(timee, "d"="days","y"="years","m"=months)
    numVari <- ncol(musoData)
    
    if(is.numeric(variable)){
    if((variable>numVari)|(variable<1)){
        return(print(paste("The variable parameter must be between 1 and ",numVari)))
    }


    plot(musoData[,variable],pch=20,col = "dark blue",xlab=xlab_muso,ylab=colnames(musoData)[variable])
    } else {
        if(variable=="all"){
            
            musoData  <- rbind((1:ncol(musoData)),musoData) #creating the column indexes
            par(mfrow = c(2,numVari/2))

            apply(musoData, 2, function(x) plot(x[2:length(x)],pch=20,col="dark blue",xlab=xlab_muso,ylab = colnames(musoData)[x[1]]))
            return(print("Everything was Ok. ;)"))
        } else {
             return(print("The variable option is the coloumn number of the output data-matrix, so it must be numeric, of if you want to plot the whole data matrix set it \"all\""))
        }

        
    }

    


}
