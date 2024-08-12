#' musoRand
#'
#' This funtion uses the Monte Carlo technique to uniformly sample the parameter space from user defined parameters of the Biome-BGCMuSo model. The sampling algorithm ensures that the parameters are constrained by the model logic which means that parameter dependencies are fully taken into account (parameter dependency means that e.g leaf C:N ratio must be smaller than C:N ratio of litter; more complicated rules apply to the allocation parameters where the allocation fractions to different plant compartments must sum up 1). This function implements a mathematically correct solution to provide uniform distriution for all selected parameters. 
#' @author Roland HOLLOS
#' @param parameters This is a dataframe (heterogeneous data-matrix), where the first column is the name of the parameter, the second is a numeric vector of the rownumbers of the given variable in the input EPC file, and the last two columns describe the minimum and the maximum of the parameter (i.e. the parameter ranges), defining the interval for the randomization.
#' @param constraints This is a matrix wich specify the constrain rules for the sampling. Parameter dependencies are described in the Biome-BGCMuSo User's Guide. Further informations is coming soon. If its set to "without", no constraints are used
#' @param iteration The number of samples for the Monte-Carlo experiment. We propose to use at least 3000 iteration because it is generally fast and it can be subsampled later at any time. 
#' @importFrom limSolve xsample
#' @export

musoRand <- function(parameters, iterations=3000, fileType="epc", sourceFile=NULL, constraints = NULL, burnin = NULL){
    
    if(identical(constraints,"without")){
        index <- parameters[,2]
        randVal <- matrix(nrow=iterations, ncol=nrow(parameters))
        for(i in 1:nrow(parameters)){
            randVal[,i] <- runif(iterations,parameters[i,3],parameters[i,4])
        }
        return(list(INDEX =index, randVal=randVal))
    }


    if(is.null(constraints)){
        constMatrix <- constraints
        constMatrix <- getOption("RMuso_constMatrix")[[fileType]][[as.character(getOption("RMuso_version"))]]
    } else {
        constMatrix <- constraints
    }

    parameters <- parameters[,-1]
    constMatrix <- constMatrix[,-1]

    depTableMaker <- function(constMatrix,parameters){
        parameters <- parameters[order(parameters[,1]),] ## BUG!!!
        selectedRows <- constMatrix[,"INDEX"] %in% parameters[,1]
        # constMatrix[constMatrix[,"INDEX"] %in% parameters[,1],]
        rankList <- rank(constMatrix[selectedRows,2])
        constMatrix[selectedRows,c(5,6)] <- parameters[rankList,c(2,3)]





        logiConstrain <- (constMatrix[,"GROUP"] %in% constMatrix[constMatrix[,"INDEX"] %in% parameters[,1],"GROUP"] &
                          (constMatrix[,"GROUP"]!=0)) | ((constMatrix[,"INDEX"] %in% parameters[,1]) & (constMatrix[,"GROUP"] == 0))
        constMatrix <- constMatrix[logiConstrain,]
        constMatrix <- constMatrix[order(apply(constMatrix[,7:8],1,function(x){x[1]/10+abs(x[2])})),]



        paragroups <- unique(constMatrix[constMatrix[,"GROUP"] != 0,"GROUP"])
        missingMembers <- list()
        for(group in paragroups){
            groupMembers <- constMatrix[constMatrix[,"GROUP"] == group,"INDEX"]
            missingMemberElems <- groupMembers[is.na(match(groupMembers,parameters[,1]))]
            if(length(missingMemberElems) > 0){
                missingMembers$indices  <- c(missingMembers$indices,
                                             match(missingMemberElems,constMatrix[,"INDEX"]))
                if(is.null(sourceFile)){
                    stop(sprintf("All group members of the group (%s) have to be in parameters if sourceFile not(epc file)",group))
                }

                missingMembers$values <- c(missingMembers$values,
                                           suppressWarnings(readValuesFromFile(sourceFile,missingMemberElems))) 
            }
        }

        list(depTable=constMatrix,missingMembers=missingMembers)
    }
    # browser()
    genMat0 <- function(dep, missingIndices){
        numberOfVariable <- nrow(dep)
        if(length(missingMembers) != 0){
            dep <- dep[-missingIndices,]
        }
        G <- rbind(diag(numberOfVariable), -1*diag(numberOfVariable))
        h <- c(dependences[,5], -1*dependences[,6])
        return(list(G=G,h=h))
    }

    genMat1 <- function(dep, N){

        ## Range <- sapply(list(min,max),function(x){
        ##   x(as.numeric(rownames(dep)))
        ## }) It is more elegant, more general, but slower
        Range <- (function(x){
                      c(min(x), max(x))
                          })(as.numeric(dep[,"rowIndex"]))

        numberOfVariables <- nrow(dep)
        G<- -1*diag(numberOfVariables)

        for(i in 1:numberOfVariables){
            if(dep[i,4]!=0){
                G[i,dep[i,4]] <- 1
            }

        }
        # browser()
        G<-G[dep[,4]!=0,]

        if(is.null(nrow(G))){
            G<-t(as.matrix(G))
        }
        numRowsInG <- nrow(G)
        if(Range[1]==1){
            G<-cbind(G,matrix(ncol=(N-Range[2]),nrow=numRowsInG,data=0))
        } else{
            if(Range[2]==N){
                G<-cbind(matrix(ncol=(Range[1]-1),nrow=numRowsInG,data=0),G)
            } else {
                G <- cbind(matrix(ncol=(Range[1]-1),
                                  nrow=numRowsInG,data=0),
                           G,
                           matrix(ncol=(N-Range[2]),
                                  nrow=numRowsInG,data=0))
            }
        }
        return(list(G=-1*G,h=-1*rep(0,nrow(G))))
    }

    genMat2 <- function(dep, N){
        G <- rep(1,nrow(dep))

        Range <- (function(x){
                      c(min(x), max(x))
                          })(as.numeric(dep[,"rowIndex"]))

        if(Range[1]==1){
            G<-c(G, numeric(N-Range[2]))
        } else{
            if(Range[2]==N){
                G<-c(numeric(Range[1]-1), G)
            } else {
                G <- c(numeric(Range[1]-1), G, numeric(N-Range[2]))
            }
        }

        G <- t(matrix(sign(dep[2,4])*G))
        h <- abs(dep[1,4])
        if(dep[1,"TYPE"]==2){ # This is not needed, I'll have to remove the if part, and keep the content
            G <- G*(-1)
            h <- h*(-1)
        }

        return(list(G=G,h=h))
    }

    genMat3 <- function(dep, N){
        Range <- (function(x){
                      c(min(x), max(x))
                          })(as.numeric(dep[,"rowIndex"]))

        E <- rep(1,nrow(dep))

        if(Range[1]==1){
            E<-c(E, numeric(N-Range[2]))
        } else{
            if(Range[2]==N){
                E<-c(numeric(Range[1]-1), E)
            } else {
                E <- c(numeric(Range[1]-1), E, numeric(N-Range[2]))
            }
        }


        E <- t(matrix(E))
        f <- dep[1,4]
        return(list(E=E,f=f))
    }


    applyRandTypeG <- function(dep,N){
        type <- unique(dep[,"TYPE"])
        minR <- min(dep[,"rowIndex"])
        maxR <- max(dep[,"rowIndex"])
        switch(type,
               invisible(Gh <- genMat1(dep, N)),
               invisible(Gh <- genMat2(dep, N)))
        return(Gh)
    }

    applyRandTypeE <- function(dep,N){
        type <- unique(dep[,"TYPE"])
        minR <- min(dep[,"rowIndex"])
        maxR <- max(dep[,"rowIndex"])
        switch(-type,
               stop("Not implemented yet"),
               stop("Not implemented yet"),
               invisible(Ef <- genMat3(dep, N)))
        return(Ef)
    }

    dependences <- depTableMaker(constMatrix, parameters)
    missingMembers <- dependences$missingMembers
    dependences <- dependences$depTable
    dependences <- cbind(dependences,1:nrow(dependences))
    colnames(dependences)[ncol(dependences)] <- "rowIndex"
    # browser()
    numberOfVariable <- nrow(dependences)
    nonZeroDeps<-dependences[dependences[,"TYPE"]!=0,]
    if(nrow(nonZeroDeps)!=0){
        splitedDeps<- split(nonZeroDeps,nonZeroDeps[,"GROUP"])
        Gh <- list()
        Ef <- list()

        for(i in 1:length(splitedDeps)){
            print(splitedDeps[[i]][1,"TYPE"])
            if(splitedDeps[[i]][1,"TYPE"]>0){
                Gh[[i]]<-applyRandTypeG(splitedDeps[[i]],nrow(dependences))
            } else {
                Ef[[i]] <- applyRandTypeE(splitedDeps[[i]],nrow(dependences))
            }
        }

        Gh0<- genMat0(dependences, missingMembers$indices)
        G <- do.call(rbind,lapply(Gh,function(x){x$G}))
        G<- rbind(Gh0$G,G)
        h <- do.call(c,lapply(Gh,function(x){x$h}))
        h <- c(Gh0$h,h)
        E <- do.call(rbind,lapply(Ef,function(x){x$E}))
        f <- do.call(c,lapply(Ef,function(x){x$f}))
        # browser()
        if(length(missingMembers$indices)!=0){
            Ep <- matrix(data=0,ncol=numberOfVariable,nrow=length(missingMembers$indices))
            Ep[1:length(missingMembers$indices),missingMembers$indices] <- 1
            E <- rbind(E,Ep)
            f <- c(f,missingMembers$values)
        }

        randVal <- suppressWarnings(limSolve::xsample(G=G,H=h,E=E,F=f,burninlength=burnin, iter = iterations))$X
    } else{
        Gh0<-genMat0(dependences,NULL)
        randVal <- suppressWarnings(xsample(G=Gh0$G,H=Gh0$h, iter = iterations))$X
    }
    if(length(missingMembers$indices)!=0){
        return(list(INDEX = dependences$INDEX[-missingMembers$indices],
                    randVal=randVal[,-missingMembers$indices]))
    }
    results <- list(INDEX =dependences$INDEX, randVal=randVal)
    return(results)
}
