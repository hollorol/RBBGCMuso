#' musoRand
#'
#' This funtion samples uniformly from the choosen parameters of the BiomeBGC-Muso model, which parameters are constrained by the model logic.
#' @author Roland Hollos
#' @param parameters This is a dataframe (heterogen data-matrix), which first column is the name of the parameters, the second is a numeric vector of the rownumbers of the given variable in the input-file, the last two column consist the endpont of the parameter-ranges, where the parameters will be randomized.
#' @param constrains This is a matrics wich specify the constrain rules for the sampling. Further informations coming son.
#' @param iteration The number of sample-s. It is adviced to use at least 3000 iteration, because it is generally fast and it can be subsampled later at any time. 
#' @export

musoRand <- function(parameters, constrains = NULL, iterations=3000){

    if(!is.null(constrains)){
        constMatrix <- constrains
    }
    
    parameters <- parameters[,-1]
    constMatrix <- constMatrix[,-1]
    
    depTableMaker <- function(constMatrix,parameters){
	parameters <- parameters[order(parameters[,1]),]
	constMatrix[constMatrix[,"INDEX"] %in% parameters[,1],c(5,6)]<-parameters[,c(2,3)]
	logiConstrain <- (constMatrix[,"GROUP"] %in% constMatrix[constMatrix[,"INDEX"] %in% parameters[,1],"GROUP"] &
			  (constMatrix[,"GROUP"]!=0)) | ((constMatrix[,"INDEX"] %in% parameters[,1]) & (constMatrix[,"GROUP"] == 0))
	constMatrix<-constMatrix[logiConstrain,]
	constMatrix <- constMatrix[order(apply(constMatrix[,7:8],1,function(x){x[1]/10+abs(x[2])})),]
	constMatrix
    }

    genMat0 <- function(dep){
	numberOfVariable <- nrow(dep)
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

	G<-G[dep[,4]!=0,]

	if(Range[1]==1){
	    G<-cbind(G,matrix(ncol=(N-Range[2]),nrow=nrow(G),data=0))
	} else{
	    if(Range[2]==N){
		G<-cbind(matrix(ncol=(Range[1]-1),nrow=nrow(G),data=0),G)
	    } else {
		G <- cbind(matrix(ncol=(Range[1]-1),nrow=nrow(G),data=0),G,matrix(ncol=(N-Range[2]),nrow=nrow(G),data=0))
	    }
	}
	return(list(G=G,h=rep(0,nrow(G))))
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
    dependences <- cbind(dependences,1:nrow(dependences))
    colnames(dependences)[ncol(dependences)] <- "rowIndex"
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
        
        Gh0<- genMat0(dependences)
        G <- do.call(rbind,lapply(Gh,function(x){x$G}))
        G<- rbind(Gh0$G,G)
        h <- do.call(c,lapply(Gh,function(x){x$h}))
        h <- c(Gh0$h,h)
        E <- do.call(rbind,lapply(Ef,function(x){x$E}))
        f <- do.call(c,lapply(Ef,function(x){x$f}))
        randVal <- suppressWarnings(limSolve::xsample(G=G,H=h,E=E,F=f,iter = iterations))$X
    } else{
        Gh0<-genMat0(dependences)
        randVal <- suppressWarnings(limSolve::xsample(G=Gh0$G,H=Gh0$h, iter = iterations))$X
    }
    
    results <- list(INDEX =dependences$INDEX, randVal=randVal)
    return(results)
}
