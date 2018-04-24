musoSensi <- function(monteCarloFile = NULL,
                     parameters,
                     settings = NULL,
                     inputDir = "./",
                     outLoc = "./calib",
                     iterations = 30,
                     preTag = "mount-",
                     outputType = "moreCsv",
                     fun = mean,
                     varIndex = 1,
                     outputFile = "sensitivity.csv",
                     plotName = "sensitivity.jpg"){

    if(is.null(monteCarloFile)){
        M <- musoMonte(parameters = parameters,
                      settings = settings,
                      inputDir = inputDir,
                      outLoc = outLoc,
                      iterations = iterations,
                      preTag = preTag,
                      outputType = outputType,
                      fun = fun,
                      varIndex = varIndex
                      )
        npar <- ncol(M)-1
        M %<>%
            tbl_df() %>%
            filter(.,!is.na(y)) %>%
            as.data.frame()
        y <- M[,(npar+1)]
        M <- apply(M[,1:npar],2,function(x){x-mean(x)})
        w <- lm(y~M)$coefficients[-1]
        Sv <- apply(M,2,var)
        overalVar <- sum(Sv^2*w^2)
        S=numeric(npar)
        for(i in 1:npar){
            S[i] <- ((w[i]^2*Sv[i]^2)/overalVar)*100
        }

        jpg(plotName)
        barplot(S)
        dev.off()
        
        write.csv(file = outputFile, x = S)
        barplot(S)
        return(S)
    } else {
        M <- read.csv(monteCarloFile)
        npar <- ncol(M)-1
        M %<>%
            tbl_df() %>%
            filter(.,!is.na(y)) %>%
            as.data.frame()
        y <- M[,(npar+1)]
        M <- apply(M[,1:npar],2,function(x){x-mean(x)})
        w <- lm(y~M)$coefficients[-1]
        Sv <- apply(M,2,var)
        overalVar <- sum(Sv^2*w^2)
        S=numeric(npar)
        for(i in 1:npar){
            S[i] <- ((w[i]^2*Sv[i]^2)/overalVar)*100
        }

        jpg(plotName)
        barplot(S)
        dev.off()
        
        write.csv(file = outputFile, x = S)
        barplot(S)
        return(S)        
    }
}
