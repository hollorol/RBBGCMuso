zero_var <- function(m){
    apply(m,2, function(v){
        var(v) != 0
    })

}


glue <- function(results="result.csv",res_r="results.RDS",output ="gplot.pdf",epcname="maize_glue.epc"){
    res <- read.csv(results)[-1]
    res <- res[-1,]
    colnames(res)
    non_zero <- res[,1:(ncol(res)-4)]
    colnames(non_zero) <- colnames(res)[1:(ncol(res)-4)]
    impvars <- zero_var(non_zero)
    nonzero <- non_zero[,impvars]
    likelihoods <- res[,(ncol(res)-3)]
    rmse <- res[,(ncol(res)-2)]
    const <- res$Const
    namess <- gsub("__.*","",colnames(nonzero))




    likelihoods <- likelihoods[res$Const==1]
    goods <- res[res$Const==1,]
    medlik <- median(likelihoods[likelihoods >= quantile(likelihoods,0.95)])
    medlik_place <- which.min(abs(likelihoods - medlik))
    parameters <- readRDS(res_r)
    glue_opt <- goods[medlik_place, 1:(ncol(res)-4)][impvars]
    nonka <- goods[likelihoods >= quantile(likelihoods,0.95),1:(ncol(res)-4)]
    med_opt <- apply(nonka,2,median)[impvars]
    # med_opt <- apply(nonka,2,mean)[impvars]
    ml_opt <- goods[which.max(likelihoods),1:(ncol(res)-4)][impvars]

    calibrationPar <- parameters$calibrationPar[impvars]
    changemulline(src="maize.epc", calibrationPar = calibrationPar, contents=glue_opt, outFiles = epcname)
    changemulline(src="maize.epc", calibrationPar = calibrationPar, contents=med_opt, outFiles = "maize_median.epc")
    changemulline(src="maize.epc", calibrationPar = calibrationPar, contents=ml_opt, outFiles = "maize_ml.epc")



    print(output)
    pdf(output)
    for(i in 1:ncol(nonzero)){
        plot(nonzero[,i],res[,(ncol(res)-3)],main="",col="lightgray", pch=20, cex=0.4, xlab=namess[i],ylab="logLikelihood")
        points(nonzero[const==1,i],res[const==1,(ncol(res)-3)],pch=20, cex=0.6, col="red",type="p",xlab=namess[i],ylab="logLikelihood")
        abline(v=glue_opt[i],col="green")
        abline(v=med_opt[i],col="blue")
        abline(v=ml_opt[i],col="black")
    }

    dev.off()
}

