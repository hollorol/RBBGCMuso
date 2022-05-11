library(rpart)
library(rpart.plot)

accuracy <- function(x,rp){
    # Accuracy = (TP + TN)/(TP + TN + FP + FP)
    # TP: True Positive
    # TN: True Negative
    # FP: False Positive
    # FN: False Negative

    predicted <- rpart.predict(rp,type = "vector")
    predicted[predicted==1] <- 0
    predicted[predicted==2] <- 1
    (sum(x*predicted) +  sum((x + predicted) == 0)) / length(x)
}

zero_var <- function(m){
    apply(m,2, function(v){
        var(v) != 0
    })

}

decbin <- function(decnum){
    if(decnum < 2){
        return(decnum)
    }
    c(decbin((decnum %/% 2)),decnum %% 2)
}

decpad <- function(decnum,len){
    binrep <- decbin(decnum)
    c(rep(0,len-length(binrep)),binrep)
}


tree_per_const <- function(results="result.csv",output ="tree_per_const.pdf",
                           parameters_file="Martonvasar_maize.set"){
    varname <-readLines(parameters_file)[1]
    parameters <- read.csv(parameters_file,skip=1,stringsAsFactors=FALSE)


    results <- read.csv(results, stringsAsFactors=FALSE)
    # likelihoods <- results[,ncol(results)-3]
    # results <- results[likelihoods>=quantile(likelihoods,0.95),]
    len <- round(log(max(results$failType),2))
    failTypes <- do.call(rbind,lapply(results$failType,function(x){decpad(x,len)}))
    sapply(1:len, function(const){
        nonzero <- results[,1:(ncol(results)-4)]
        nonzero <- nonzero[,-1]
        nonzero <- nonzero[,zero_var(nonzero)]
        colnames(nonzero) <- gsub("__.*","",colnames(nonzero))
        constraint <- failTypes[,const]
        baseTable <- cbind.data.frame(nonzero,constraint = as.factor(constraint))
        tryCatch({
            rp <- rpart(constraint ~ .,data = baseTable)
            accuracy(constraint, rp)
        }, error = function(e){NA})
    })
}

results <- matrix(nrow=10,ncol=4)
row.names(results) <- 1:10
colnames(results) <- c("Harvest Index", "LAI", "Root depth in phen. 5", "Flowering date")
for(i in 1:10){
    setwd(as.character(i))
    results[i,] <- tree_per_const(parameters_file="Martonvasar_maize_after_tree.set")
    setwd("../")
}
results
