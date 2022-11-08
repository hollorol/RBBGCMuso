library(rpart)
library(rpart.plot)
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
    pdf(output)
    sapply(1:len, function(const){
        nonzero <- results[,1:(ncol(results)-4)]
        nonzero <- nonzero[,-1]
        nonzero <- nonzero[,zero_var(nonzero)]
        colnames(nonzero) <- gsub("__.*","",colnames(nonzero))
        constraint <- failTypes[,const]
        baseTable <- cbind.data.frame(nonzero,constraint = as.factor(constraint))
        try({
            rp = rpart(constraint ~ .,data = baseTable)
        })
        try({
            parameters <<- update_parameters_based_on_tree(rp, parameters)
        })
        try({
            rpart.plot(rp)
        })


    })
    dev.off()
    outname <- paste0(tools::file_path_sans_ext(parameters_file),"_after_tree.",
                      tools::file_ext(parameters_file))
    writeLines(varname,outname)
    write.table(parameters,outname,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)
}
update_parameters_based_on_tree <- function(rp, parameters){
    frm <- rp$frame
    nodes <- labels(rp)
    names(nodes) <- row.names(frm)
    node <- get_start_node(frm)
    parameters <- change_parameters_on_node(nodes,node,parameters)
    while(node !=1){
       node <- get_parent_node(node) 
        if(node == 1){
            break()
        }
       parameters <- change_parameters_on_node(nodes,node,parameters)
    }
    parameters
}




parse_rule_row <- function(string){

  rule_row  <- regmatches(string,regexec("([a-zA-Z_0-9]+)([>=< ]+)(.*)",string,perl=TRUE))[[1]][-1]
    if(rule_row[2] == ">="){
        rule_num <- 1
    } else {
        rule_num <- 2
    }
    rule <- list(c(rule_num,as.numeric(rule_row[3])))
    names(rule) <- rule_row[1]
    return(rule)
}

get_start_node <- function(frm){
    nfrm <- frm[frm$yval == 2,]
    nfrm <- nfrm[nfrm[,"var"] == "<leaf>",]
    pot_start <- as.numeric(row.names(nfrm))[which.max(nfrm$n)]
    pot_start
}

get_parent_node <- function(node_id){
    as.integer(node_id/2)
}

change_parameters_on_node <- function(nodes,node,parameters2){
    crule <- parse_rule_row(nodes[as.character(node)])
    minmax <- unlist(parameters2[parameters2[,1] == names(crule),c(3,4)])

    if(crule[[1]][1] == 1){
        if(minmax[1]<=crule[[1]][2]){
            minmax[1] <- crule[[1]][2]
            if(minmax[1] <= minmax[2]){
                parameters2[parameters2[,1] == names(crule),c(3,4)] <- minmax
            } else {
                write(sprintf("WARNING: %s's  minimum(%s) > maximum(%s)", parameters2[,1],
                                                   minmax[1], minmax[2]),  "errorlog.txt", append=TRUE)
            }
        }
    } else {
        if(minmax[2]>=crule[[1]][2]){
            minmax[2] <- crule[[1]][2]
            if(minmax[1] <= minmax[2]){
                parameters2[parameters2[,1] == names(crule),c(3,4)] <- minmax
            } else {
                write(sprintf("WARNING: %s's  minimum(%s) > maximum(%s)", parameters2[,1],
                                                   minmax[1], minmax[2]),  "errorlog.txt", append=TRUE)
            }
     }
    }
    parameters2
}


