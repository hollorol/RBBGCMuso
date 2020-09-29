library(parallel)
library('future')
plan(multiprocess)
library('RBBGCMuso')

a <- tempdir()
setwd(a)
file.copy(from="~/R/x86_64-pc-linux-gnu-library/3.5/RBBGCMuso/examples/hhs",to="./", recursive = TRUE)
setwd("hhs")
list.files()

settings <- setupMuso()
setupMuso()
settings$outputLoc


copyToThreadDirs <- function(prefix="thread", numcores=parallel::detectCores()-1, runDir="."){
    dir.create(file.path(runDir,prefix), showWarnings=TRUE)
    fileNames <- grep("^thread.*", list.files(runDir), value=TRUE, invert=TRUE)
    invisible(sapply(1:numcores,function(corenum){
                threadDir <- file.path(runDir,prefix,paste0(prefix,"_",corenum))
                dir.create(threadDir, showWarnings=FALSE)
                file.copy(from=fileNames,to=threadDir, overwrite=FALSE)
    }))
}

copyToThreadDirs()
unlink("thread", recursive=TRUE)

procFun <- function(index){
    progressState <- tempfile(pattern=paste("thread",index,sep="-", tmpdir="./"))
    for(i in 1:100){
       Sys.sleep(1) 
        writeLines(as.character(i),paste("thread",index,sep="-"))
    }
}

futu <- vector(mode="list", length=4) 
names(futu) <- 1:4
futu


getProgress <- function(){
    threadfiles <- list.files(pattern="thread*")
    if(length(threadfiles)==0){
        return(0)
    } else {
        sum(sapply(threadfiles, function(x){
                       partRes <- readLines(x)
                       if(length(partRes)==0){
                           return(0)
                       } else {
                           return(as.numeric(partRes))
                       }

    }))

    }
}

getProgress()
futu
list.files()
readLines("threadi-1")
procFun(8)
file.remove(pattern="thread*")
file.remove((list.files(pattern="thread*")))


wachProgress <- function(){
    progress <- 0
    while(progress < 400){
        Sys.sleep(1)
        progress <- getProgress()
        print(paste(as.numeric(progress)/400*100,"%"))
    }
}

for(i in 1:4){
    futu[[as.character(i)]] <- future({procFun(i)})
}
lapply(1:4,function(i) future({procFun(i)}))
pb <- txtProgressBar(min=0,max=400,style=3)
progress <- 0
while(progress < 400){
    Sys.sleep(1)
    progress <- getProgress()
    setTxtProgressBar(pb,as.numeric(progress))
}
close(pb)
