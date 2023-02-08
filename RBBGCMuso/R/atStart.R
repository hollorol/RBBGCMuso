
printback <- function(text,color){
  cat(sprintf("\033[%dm%s\033[0m\n",color,text))
}
colorText <- function(text,color){
  sprintf("\033[%dm%s\033[0m",color,text)
}

.onLoad <- function(libname,pkgname){
    RMuso_version <- 7
    cat(sprintf('This is RBBGCMuso version 1.0\nDefault Biome-BGCMuSo version: %d\n',
                RMuso_version))
    cat(sprintf('For quick tutorial visit %s\n', colorText('https://github.com/hollorol/RBBGCMuso',104)))
    cat(sprintf('For help, issue the command: %s\n',colorText('help(package="RBBGCMuso")',104)))
    cat(sprintf('In order to get a sample simulation package use the %s command\n',colorText('copyMusoExample()',104)))
    RMuso_constMatrix <- list(epc=NULL,soil=NULL) 
    RMuso_varTable <- list()
    #___________________________
    sapply(names(RMuso_constMatrix),function(fType){
        sapply(list.files(path=system.file("data",package="RBBGCMuso"),
                          pattern=sprintf("^%sConstMatrix\\d\\.json$",fType), full.names=TRUE),function(fName){
            constMatrix <- jsonlite::read_json(fName,simplifyVector = TRUE)[,c(1,2,3,4,9,5,6,7,8)]
            version <- gsub(".*(\\d)\\.json","\\1",fName)
            RMuso_constMatrix[[fType]][[version]] <<- constMatrix
        })
        RMuso_constMatrix
        # RMuso_constMatrix <<- RMuso_constMatrix 
    })


        sapply(list.files(path=system.file("data",package="RBBGCMuso"),
                          pattern="^varTable\\d\\.json$", full.names=TRUE),function(fName){
            varTable <- jsonlite::read_json(fName,simplifyVector = TRUE)
            version <- gsub(".*(\\d)\\.json","\\1",fName)
            RMuso_varTable[[version]] <<- varTable
        })

    RMuso_depTree<- read.csv(file.path(system.file("data",package="RBBGCMuso"),"depTree.csv"), stringsAsFactors=FALSE)


    options(RMuso_version=RMuso_version,
            RMuso_constMatrix=RMuso_constMatrix,
            RMuso_varTable=RMuso_varTable,
            RMuso_depTree=RMuso_depTree
    )
    # getOption("RMuso_constMatrix")$soil[[as.character(getOption("RMuso_version"))]]
}
