.onLoad <- function(libname,pkgname){
    print("This is RBBGCMuso version 0.7")
    RMuso_version <- 6 
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


    options(RMuso_version=RMuso_version,
            RMuso_constMatrix=RMuso_constMatrix,
            RMuso_varTable=RMuso_varTable)
    # getOption("RMuso_constMatrix")$soil[[as.character(getOption("RMuso_version"))]]
}
