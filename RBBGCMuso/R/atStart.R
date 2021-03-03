.onLoad <- function(libname, pkgname){
    print(paste0("This is RBBGCMuso version ", packageVersion("RBBGCMuso")))
    RMuso_version <- "latest" 

    versionDir <- system.file("versionSpecifiers", package = "RBBGCMuso")
    versions <- list.dirs(versionDir, full.names = FALSE, recursive = FALSE) 
    res <- lapply(versions, function(version){
    
        constFiles <- list.files(file.path(versionDir,version,"constraints"), full.names=TRUE)

        if(length(constFiles) == 0){
            return(NULL)
        }

        atomFile <-  file.path(versionDir, version, "atoms.json")
        varTableFile <-  file.path(versionDir, version, "varTable.json")
        fileDepFile <- file.path(versionDir, version, "fileDeps.csv")

        if(!all(file.exists(c(atomFile,varTableFile, fileDepFile)))){
            return(NULL)
        }

        
        constraints <- lapply(constFiles, function(constraint){
            jsonlite::read_json(constraint,simplifyVector = TRUE)[,c(1,2,3,4,9,5,6,7,8)]
        })

        names(constraints) <- tools::file_path_sans_ext(basename(constFiles))
        varTable <- jsonlite::read_json(varTableFile, simplifyVector = TRUE)
        atoms <-  jsonlite::read_json(atomFile)
        fileDeps <- read.csv(fileDepFile, stringsAsFactor=FALSE) 
        list(varTable = varTable, constraints = constraints, atomList = atoms, fileDeps = fileDeps)
    })

    names(res) <- versions
    options(RMuso_versionVars=res)
}
