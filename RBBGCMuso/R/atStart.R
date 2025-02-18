
printback <- function(text,color){
  cat(sprintf("\033[%dm%s\033[0m\n",color,text))
}
colorText <- function(text,color){
  sprintf("\033[%dm%s\033[0m",color,text)
}

colorTextBg <- function(text, bg_color, fg_color = 97) {
    sprintf("\033[%s;%sm%s\033[0m", as.character(fg_color), as.character(bg_color), text)
}

# I can't believe this is so complicated
plainText <- function(text) {
  sprintf("\033[39m%s\033[0m", text)
}
# I cannot 
styledText <- function(text, style_code) {
  sprintf("\033[%s;39m%s\033[0m", style_code, text)
}

# PROBABLY PLAINTEXT FUNCTION IS NOT EVEN NEEDED ANYMORE, I WAS JUST BAMBOOZLED BY THE STD.ERR MAKING MY TEXT RED THAT BOZO

.onLoad <- function(libname,pkgname){
    RMuso_version <- 7

    dark_blue_bg = 44
    red_bg = 41
    
    # ================== #
    #   Package Header   #
    # ================== #
  cat(sprintf("%s\n", plainText("=========================================")))
  cat(sprintf("%s\n", plainText("            RBBGCMuso Package            ")))
  cat(sprintf("%s\n", plainText("=========================================")))
  
  # Package info
  cat(sprintf("%s\n", plainText(sprintf("RBBGCMuso version: 1.0  |  Default Biome-BGCMuSo version: %d", RMuso_version))))
  cat(sprintf("%s\n", plainText("-----------------------------------------")))
  
  # Documentation
  cat(sprintf("%s\n", plainText("📖 Documentation & Tutorials: ")))
  cat(sprintf("%s\n", paste0(plainText("- Quick Tutorial: "), 
                              colorTextBg("https://github.com/hollorol/RBBGCMuso", dark_blue_bg))))
  cat(sprintf("%s\n", paste0(plainText("- Help: "), 
                              colorTextBg('help(package="RBBGCMuso")', dark_blue_bg))))
  cat(sprintf("%s\n", paste0(plainText("- Sample data: "), 
                              colorTextBg("copyMusoExampleTo()", dark_blue_bg))))
  cat(sprintf("%s\n", plainText("-----------------------------------------")))
  
  # Warning Section
  cat(sprintf("%s\n", paste0("⚠️ ", colorTextBg("IMPORTANT", red_bg), " ⚠️")))
  cat(sprintf("%s\n", plainText("Certain functions of this package can modify the following files:")))
  cat(sprintf("%s\n", plainText(paste0("- ", styledText(".ini", "1"),  # 1 is bold, 3 is italic
                                         plainText(" "), # there was a comma here once
                                         styledText(".epc", "1"), 
                                         plainText(" "), # here too
                                         styledText(".soil", "1") 
                                         )))) # I'm pyhsically dying
  cat(sprintf("%s\n", colorTextBg("It is advised to always maintain a backup of them!", red_bg)), file = stderr())
    
    # OLD DESIGN
    #cat(sprintf('This is RBBGCMuso version 1.0\nDefault Biome-BGCMuSo version: %d\n',
    #            RMuso_version))
    #cat(sprintf('For quick tutorial visit %s\n', colorText('https://github.com/hollorol/RBBGCMuso',44)))
    #cat(sprintf('For help, issue the command: %s\n',colorText('help(package="RBBGCMuso")',44)))
    #cat(sprintf('In order to get a sample simulation package use the %s command\n',colorText('copyMusoExampleTo()',44)))
    #cat(sprintf('Certain functions of the package can alter your ini, epc, soil files. %s', 
    #            colorText('It is advised to always keep a backup of them!',41)))
    
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
