install.packages(c("dplyr","digest","ggplot2","shiny","latex2expr",
                   "magrittr","tibble","tidyr","limSolve","rmarkdown"))
.RBBGCMusoVersion <- gsub("Version\\:\\s","",grep("Version",readLines("https://github.com/hollorol/RBBGCMuso/raw/master/RBBGCMuso/DESCRIPTION"),value=TRUE))
install.packages(paste0("https://github.com/hollorol/RBBGCMuso/raw/master/RBBGCMuso_",.RBBGCMusoVersion,".zip"), repos = NULL, type = "win.binary")
