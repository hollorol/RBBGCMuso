basePackages <- c("dplyr","digest","ggplot2","shiny","latex2expr",
                  "magrittr","tibble","tidyr","limSolve")
install.packages(basePackages)
install.packages("https://github.com/hollorol/RBBGCMuso/raw/master/RBBGCMuso_0.6.0.1-0.zip", repos = NULL, type = "win.binary")
rm(basePackages)
