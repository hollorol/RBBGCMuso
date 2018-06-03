## runModell <- function(executable,)
## {
    

##     if(silent){#silenc mode
##         if(Linuxp){
##             #In this case, in linux machines
##             tryCatch(system(paste(executable,iniInput[1],"> /dev/null",sep=" ")),
##                    error= function (e){stop("Cannot run the modell-check the executable!")})
##         } else {
##             #In windows machines there is a show.output.on.console option
##             tryCatch(system(paste(executable,iniInput[1],sep=" "),show.output.on.console = FALSE),
##                      error= function (e){stop("Cannot run the modell-check the executable!")})
##         }
        
##     } else {
##         system(paste(executable,iniInput[1],sep=" "))
##     }


## }
