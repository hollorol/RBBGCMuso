library('RBBGCMuso')

## This function can be modified, it is a generator function,
## which works like this:
## meanmax_gen(4): meanmax for for the 4th year.

meanmax_gen <- function(selectedYears=NULL){

    function(x){
        years <- rep(1:(as.integer(length(x)/365)), each=365)
        if(!is.null(selectedYears)){
            x <- x[years %in% selectedYears]
            years <- years[years %in% selectedYears]
        }
        mean(tapply(x, years,function(year){
                        max(year)
        }))
    }
}

musoSensi(iteration=100,varIndex=2,fun=meanmax_gen())

settings <- setupMuso(iniInput=c(spinup="n.ini",normal="n.ini"))
# I would like to do the sensitivity only for maize.
settings$epcInput["normal"] <- "maize.epc"
## Select the 4th,5th year only for sensitivity ,assuming that these are the maize years
## The sensitivity will be calculated for mean of annual maximum values of LAI(varIndex = 2)
musoSensi(iteration=100,varIndex=2,fun=meanmax_gen(c(4,5)))
