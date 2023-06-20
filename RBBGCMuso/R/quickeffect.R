#' musoQuickEffect
#'
#' This function changes a chosen parameter from the INI or from the ecophysiological constants file (EPC) within a predefined range (defined by the user), and visualizes the effect of the change on the selected output variable. The user has to specify the parameter, the interval for the parameter effect test, and the number of steps. This function focuses only on one parameter. The so-called paramSweep function can manipulate multiple INI/EPC parameters and visualize the results. 
#' @author Roland HOLLOS
#' @param settings RBBGCMuso uses variables that define the entire simulation environment. Those environment variables include the name of the INI files, the name of the meteorology files, the path to the model executable and its file name, the entire output list, the entire output variable matrix, the dependency rules for the EPC parameters etc. Using the runMuso function RBBGCMuso can automatically create those environment variables by inspecting the files in the working directory (this happens through the setupMuso function). It means that by default model setup is performed automatically in the background and the user has nothing to do. With this settings parameter we can force runMuso to skip automatic environment setup as we provide the environment settings to runMuso. In a typical situation the user can skip this option.
#' @param startVal The initial value of the given parameter. 
#' @param endVal The maximum of the given parameter. 
#' @param nSteps Number of steps from startVal to endVal. It equals the number of simulations, and number of curves on the final plot. 
#' @param fileTochange Please choose "EPC", "INI" or "BOTH". This file will be used for the analysis, and the original parameter values will be changed according to the choice of the user. 
#' @return Graph showing the runs with the selected parameters with color coding. The graph will show data from the last simulation year. 
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point aes labs theme ggsave element_blank facet_wrap
#' @importFrom dplyr filter group_by summarize mutate '%>%' tbl_df select
#' @importFrom tibble rownames_to_column
#' @importFrom magrittr '%<>%'
#' @importFrom tidyr separate
#' @export

musoQuickEffect <- function(settings = setupMuso(), calibrationPar = NULL,  startVal, endVal, nSteps = 1, fileToChange="epc",modifyOriginal=TRUE, outVar, parName = "parVal", yearNum=1, year=(settings$startYear + yearNum -1),fixAlloc=FALSE){

    if(is.character(outVar)){
                      varNames <- as.data.frame(musoMappingFind(outVar))
                      if(nrow(varNames)!=1){
                          warning("There are more than one output variable in conection with ", outVar, ". The first possibility were choosen.")
                          print(varNames)
                          outVarIndex <- unlist(varNames[1,1])
                          varNames <- as.character(unlist(varNames[1,2]))
                      } else {
                          outVarIndex <- unlist(varNames[1,1])
                          varNames <- as.character(unlist(varNames[1,2]))
                      }
                  } else {
                      varNames <- musoMapping(outVar)
                      outVarIndex<-outVar
                  }
    
     if(is.null(calibrationPar)){
         calibrationPar <- settings$calibrationPar
     }
    
    parVals <- seq(startVal, endVal, length = (nSteps + 1))
    parVals <- dynRound(startVal, endVal, seqLen = (nSteps + 1))
    a <- do.call(rbind,lapply(parVals, function(parVal){
        calResult <- tryCatch(calibMuso(settings = settings,calibrationPar = calibrationPar,
                                        modifyOriginal = modifyOriginal,
                                        parameters = parVal,
                                        outVars = outVarIndex,
                                        silent = TRUE,
                                        fileToChange = fileToChange,fixAlloc=fixAlloc), error = function(e){NULL})
        if(is.null(calResult)){
            b <- cbind(rep(NA,365),parVal)
            rownames(b) <- musoDate(startYear = year, numYears = 1)
            colnames(b)[1] <- varNames
            return(b)
        } else {
            if(yearNum >=0){
                m <- as.data.frame(calResult[musoDate(startYear = year, numYears = 1),])
            } else{
                m <- as.data.frame(calResult)
            }
            colnames(m) <- colnames(calResult)
            return(cbind(m, parVal))
        }
        
    }))
    a %<>%
        tbl_df %>%
        mutate(date=as.Date(rownames(a),"%d.%m.%Y")) %>%
        select(date,as.character(varNames),parVal)
    print(suppressWarnings(ggplot(data = a, aes_string(x= "date", y= varNames))+geom_line(aes(alpha = factor(parVal))) + labs(y=varNames, alpha = parName) + scale_alpha_discrete(range=c(0.25,1))))
}
# calma <- calibMuso(settings = settings,calibrationPar = calibrationPar,
#                                          modifyOriginal = modifyOriginal,
#                                          parameters = parVal,
#                                          outVars = outVarIndex,
#                                          silent = TRUE,
#                                          fileToChange = fileToChange)
#  plot(calma[,1])
# calma <- calibMuso(settings = settings,calibrationPar = calibrationPar,
#                                          modifyOriginal = modifyOriginal,
#                                          parameters = parVal,
#                                          silent = TRUE,
#                                          fileToChange = fileToChange)
# calm <- calibMuso(calibrationPar=calibrationPar,parameters=parVal,modifyOriginal=TRUE)
# plot(x=as.Date(musoDate(2015,numYears=1),"%d.%m.%Y"),y=calm[musoDate(2015,numYears=1),"daily_gpp"],type="l")
# calibrationPar
# parVal
