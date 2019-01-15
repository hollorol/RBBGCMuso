#' musoQuickEffect
#'
#' This function changes a choosen parameter, and visualize the effect of the change on a chosen variable.
#' @author Roland Hollos
#' @param settings The settings from setupMuso output
#' @param startVal The oroginal parameterValue
#' @param endVal The goal value while the function pass 
#' @param nSteps How many steps 'till you reach the endVal
#' @param fileTochange Please choose "epc" "ini" or "both". This is the place of the orininal variable.
#' @return An effect plot
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point aes labs theme ggsave element_blank facet_wrap
#' @importFrom dplyr filter group_by summarize mutate '%>%' tbl_df select
#' @importFrom tibble rownames_to_column
#' @importFrom magrittr '%<>%'
#' @importFrom tidyr separate
#' @export

musoQuickEffect <- function(settings = NULL,calibrationPar = NULL,  startVal, endVal, nSteps = 1, fileToChange="epc", outVar, parName = "parVal"){

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
    
    if(is.null(settings)){
        settings <- setupMuso()
    }
     if(is.null(calibrationPar)){
         calibrationPar <- settings$calibrationPar
     }
    
    parVals <- seq(startVal, endVal, length = (nSteps + 1))
    a <- do.call(rbind,lapply(parVals, function(parVal){
        calResult <- tryCatch(calibMuso(settings = settings,calibrationPar = calibrationPar, parameters = parVal, outVars = outVarIndex, silent = TRUE,fileToChange = fileToChange), error = function(e){NA})
        if(all(is.na(calResult))){
            b <- cbind(rep(NA,365),parVal)
            rownames(b) <- tail(musoDate(startYear = settings$startYear, numYears = settings$numYears),365)
            colnames(b)[1] <- varNames
            return(b)
        } else {
            return(cbind(tail(calResult,365), parVal))
        }
        
    }))
    
    a %<>%
        tbl_df %>%
        mutate(date=as.Date(rownames(a),"%d.%m.%Y")) %>%
        select(date,as.character(varNames),parVal)
    print(suppressWarnings(ggplot(data = a, aes_string(x= "date", y= varNames))+geom_line(aes(alpha = factor(round(parVal,2)))) + labs(y=varNames, alpha = parName) + scale_alpha_discrete(range=c(0.25,1))))
}
