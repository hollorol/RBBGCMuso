#' getyearlycum
#'
#' Funtion for getting cumulative yearly data from observations
#' @author Roland Hollos
#' @param daily_observations vector of the daily observations.
#' @return A vector of yearly data
#' @export


getyearlycum<-function(daily_observations){
  number_of_years<-length(daily_observations)/365
  # daily_observations[is.na(daily_observations)]<-0 # 3+NA=NA
  fr<-1
  yearlycum<-rep(NA,number_of_years)
  for(i in 1:number_of_years){
    to<-i*365
    yearlycum[i]<-sum(daily_observations[fr:to],na.rm = TRUE)
    fr<-i*365+1
  }
  return(yearlycum)
}

#' getyearlymax
#'
#' Function for getting the maximum values of the years, from daily data
#' @author Roland Hollos
#' @param daily_observations vector of the daily observations
#' @return A vector of yearly data
#' @usage getyearlymax(daily_observations)
#' @export

getyearlymax<-function(daily_observations){
  number_of_years<-length(daily_observations)/365
  # daily_observations[is.na(daily_observations)]<-0 # 3+NA=NA
  fr<-1
  yearlycum<-rep(NA,number_of_years)
  for(i in 1:number_of_years){
    to<-i*365
    yearlymax[i]<-max(daily_observations[fr:to],na.rm=TRUE)
    fr<-i*365+1
  }
  return(yearlymax)
}

#' fextension
#'
#' A function for extracting the extension name from the filename string
#' @author Roland Hollos
#' @param filename The string of the filenam
#' @return the extension of the given file
#' @usage fextension(filename)

fextension <- function(filename){
    #this function gives back the given filenames extension
    fextension <- tail(unlist(strsplit(filename,"\\.")),1)
    return(fextension)
}

#'supportedMuso
#'
#' A function for getting the list of the output formats which is supported by RBBGCMuso
#' @author Roland Hollos
#' @param type "outputs" or "message", if you choose "outputs", it gives you a simple vector of the formats, if you choose "message", it gives you a full sentence which contains the same information. 
#' @return if you choose "outputs", it gives you a simple vector of the formats, if you choose "message", it gives you a full sentence which contains the same information.
#' @usage supportedMuso(type="outputs")
#' @export

supportedMuso <- function(type="outputs"){
    supportedFormats <- c("xls","xlsx","odt","csv","txt")
    
    if(type=="outputs"){
        #If you add new format supports, please expand the lists
        return(supportedFormats)
    }
    if(type=="message"){
        return(cat("Supported formats are ",supportedFormats,"If your fileformat is something else, we automaticle coerced it to csv.\n"))
    }
}

#' corrigMuso
#'
#' This function leapyear-corrigate the output of the modell
#' @author Roland Hollos
#' @param settings This is the output of the setupMuso() function. It contains all of the RBBGCMuso settings
#' @param data the models outputdata
#' @return It returns the modells leapyear-corrigated output data.
#' @export
#' @usage corrigMuso(settings, data)

corrigMuso <- function(settings, data){

    insertRow <- function(existingDF, newrow, r){
        nr <- nrow(existingDF)
        existingDF <- rbind(existingDF,rep(NA,ncol(existingDF)))
        existingDF[seq(r+1,nr+1),] <- existingDF[seq(r,nr),]
        existingDF[r,] <- newrow
        existingDF
    }


    numdays <- nrow(data)
    data <- data
    numyears <- settings$numyears
    leapyears <- musoLeapYears(settings)
    sylvesters <- data[seq(from=365, to=numdays, by=365),]
    ind <- 0
    for(i in 1:numyears){
        
        if(leapyears[i]){
            data <- insertRow(data,sylvesters[i],i*360+ind)
            ind <- ind+1
        }
    }
    return(data)
}

## #' file.path2
## #'
## #' It is an extended file.path function, it can concatenate path where the first ends and the second begins with "/", so
## #' there wont be two  slash nearby eachother
## #' @author Roland Hollos
## #' @param str1 This is the first path string
## #' @param str2 This is the second path string
## #' @return A concatenated path
## #' @export
## #' @usage file.path2(str1, str2)

## file.path2<-function(str1, str2){
##     if(str1==""|str1=="./"){
##         return(str2)
##     }
##     str1<-file.path(dirname(str1),basename(str1))
##     if(substring(str2,1,1)=="/"){
##         return(paste(str1,str2,sep=""))
##     } else{
##         return(file.path(str1,str2))
##     }
## } 

numFactors <- function(x,type="pos"){
    x <- as.integer(abs(x))
    div <- seq_len(x)
    posdiv <- div[x%%div==0L]
    negdiv <- posdiv*-1
    alldiv <- c(negdiv,posdiv)
    switch(type,"pos"=return(posdiv),"neg"=return(negdiv),"all"=return(alldiv))
}

niceMatrixLayoutForPlots <- function(n){
    if(n==0){
        return(cat("Ther is nothing to do with 0 graph"))
    }
    n <- as.integer(n)
    factors <- numFactors(n)
    if(length(factors)==2){
        return(n)}
    sqrtn <- round(sqrt(n))
    num1 <- factors[which(min(abs(factors-sqrtn))==abs(factors-sqrtn))[1]]
    num2 <- n/num1
    return(c(num1,num2))
}

truncNorm<-function(N,mean, sd, min, max){
n=0
randomNorm<-rep(NA,N)
while(n<=N){
  transNorm<-rnorm(1,mean,sd)
  if((transNorm>min)&(transNorm<max)){
    randomNorm[n]<-transNorm
    n<-n+1
  }
}
return(randomNorm)
}

#' getConstMatrix
#'
#' getConstMatrix is a function whith wich you can get the default constrain matrix for your choosen type and version. 
#' @param filetype It can be "epc" or "soil".
#' @param version The version of the MuSo environment
#' @export 

getConstMatrix <- function (filetype="epc", version = as.character(getOption("RMuso_version"))) {
    getOption("RMuso_constMatrix")[[filetype]][[version]]
}


#' fixAlloc 
#' 
#' Fix allocation parameter in the epc file
#'
#' @param settings the base RMuso settings variable
#' @param type normal or spinup depending what you want to modify
#' @usage ...
#' @export 

fixAlloc <- function(settings=NULL,type="normal"){
    if(is.null(settings)){
        settings <- setupMuso()
    }
    print("Need fix?")
    epc_file <- settings$epcInput[type]
    depTable <- options()$RMuso_constMatrix$epc[[as.character(options()$RMuso_version)]]
    alloc_params<- depTable$INDEX[grep("ALLOCATION",depTable$NAME)]
    alloc_groups <- round(100*(alloc_params - floor(alloc_params)))
    tapply(alloc_params, alloc_groups, function(x){
        currentValues <- readValuesFromFile(epc_file,x)
        difference <- 1 - sum(currentValues)
        if(difference == 0){
            return(FALSE)
        }
        tomodiff <- currentValues[currentValues != 0]
        changemulline(filePaths="c3grass_muso7.epc",
                      contents=(tomodiff + difference/length(tomodiff)),
                      calibrationPar=x[currentValues != 0])
        return(TRUE)
    })
}


#' Redistribute Soil Organic Carbon (SOC) in a Biome-BGCMuSo Endpoint File 
#'
#' This function reads a binary endpoint file, modifies the SOC (carbon) and SON (nitrogen) pools
#' based on target values for any specified topsoil depth, and writes a new
#' endpoint file with a dynamically generated name. The C:N ratio for each
#' pool is conserved. This function is useful for creating a specific endpoint (initialization) with
#' our desired SOC distribution for example for a bare fallow experiment with measurements.
#'
#' @param input_file Path to the original binary endpoint file.
#' @param output_dir Path to the directory where the output file will be saved.
#' @param site_name Character string for the site name ("askov"). Used for the output filename.
#' @param method_name Character string for the method name ("Dist1"). Used for the output filename.
#' @param target_layer_cm Numeric. The thickness of the topsoil layer to consider (in cm). Can be any value up to 1000.
#' @param target_tsoc_layer Numeric. The target total SOC for the specified topsoil layer.
#' @param target_rsoc_layer Numeric. The target resistant SOC (pool 4) for the specified topsoil layer.
#' @param ratio_components Numeric vector of length 2 (e.g., c(0.5, 9.5)). Represents the
#'        numerator and denominator of the desired SOC pool 2 to pool 3 ratio. This is needed for distributing the remaning SOC after we've set pool 4. Pool 1 remains untouched.
#'
#' @return Invisibly returns the path to the created output file.
#' @export
musoRedistSoc <- function(input_file,
                             output_dir,
                             site_name,
                             method_name,
                             target_layer_cm,
                             target_tsoc_layer,
                             target_rsoc_layer,
                             ratio_components) {
  
    # Variable and Layer Definitions 
    variable_defs <- list(
        soil1c = list(name = "soil1c", indices = 98:107),
        soil2c = list(name = "soil2c", indices = 108:117),
        soil3c = list(name = "soil3c", indices = 118:127),
        soil4c = list(name = "soil4c", indices = 128:137),
        soil1n = list(name = "soil1n", indices = 263:272),
        soil2n = list(name = "soil2n", indices = 273:282),
        soil3n = list(name = "soil3n", indices = 283:292),
        soil4n = list(name = "soil4n", indices = 293:302)
    )
    layer_thickness_cm <- c(3, 7, 20, 30, 30, 30, 30, 50, 200, 600)
    total_depth <- sum(layer_thickness_cm)
    
    # Input Validation and Setup
    if (!file.exists(input_file)) stop("Input file not found: ", input_file)
    if (!dir.exists(output_dir)) stop("Output directory not found: ", output_dir)
    if (target_layer_cm > total_depth) stop("target_layer_cm cannot exceed total profile depth of ", total_depth, " cm.")
    if (length(ratio_components) != 2) stop("ratio_components must be a numeric vector of length 2.")
    
    ratio_str <- paste(ratio_components, collapse = "-")
    output_filename <- paste0(site_name, ".", method_name, "_RSOC_", ratio_str, "_SOC", target_layer_cm, ".endpoint")
    output_file <- file.path(output_dir, output_filename)
    ratio_pool2_pool3 <- ratio_components[1] / ratio_components[2]
    
    # Read Input File 
    con <- file(input_file, "rb")
    endpoint_data <- readBin(con, what = "double", n = 400, size = 8, endian = "little")
    close(con)
    modified_data <- endpoint_data
    
    # Calculations
    soc1_idx <- variable_defs$soil1c$indices + 1; soc2_idx <- variable_defs$soil2c$indices + 1
    soc3_idx <- variable_defs$soil3c$indices + 1; soc4_idx <- variable_defs$soil4c$indices + 1
    son1_idx <- variable_defs$soil1n$indices + 1; son2_idx <- variable_defs$soil2n$indices + 1
    son3_idx <- variable_defs$soil3n$indices + 1; son4_idx <- variable_defs$soil4n$indices + 1
    
    calculate_top_layer_content <- function(pool_data, target_depth, thicknesses) {
        cumulative_depths <- cumsum(thicknesses)
        total_content <- 0
        full_layers_indices <- which(cumulative_depths <= target_depth)
        if (length(full_layers_indices) > 0) {
        total_content <- sum(pool_data[full_layers_indices])
        }
        partial_layer_index <- findInterval(target_depth, cumulative_depths) + 1
        if (partial_layer_index <= length(thicknesses) && target_depth > (cumulative_depths[partial_layer_index - 1] %||% 0)) {
        depth_above_partial <- if (partial_layer_index > 1) cumulative_depths[partial_layer_index - 1] else 0
        included_thickness <- target_depth - depth_above_partial
        fraction <- included_thickness / thicknesses[partial_layer_index]
        total_content <- total_content + (pool_data[partial_layer_index] * fraction)
        }
        return(total_content)
    }
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    
    soc4_top_orig <- calculate_top_layer_content(modified_data[soc4_idx], target_layer_cm, layer_thickness_cm)
    multiplier_soc4 <- target_rsoc_layer / soc4_top_orig
    modified_data[soc4_idx] <- modified_data[soc4_idx] * multiplier_soc4
    modified_data[son4_idx] <- modified_data[son4_idx] * multiplier_soc4
    
    soc1_top_current <- calculate_top_layer_content(modified_data[soc1_idx], target_layer_cm, layer_thickness_cm)
    soc4_top_current <- calculate_top_layer_content(modified_data[soc4_idx], target_layer_cm, layer_thickness_cm)
    soc_to_distribute <- target_tsoc_layer - soc1_top_current - soc4_top_current
    
    if (soc_to_distribute < 0) {
        warning("Target SOC is less than the sum of SOC1 and the new RSOC. This may result in negative values for SOC2 or SOC3.")
    }
    
    target_soc2_top <- soc_to_distribute * (ratio_pool2_pool3 / (1 + ratio_pool2_pool3))
    target_soc3_top <- soc_to_distribute * (1 / (1 + ratio_pool2_pool3))
    soc2_top_current <- calculate_top_layer_content(modified_data[soc2_idx], target_layer_cm, layer_thickness_cm)
    soc3_top_current <- calculate_top_layer_content(modified_data[soc3_idx], target_layer_cm, layer_thickness_cm)
    multiplier_soc2 <- if (soc2_top_current != 0) target_soc2_top / soc2_top_current else 0
    multiplier_soc3 <- if (soc3_top_current != 0) target_soc3_top / soc3_top_current else 0
    
    modified_data[soc2_idx] <- modified_data[soc2_idx] * multiplier_soc2
    modified_data[son2_idx] <- modified_data[son2_idx] * multiplier_soc2
    modified_data[soc3_idx] <- modified_data[soc3_idx] * multiplier_soc3
    modified_data[son3_idx] <- modified_data[son3_idx] * multiplier_soc3
    
    # Write Output File
    con_out <- file(output_file, "wb")
    writeBin(modified_data, con_out, size = 8, endian = "little")
    close(con_out)
    message("Successfully created: ", basename(output_file))
    return(invisible(output_file))
}


#' Batch Process SOC Redistribution for Multiple Scenarios
#'
#' A wrapper function to run `musoRedistSoc` for multiple methods and ratios
#' based on a single configuration list.
#'
#' @param config A list containing all parameters for the batch run. See example for structure
#'
#' @return Invisibly returns a vector of the file paths created.
#' @examples
#' run_config <- list(
#'  
#'  # Site level information
#'  site_name = "askov270",
#'  output_dir = "c:/muso/SOC/askovGLUE/",
#'  # this will be the base endpoint file which will be modified for each method and ratio
#'  input_file = "c:/muso/SOC/askovGLUE/askov270GLUE/base.endpoint", 
#'  
#'  target_layer_cm = 23,
#'  target_tsoc_layer = 4.77,
#'  
#'  # "method" names and their corresponding rsoc values
#'  methods = list(
#'    list(name = "Barre",   rsoc = 1.32),
#'    list(name = "Falloon", rsoc = 0.4),
#'    list(name = "Willard", rsoc = 1.76)
#'  ),
#'  
#'  # A list of ratio components. The script will loop through these for EACH method
#'  ratios = list(
#'    c(1, 9), c(2, 8), c(3, 7), c(4, 6), c(5, 5),
#'    c(6, 4), c(7, 3), c(8, 2), c(9, 1),
#'    c(0.5, 9.5) 
#'  )
#' )
#' # using the function with the example list
#' musoBatchRedistSoc(config = run_config)
#' 
#' @export

musoBatchRedistSoc <- function(config) {
  
  created_files <- c()
  
  for (method in config$methods) {
    
    message(paste("\n Processing Method:", method$name, " "))
    
    # For the current method, loop over all specified ratios
    for (ratio in config$ratios) {
      
      # Call the worker function with parameters from the config list
      new_file <- musoRedistSoc(
        input_file        = config$input_file,
        output_dir        = config$output_dir,
        site_name         = config$site_name,
        method_name       = method$name,
        target_layer_cm   = config$target_layer_cm,
        target_tsoc_layer = config$target_tsoc_layer,
        target_rsoc_layer = method$rsoc,
        ratio_components  = ratio
      )
      created_files <- c(created_files, new_file)
    }
  }
  
  message(paste("\nBatch processing complete.", length(created_files), "files created."))
  return(invisible(created_files))
}

#Example list for running the batch function
# run_config <- list(
  
#   # Site level information
#   site_name = "askov270",
#   output_dir = "c:/muso/SOC/askovGLUE/",
#   # this will be the base endpoint file which will be modified for each method and ratio
#   input_file = "c:/muso/SOC/askovGLUE/askov270GLUE/base.endpoint", 
  
#   target_layer_cm = 23,
#   target_tsoc_layer = 4.77,
  
#   # "method" names and their corresponding rsoc values
#   methods = list(
#     list(name = "Barre",   rsoc = 1.32),
#     list(name = "Falloon", rsoc = 0.4),
#     list(name = "Willard", rsoc = 1.76)
#   ),
  
#   # A list of ratio components. The script will loop through these for EACH method
#   ratios = list(
#     c(1, 9), c(2, 8), c(3, 7), c(4, 6), c(5, 5),
#     c(6, 4), c(7, 3), c(8, 2), c(9, 1),
#     c(0.5, 9.5) 
#   )
# )
# using the function with the example list
# musoBatchRedistSoc(config = run_config)
