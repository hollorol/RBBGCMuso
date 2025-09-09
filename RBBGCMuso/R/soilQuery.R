#' getSoilDataFull
#' 
#' This function collects soil data from a given restapi, de default is soilGrid
#'
#' @author Roland HOLLÓS
#' @name getSoilDataFull
#' @importFrom glue glue
#' @importFrom httr  config with_config GET content

getSoilDataFull <- function(lat, lon, apiURL) {
    if(missing(apiURL)){
        apiURL <- "https://rest.isric.org/soilgrids/v2.0/properties"
    }
    apiString <- glue("{apiURL}/query?lon={lon}&lat={lat}")
    soilREST <- #with_config(config(ssl_verifypeer=0L, ssl_verifyhost=0L),
                            GET(apiString) # ) # This is temporary solution ssl_verification wont bypass
   content(soilREST) 
}

#' createSoilFile
#' 
#' This function collects soil data from a given restapi, de default is soilGrid
#'
#' @author Roland HOLLOS
#' @name createSoilFile
#' @importFrom glue glue
#' @importFrom stats approx 
#' @importFrom magrittr '%>%'
#' @export

createSoilFile <- function(lat,lon,
                            outputFile="recent.soi",
                            method="constant",apiURL, getVWC = FALSE, getSOC = TRUE, getBD = FALSE.
                            template=system.file("examples/hhs/hhs_muso7.soi",package="RBBGCMuso")) {
    if(missing(apiURL)){
        apiURL <- "https://rest.isric.org/soilgrids/v2.0/properties"
    }
    outFile <- suppressWarnings(readLines(template))
    outFile[1] <- sprintf("SOILPROP FILE - lat: %s, lon: %s, created in: %s",lat,lon,date())
    musoCenters <- c(1.5,6.5,20.0,45.0,75.0,105.0,135.0,175.0,300.0,700.0)
    # soilGridDepths  <- c(0,5,15,30,60,100,200)
    soilGridDepths  <- c(2.5, 10, 22.5, 45, 80, 150)
    Reduce(function(x,y){(y-x)/2+x},soilGridDepths,accumulate=TRUE)
    rest<- getSoilDataFull(lat,lon, apiURL)


    createMusoLayers <- function(values,depths=soilGridDepths,centers=musoCenters,intMethod=method){
        approx(x=depths,y=values, xout = centers, method=intMethod,rule=2)$y %>%
            paste(.,collapse="\t") %>% paste0(.," ")
    }

    soilDepth <- tryCatch(getMeanSoil(rest,"bdod")/100,error=function(e){stop("There is no data for the given coordinates")})
    outFile[88] <- sprintf("%s (%%) percentage of sand by volume in rock free soil",
                           paste(createMusoLayers(getMeanSoil(rest,"sand")/10), collapse="\t"))
    outFile[89] <- sprintf("%s (%%) percentage of silt by volume in rock free soil",
                           paste(createMusoLayers(getMeanSoil(rest,"silt")/10), collapse="\t"))
    outFile[90] <- sprintf("%s (dimless) soil PH",
                           paste(createMusoLayers(getMeanSoil(rest,"phh2o")/10), collapse="\t"))
    if(getBD){ # Because this is for the "fine earth fraction" which is not necessarily what we want
    outFile[92] <- sprintf("%s (g/cm3) bulk density",
                           paste(createMusoLayers(getMeanSoil(rest,"bdod")/100), collapse="\t"))
    }
    if(getVWC){
        # this can be used for field capacity
        outFile[93] <- sprintf("%s (m3/m3) volumetric water content at saturation",
                            paste(createMusoLayers(getMeanSoil(rest,"wv0010")/1000), collapse="\t"))
        # we've found out that the values underestimate the field capacity, so this is just conditional for now
        # but we have ratios so in the future we'll use that 
        outFile[94] <- sprintf("%s (m3/m3) volumetric water content at field capacity",
                            paste(createMusoLayers(getMeanSoil(rest,"wv0033")/1000), collapse="\t"))
        # this can be used for wilting point
        outFile[95] <- sprintf("%s (m3/m3) volumetric water content at wilting point",
                            paste(createMusoLayers(getMeanSoil(rest,"wv1500")/1000), collapse="\t"))
    }

    # this isn't from the properties url
    # if (getSOC) {
    #     soilOC <- tryCatch(getMeanSoil(rest, "soc") * 10, error = function(e) {
    #         stop("There is no data for the given coordinates")
    #     })
    #     soilN <- tryCatch(getMeanSoil(rest, "nitrogen") * 10, error = function(e) {
    #         stop("There is no data for the given coordinates")
    #     })
    #     # Create a data frame for SOC and Nitrogen
    #     soilData <- data.frame(
    #         Depth_cm = soilGridDepths,
    #         SOC_g_per_kg = soilOC,
    #         Nitrogen_g_per_kg = soilN
    #     )
        
    #     csvFile <- sub("\\.soi$", "_soc_n.csv", outputFile)
    #     write.csv(soilData, file = csvFile, row.names = FALSE)
    #     cat(glue("SOC (g/kg) and Nitrogen (g/kg) data saved to {csvFile}\n"))
    # }

    writeLines(outFile,outputFile)
}


getMeanSoil <- function(rest, name){
    sapply(
        rest$properties$layers[sapply(rest$properties$layers,function(x){
                   x$name == name
                })][[1]]$depths,
           function(s){
                s$values$mean
           }
    )
}
