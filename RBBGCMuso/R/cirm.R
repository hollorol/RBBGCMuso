#' cirmi_glue
#'
#' This funtion uses the Monte Carlo technique to uniformly sample the parameter space from user defined parameters of the Biome-BGCMuSo model. The sampling algorithm ensures that the parameters are constrained by the model logic which means that parameter dependencies are fully taken into account (parameter dependency means that e.g leaf C:N ratio must be smaller than C:N ratio of litter; more complicated rules apply to the allocation parameters where the allocation fractions to different plant compartments must sum up 1). This function implements a mathematically correct solution to provide uniform distriution of the random parameters on convex polytopes. 
#' @author Roland HOLLOS
#' @importFrom future future
#' @importFrom rpart rpart rpart.control
#' @importFrom rpart.plot rpart.plot
#' @param measuremets The table which contains the measurements
#' @param calTable A dataframe which contantains the ini file locations and the domains they belongs to
#' @param parameters A dataframe with the name, the minimum, and the maximum value for the parameters used in MonteCarlo experiment 
#' @param dataVar A named vector where the elements are the MuSo variable codes and the names are the same as provided in measurements and likelihood
#' @param iterations The number of MonteCarlo experiments to be executed
#' @param burnin Currently not used, altought it is the length of burnin period of the MCMC sampling used to generate random parameters
#' @param likelihood A list of likelihood functions which names are linked to dataVar
#' @param execPath If you are running the calibration from different location than the MuSo executable, you have to provide the path
#' @param thread_prefix The prefix of thread directory names in the tmp directory created during the calibrational process
#' @param numCores The number of processes used during the calibration. At default it uses one less than the number of threads available
#' @param pb The progress bar function. If you use (web-)GUI you can provide a different function
#' @param pbUpdate The update function for pb (progress bar)
#' @param copyThread A boolean, recreate tmp directory for calibration or not (case of repeating the calibration)
#' @param contsraints A dataframe containing the constraints logic the minimum and a maximum value for the calibration.
#' @param th A trashold value for multisite calibration. What percentage of the site should satisfy the constraints. 
#' @param treeControl A list which controls (maximal complexity, maximal depth) the details of the decession tree making.
#' @param max_steps maximum iteration steps in the cirm function
#' @param succ_rate_th Treshold for succes rate.
#' @export

cirmi_glue <- function(parameters,
                       iterations = 100,
                       max_steps = 10,
                       succ_rate_th = 0.8,
                       intermediate_dir = "cirm_out",...){

    for(i in 1:max_steps){
        results <- multiSiteCalib(parameters, iterations, ...)
        tree <- tree_per_const(results)
        save_intermediate_state(intermediate_dir)
        parameters <- update_parameters(tree, parameters)
        if(check_succ_rate(succ_rate_th)){
            break;
        }
    }
    
    return(results)
}

dir.create("new_results")
for(i in 1:10){
    print(sprintf("step: %s",i))
    stepdir <- paste0("new_results/",i)
    dir.create(stepdir)
    parameters <- read.csv("Martonvasar_maize.set",skip=1,stringsAsFactors=FALSE)
    results <- multiSiteCalib(measurements=measurements,
                              parameters=parameters, calTable=calTable,
                              dataVar = dataVar,
                              iterations= 10000,
                              likelihood = likelihood,
                              execPath="./", constraints = consts, th =th)
    tree_per_const()
    glue()
    file.copy(c("Martonvasar_maize.set","result.csv","results.RDS","constRes.csv",
                "constraints.json","calibRes.png","tree_per_const.pdf",
                "Martonvasar_maize_after_tree.set","Martonvasar_maize.set",
                "gplot.pdf","maize_median.epc","maize_glue.epc","maize_ml.epc","errorlog.txt"),stepdir)
    file.copy("Martonvasar_maize_after_tree.set","Martonvasar_maize.set", overwrite=TRUE)
}
