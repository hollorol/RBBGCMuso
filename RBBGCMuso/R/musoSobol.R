#' musoSobol
#'
#' Variance-based (Sobol) sensitivity analysis for Biome-BGCMuSo.
#'
#' Uses the Saltelli (2002) extension of the Sobol method as implemented in
#' \code{sensitivity::sobol2007}.  Two independent random matrices X1 and X2
#' (each \code{n} rows × \emph{p} parameters) are combined into a design of
#' \code{n * (p + 2)} model evaluations.  From the resulting output vector,
#' first-order indices (S1) and total-order indices (ST) are computed with
#' bootstrap confidence intervals.
#'
#' \strong{Note on constraints:}  Sobol's method requires independent parameter
#' samples, so parameter dependency constraints (as used in \code{musoMonte})
#' are not applied here.  If your parameters have strong dependencies, interpret
#' the indices with care.
#'
#' @param parameters Dataframe (or path to CSV) with four columns: name, EPC
#'   row number, minimum value, maximum value.  The same format as used by
#'   \code{musoMonte}.
#' @param settings Settings list from \code{setupMuso()}.  If NULL, settings
#'   are generated automatically.
#' @param n Number of base samples per matrix (X1 and X2).  The total number
#'   of model runs is \code{n * (p + 2)} where \emph{p} is the number of
#'   parameters.  Default 500.
#' @param nboot Number of bootstrap replicates for confidence intervals.
#'   Default 100.
#' @param varIndex Which output variable to use as the SA target (1-based index
#'   into the model output columns).  Default 1.
#' @param outVars Character vector of output variable names.  If NULL, the
#'   first output variable set from the settings INI file is used.
#' @param fun Aggregation function applied to each model time series to produce
#'   a scalar response (default: \code{mean}).
#' @param outLoc Directory for intermediate per-run CSV files.
#' @param outputFile Path for the results CSV (S1 and ST with CIs).
#' @param plotName Path for the bar-chart PNG showing S1 and ST side by side.
#' @param plotTitle Title for the plot.
#' @param skipSpinup Skip spinup phase.  Default TRUE.
#' @param skipZero Replace zero model outputs with NA.  Default TRUE.
#' @param postProcString Optional post-processing expression string.
#' @param modifyOut Whether to modify the original EPC file (TRUE) or a copy.
#' @param parallel If TRUE, the model evaluations are distributed across
#'   multiple cores using \pkg{doParallel} and \pkg{foreach}.  Default FALSE.
#' @param nCores Number of parallel workers.  NULL uses
#'   \code{detectCores() - 1}.
#' @param dpi Resolution for the saved plot.  Default 300.
#' @importFrom ggplot2 ggplot aes geom_bar geom_errorbar position_dodge
#'   theme element_text xlab ylab ggtitle ggsave scale_fill_manual
#' @export

musoSobol <- function(parameters    = NULL,
                      settings      = NULL,
                      n             = 500,
                      nboot         = 100,
                      varIndex      = 1,
                      outVars       = NULL,
                      fun           = mean,
                      outLoc        = "./calib",
                      outputFile    = "sobol_indices.csv",
                      plotName      = "sobol.png",
                      plotTitle     = "Sobol Sensitivity Indices",
                      skipSpinup    = TRUE,
                      skipZero      = TRUE,
                      postProcString = NULL,
                      modifyOut     = TRUE,
                      parallel      = FALSE,
                      nCores        = NULL,
                      dpi           = 300){

    # ------------------------------------------------------------------
    # 0. Dependencies
    # ------------------------------------------------------------------
    if(!requireNamespace("sensitivity", quietly = TRUE))
        stop("Package 'sensitivity' is required. Install with: install.packages('sensitivity')")
    if(!requireNamespace("lhs", quietly = TRUE))
        stop("Package 'lhs' is required. Install with: install.packages('lhs')")
    if(parallel){
        if(!requireNamespace("doParallel", quietly = TRUE))
            stop("Package 'doParallel' is required for parallel mode.")
        if(!requireNamespace("foreach", quietly = TRUE))
            stop("Package 'foreach' is required for parallel mode.")
    }

    # ------------------------------------------------------------------
    # 1. Load / validate parameters
    # ------------------------------------------------------------------
    if(is.null(parameters)){
        parameters <- tryCatch(
            read.csv("parameters.csv", stringsAsFactors = FALSE),
            error = function(e)
                stop("Provide a parameters dataframe or a path to parameters.csv")
        )
    } else if((!is.list(parameters)) && (!is.matrix(parameters))){
        parameters <- tryCatch(
            read.csv(parameters, stringsAsFactors = FALSE),
            error = function(e)
                stop("Cannot read parameters file or matrix")
        )
    }

    # Sort by EPC line number (same convention as musoMonte)
    paramOrder  <- order(parameters[, 2])
    parameters  <- parameters[paramOrder, ]
    paramNames  <- gsub("([\\s]|\\-epc)", "", parameters[, 1], perl = TRUE)
    npar        <- nrow(parameters)

    # ------------------------------------------------------------------
    # 2. Setup model settings
    # ------------------------------------------------------------------
    if(is.null(settings)){
        settings <- setupMuso()
    }
    settings$calibrationPar <- parameters[, 2]

    if(is.null(outVars)){
        numVars    <- length(settings$outputVars[[1]])
        outVarNames <- settings$outputVars[[1]]
    } else {
        numVars    <- length(outVars)
        outVarNames <- sapply(outVars, musoMapping)
    }
    if(!is.null(postProcString)){
        outVarNames <- c(outVarNames,
                         gsub("\\s", "",
                              unlist(strsplit(postProcString, "<-"))[1]))
    }
    if(!is.list(fun)){
        funct <- rep(list(fun), numVars)
    } else {
        funct <- fun
    }

    # ------------------------------------------------------------------
    # 3. Build the Sobol design using Latin Hypercube Sampling.
    #
    # LHS gives better space-filling coverage than plain runif for the same
    # n, which means more accurate sensitivity indices with fewer runs.
    # Sobol's method requires the two matrices to be statistically
    # independent, which LHS preserves (X1 and X2 are sampled separately).
    # ------------------------------------------------------------------
    if(!requireNamespace("lhs", quietly = TRUE)){
        stop(paste0("Package 'lhs' is required for musoSobol.\n",
                    "Install with: install.packages('lhs')"))
    }

    lh1 <- lhs::randomLHS(n, npar)
    lh2 <- lhs::randomLHS(n, npar)

    X1 <- as.data.frame(matrix(nrow = n, ncol = npar))
    X2 <- as.data.frame(matrix(nrow = n, ncol = npar))
    for(j in seq_len(npar)){
        lo <- parameters[j, 3]
        hi <- parameters[j, 4]
        X1[, j] <- lo + lh1[, j] * (hi - lo)
        X2[, j] <- lo + lh2[, j] * (hi - lo)
    }
    colnames(X1) <- colnames(X2) <- paramNames

    # Create sensitivity object (model = NULL: we supply Y ourselves)
    sa    <- sensitivity::sobol2007(model = NULL, X1 = X1, X2 = X2,
                                    nboot = nboot)
    nRuns <- nrow(sa$X)
    message(sprintf(
        "musoSobol: running %d model evaluations (%d base samples x %d parameters + 2).",
        nRuns, n, npar
    ))

    # ------------------------------------------------------------------
    # 4. Run the model for every row in the Sobol design
    # ------------------------------------------------------------------
    if(parallel){
        nCoresActual <- if(is.null(nCores)) max(1L, parallel::detectCores() - 1L) else as.integer(nCores)
        nCoresActual <- min(nCoresActual, nRuns)

        cl <- parallel::makeCluster(nCoresActual)
        doParallel::registerDoParallel(cl)
        `%dopar%` <- foreach::`%dopar%`

        localSettings    <- settings
        localSaX         <- sa$X
        localSkipSpinup  <- skipSpinup
        localModifyOut   <- modifyOut
        localOutVars     <- outVars
        localPPS         <- postProcString
        localVarIndex    <- varIndex
        localSkipZero    <- skipZero

        Y <- tryCatch(
            foreach::foreach(
                i         = seq_len(nRuns),
                .packages = "RBBGCMuso",
                .combine  = c,
                .export   = character(0)
            ) %dopar% {
                iterDir <- file.path(tempdir(),
                                     paste0("sobol_", i, "_", Sys.getpid()))
                dir.create(iterDir, showWarnings = FALSE, recursive = TRUE)

                file.copy(
                    list.files(localSettings$inputLoc,
                               full.names = TRUE, recursive = FALSE),
                    iterDir, recursive = TRUE
                )

                # Inline path remap (avoids namespace issues in workers)
                oldDir <- normalizePath(localSettings$inputLoc,
                                        winslash = "/", mustWork = FALSE)
                newDir <- normalizePath(iterDir, winslash = "/", mustWork = FALSE)
                remap <- function(ps){
                    if(is.null(ps)) return(NULL)
                    vapply(ps, function(p)
                        gsub(oldDir, newDir,
                             normalizePath(p, winslash = "/", mustWork = FALSE),
                             fixed = TRUE),
                        character(1), USE.NAMES = FALSE)
                }
                iterSettings <- localSettings
                iterSettings$inputLoc  <- newDir
                if(!is.null(iterSettings$outputLoc))
                    iterSettings$outputLoc  <- remap(iterSettings$outputLoc)
                if(!is.null(iterSettings$executable))
                    iterSettings$executable <- remap(iterSettings$executable)
                if(!is.null(iterSettings$epcInput))
                    iterSettings$epcInput   <- remap(iterSettings$epcInput)
                if(!is.null(iterSettings$iniInput))
                    iterSettings$iniInput   <- remap(iterSettings$iniInput)
                if(!is.null(iterSettings$epc))
                    iterSettings$epc        <- remap(iterSettings$epc)
                if(!is.null(iterSettings$soilFile))
                    iterSettings$soilFile   <- remap(iterSettings$soilFile)

                result <- tryCatch(
                    calibMuso(
                        settings       = iterSettings,
                        parameters     = as.numeric(localSaX[i, ]),
                        silent         = TRUE,
                        skipSpinup     = localSkipSpinup,
                        modifyOriginal = localModifyOut,
                        outVars        = localOutVars,
                        postProcString = localPPS
                    ),
                    error = function(e) NA
                )
                unlink(iterDir, recursive = TRUE)

                if(length(dim(result)) >= 1){
                    val <- funct[[localVarIndex]](result[, localVarIndex])
                    if(localSkipZero && !is.na(val) && val == 0) val <- NA
                    val
                } else {
                    NA_real_
                }
            },
            finally = parallel::stopCluster(cl)
        )

    } else {
        # Sequential path
        pb <- txtProgressBar(1, nRuns, style = 3)
        Y  <- numeric(nRuns)
        for(i in seq_len(nRuns)){
            result <- tryCatch(
                calibMuso(
                    settings       = settings,
                    parameters     = as.numeric(sa$X[i, ]),
                    silent         = TRUE,
                    skipSpinup     = skipSpinup,
                    modifyOriginal = modifyOut,
                    outVars        = outVars,
                    postProcString = postProcString
                ),
                error = function(e) NA
            )
            if(length(dim(result)) >= 1){
                val <- funct[[varIndex]](result[, varIndex])
                if(skipZero && !is.na(val) && val == 0) val <- NA
                Y[i] <- val
            } else {
                Y[i] <- NA_real_
            }
            setTxtProgressBar(pb, i)
        }
    }

    # ------------------------------------------------------------------
    # 5. Compute Sobol indices
    # ------------------------------------------------------------------
    sensitivity::tell(sa, Y)

    # sa$S = first-order indices (data.frame: original, bias, std.error,
    #                              min. c.i., max. c.i.)
    # sa$T = total-order indices (same layout)
    S1 <- sa$S
    ST <- sa$T

    results <- data.frame(
        parameter = paramNames,
        S1        = S1[["original"]],
        S1_ci_lo  = S1[["min. c.i."]],
        S1_ci_hi  = S1[["max. c.i."]],
        ST        = ST[["original"]],
        ST_ci_lo  = ST[["min. c.i."]],
        ST_ci_hi  = ST[["max. c.i."]],
        stringsAsFactors = FALSE
    )
    write.csv(results, file = outputFile, row.names = FALSE)

    # ------------------------------------------------------------------
    # 6. Plot S1 and ST side by side with error bars
    # ------------------------------------------------------------------
    plotData <- data.frame(
        parameter = rep(paramNames, 2),
        index     = c(S1[["original"]], ST[["original"]]),
        ci_lo     = c(S1[["min. c.i."]], ST[["min. c.i."]]),
        ci_hi     = c(S1[["max. c.i."]], ST[["max. c.i."]]),
        type      = factor(rep(c("First-order (S1)", "Total-order (ST)"),
                               each = npar),
                           levels = c("First-order (S1)", "Total-order (ST)")),
        stringsAsFactors = FALSE
    )

    p <- ggplot(plotData,
                aes(x = parameter, y = index, fill = type)) +
        geom_bar(stat = "identity",
                 position = position_dodge(width = 0.8),
                 width = 0.7) +
        geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                      position = position_dodge(width = 0.8),
                      width = 0.25) +
        scale_fill_manual(values = c("First-order (S1)" = "steelblue",
                                     "Total-order (ST)" = "tomato"),
                          name = "Index type") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab(NULL) +
        ylab("Sensitivity index") +
        ggtitle(plotTitle)

    print(p)
    ggsave(plotName, dpi = dpi)

    message(sprintf("Results saved to '%s' and '%s'.", outputFile, plotName))
    return(invisible(sa))
}
