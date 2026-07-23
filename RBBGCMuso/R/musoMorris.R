#' musoMorris
#'
#' Morris (Elementary Effects) sensitivity screening for Biome-BGCMuSo.
#'
#' The Morris method is designed specifically for parameter screening — it
#' identifies which parameters are influential enough to include in calibration
#' and which can be fixed at default values. It requires only
#' \code{r * (p + 1)} model runs, making it far more efficient than
#' variance-based methods for large parameter sets.
#'
#' Two indices are computed per parameter:
#' \describe{
#'   \item{\strong{μ* (mu.star)}}{Mean absolute elementary effect. The primary
#'     ranking criterion — higher means more influential. Use this to decide
#'     which parameters to carry into calibration.}
#'   \item{\strong{σ (sigma)}}{Standard deviation of elementary effects.
#'     Signals nonlinearity or interactions. A parameter with high σ relative
#'     to μ* has an effect that depends strongly on the values of other
#'     parameters.}
#' }
#'
#' \strong{Multi-output support:} Pass a vector to \code{varIndex} (e.g.
#' \code{c(1, 2, 3)} for GPP, RECO, ET) to screen all targets in a single
#' model run set. Each output gets its own μ*/σ plot. A parameter is flagged
#' as influential if it scores highly for \emph{any} output (union criterion),
#' which is the recommended approach for calibration parameter selection.
#'
#' \strong{Allocation constraints (residual adjustment):}
#' Morris OAT perturbations are independent by design, which normally violates
#' allocation sum-to-1 constraints. Use \code{constraintGroups} to enable
#' residual adjustment: one parameter in each constrained group is designated
#' the \emph{residual} and its value is automatically recomputed so the group
#' always sums to \code{targetSum}. Every design point remains feasible and
#' well-defined elementary effects are computed for all non-residual parameters.
#' The residual parameter still appears in results but its elementary effect
#' reflects the balance rather than a fully independent perturbation — choose
#' the dominant allocation fraction (e.g. dead wood stem) as the residual.
#' Design points where the implied residual falls outside its \code{[min, max]}
#' bounds are flagged as infeasible and returned as NA.
#'
#' @param parameters Dataframe or path to CSV with columns: name, EPC row
#'   number, min, max. Same format as \code{musoMonte}.
#' @param settings Settings list from \code{setupMuso()}. Generated
#'   automatically if NULL.
#' @param r Number of trajectories. More gives more stable μ*/σ estimates.
#'   Default 20. Use 10 for a quick screen, 50 for publication quality.
#' @param levels Number of grid levels for the Morris OAT design. Default 5.
#' @param grid.jump Step size within the grid. Default \code{floor(levels/2)}.
#' @param varIndex Integer or integer vector of output variable indices to
#'   screen (1-based). E.g. \code{c(1, 2, 3)} runs GPP, RECO, and ET
#'   simultaneously. Default 1.
#' @param outVars Character vector of output variable names. If NULL, uses the
#'   first output set from the settings INI file.
#' @param fun Aggregation function applied to each model time series
#'   (default: \code{mean}).
#' @param outLoc Directory for intermediate per-run CSV files.
#' @param outputFile Path for the results CSV containing μ, μ*, and σ for
#'   every parameter × output combination.
#' @param plotName Base path for the μ*/σ scatter plot PNG. When multiple
#'   outputs are requested, the output variable name is appended automatically
#'   (e.g. \code{morris_GPP.png}, \code{morris_RECO.png}).
#' @param plotTitle Base title for the plots.
#' @param influenceThreshold Parameters with μ* above this quantile (default
#'   0.75) are highlighted in the plot as influential. Set to NULL to disable.
#' @param skipSpinup Skip spinup phase. Default TRUE.
#' @param skipZero Replace zero model outputs with NA. Default TRUE.
#' @param postProcString Optional post-processing expression string.
#' @param modifyOut Whether to modify the original EPC file.
#' @param parallel If TRUE, model runs are parallelised using doParallel +
#'   foreach. Default FALSE.
#' @param nCores Number of parallel workers. NULL uses detectCores() - 1.
#' @param dpi Resolution for saved plots. Default 300.
#' @param constraintGroups Optional list of allocation constraint group
#'   definitions for enforcing sum-to-1 constraints via residual adjustment.
#'   Each element must be a named list with:
#'   \describe{
#'     \item{\code{members}}{Character vector of parameter names sharing the
#'       constraint (must all appear in \code{parameters}).}
#'     \item{\code{residual}}{Name of the single parameter that absorbs the
#'       balance to maintain \code{targetSum}. Typically the largest allocation
#'       fraction. This parameter still appears in results but its elementary
#'       effect is partially confounded with the others.}
#'     \item{\code{targetSum}}{Numeric. The value all \code{members} must sum
#'       to. For a fully-free group use 1.0. If other allocations are fixed
#'       outside the SA set, subtract them: e.g. if live+dead coarse root are
#'       fixed at 0.11, use \code{targetSum = 0.89}.}
#'   }
#'   Default NULL (no constraint enforcement).
#'
#'   \strong{Example for Norway spruce phase-1 allocation:}
#'   \preformatted{
#'   constraintGroups = list(
#'     list(
#'       members   = c("leafAlloc_ph1", "frootAlloc_ph1",
#'                     "liveWoodAlloc_ph1", "deadWoodAlloc_ph1"),
#'       residual  = "deadWoodAlloc_ph1",
#'       targetSum = 0.89  # 1 - 0.01 (live CR) - 0.10 (dead CR)
#'     )
#'   )
#'   }
#' @importFrom ggplot2 ggplot aes geom_point geom_text theme element_text
#'   xlab ylab ggtitle ggsave theme_bw annotate scale_colour_manual
#' @export

musoMorris <- function(parameters         = NULL,
                       settings           = NULL,
                       r                  = 20,
                       levels             = 5,
                       grid.jump          = NULL,
                       varIndex           = 1,
                       outVars            = NULL,
                       fun                = mean,
                       outLoc             = "./calib",
                       outputFile         = "morris_indices.csv",
                       plotName           = "morris.png",
                       plotTitle          = "Morris Sensitivity",
                       influenceThreshold = 0.75,
                       skipSpinup         = TRUE,
                       skipZero           = TRUE,
                       postProcString     = NULL,
                       modifyOut          = TRUE,
                       parallel           = FALSE,
                       nCores             = NULL,
                       dpi                = 300,
                       constraintGroups   = NULL){

    # ------------------------------------------------------------------
    # 0. Dependencies
    # ------------------------------------------------------------------
    if(!requireNamespace("sensitivity", quietly = TRUE))
        stop("Package 'sensitivity' is required. Install with: install.packages('sensitivity')")
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
            error = function(e) stop("Provide a parameters dataframe or path to parameters.csv")
        )
    } else if(!is.list(parameters) && !is.matrix(parameters)){
        parameters <- tryCatch(
            read.csv(parameters, stringsAsFactors = FALSE),
            error = function(e) stop("Cannot read parameters file")
        )
    }

    paramOrder  <- order(parameters[, 2])
    parameters  <- parameters[paramOrder, ]
    paramNames  <- gsub("([\\s]|\\-epc)", "", parameters[, 1], perl = TRUE)
    npar        <- nrow(parameters)

    # Parameter bounds matrix (npar rows x 2 cols: min, max)
    paramBounds <- as.matrix(parameters[, 3:4, drop = FALSE])

    # ------------------------------------------------------------------
    # 1b. Validate constraintGroups
    # ------------------------------------------------------------------
    if(!is.null(constraintGroups)){
        for(k in seq_along(constraintGroups)){
            grp <- constraintGroups[[k]]
            if(is.null(grp$members))
                stop(sprintf("constraintGroups[[%d]] must have a 'members' element.", k))
            if(is.null(grp$residual))
                stop(sprintf("constraintGroups[[%d]] must have a 'residual' element.", k))
            if(is.null(grp$targetSum))
                stop(sprintf(paste0(
                    "constraintGroups[[%d]] must have a 'targetSum' element.\n",
                    "  Example: for spruce ph1 with fixed coarse-root allocations of 0.11:",
                    " targetSum = 0.89"), k))
            unknown <- setdiff(grp$members, paramNames)
            if(length(unknown) > 0)
                warning(sprintf(
                    "constraintGroups[[%d]]: parameter(s) not found in parameter set: %s",
                    k, paste(unknown, collapse = ", ")))
            if(!(grp$residual %in% grp$members))
                stop(sprintf(
                    "constraintGroups[[%d]]$residual '%s' is not listed in members.",
                    k, grp$residual))
        }
        message(sprintf(
            "Constraint enforcement active: %d group(s), residual(s): %s",
            length(constraintGroups),
            paste(vapply(constraintGroups, `[[`, character(1), "residual"), collapse = ", ")
        ))
    }

    # ------------------------------------------------------------------
    # 2. Settings and output variables
    # ------------------------------------------------------------------
    if(is.null(settings)) settings <- setupMuso()
    settings$calibrationPar <- parameters[, 2]

    if(is.null(outVars)){
        outVarNames <- settings$outputVars[[1]]
    } else {
        outVarNames <- sapply(outVars, musoMapping)
    }
    if(!is.null(postProcString)){
        outVarNames <- c(outVarNames,
                         gsub("\\s", "", unlist(strsplit(postProcString, "<-"))[1]))
    }

    if(!is.list(fun)) funct <- rep(list(fun), length(outVarNames)) else funct <- fun

    varIndex <- as.integer(varIndex)
    nTargets <- length(varIndex)
    if(is.null(grid.jump)) grid.jump <- floor(levels / 2)

    # ------------------------------------------------------------------
    # 3. Build Morris design
    # ------------------------------------------------------------------
    sa <- sensitivity::morris(
        model  = NULL,
        factors = paramNames,
        r      = r,
        design = list(type = "oat", levels = levels, grid.jump = grid.jump),
        binf   = parameters[, 3],
        bsup   = parameters[, 4],
        scale  = TRUE
    )

    nRuns <- nrow(sa$X)
    message(sprintf(
        "musoMorris: %d trajectories x %d parameters = %d model runs  |  outputs: %s",
        r, npar, nRuns,
        paste(outVarNames[varIndex], collapse = ", ")
    ))

    # ------------------------------------------------------------------
    # 4. Constraint enforcement helper (used in sequential path)
    #
    # Given a named parameter vector, applies residual adjustment for each
    # constraint group: the residual parameter is set to
    #   targetSum - sum(other members)
    # Returns the adjusted vector, or NULL if the residual falls outside
    # its [min, max] bounds (infeasible design point).
    # ------------------------------------------------------------------
    enforceConstraints <- function(params, cGroups, pNames, pBounds){
        for(grp in cGroups){
            memberNames  <- grp$members
            residualName <- grp$residual
            targetSum    <- grp$targetSum
            otherNames   <- setdiff(memberNames, residualName)

            if(!all(memberNames %in% pNames)) next

            otherSum    <- sum(params[otherNames])
            residualVal <- targetSum - otherSum

            residualRow <- which(pNames == residualName)
            lo <- pBounds[residualRow, 1]
            hi <- pBounds[residualRow, 2]

            if(residualVal < lo || residualVal > hi) return(NULL)

            params[residualName] <- residualVal
        }
        params
    }

    # ------------------------------------------------------------------
    # 5. Run model for every design point
    # ------------------------------------------------------------------

    runModel <- function(i, settingsArg){
        rawParams <- setNames(as.numeric(sa$X[i, ]), paramNames)

        if(!is.null(constraintGroups)){
            rawParams <- enforceConstraints(rawParams, constraintGroups,
                                            paramNames, paramBounds)
            if(is.null(rawParams)) return(rep(NA_real_, nTargets))
        }

        result <- tryCatch(
            calibMuso(
                settings       = settingsArg,
                parameters     = as.numeric(rawParams),
                silent         = TRUE,
                skipSpinup     = skipSpinup,
                modifyOriginal = modifyOut,
                outVars        = outVars,
                postProcString = postProcString
            ),
            error = function(e) NA
        )
        vapply(varIndex, function(j){
            if(length(dim(result)) >= 1){
                val <- funct[[j]](result[, j])
                if(skipZero && !is.na(val) && val == 0) NA_real_ else val
            } else {
                NA_real_
            }
        }, numeric(1))
    }

    if(parallel){
        nCoresActual <- if(is.null(nCores)) max(1L, parallel::detectCores() - 1L) else as.integer(nCores)
        nCoresActual <- min(nCoresActual, nRuns)

        cl <- parallel::makeCluster(nCoresActual)
        doParallel::registerDoParallel(cl)
        `%dopar%` <- foreach::`%dopar%`

        localSettings         <- settings
        localSaX              <- sa$X
        localFunct            <- funct
        localVarIndex         <- varIndex
        localSkipZero         <- skipZero
        localSkipSpin         <- skipSpinup
        localModOut           <- modifyOut
        localOutVars          <- outVars
        localPPS              <- postProcString
        localParamNames       <- paramNames
        localParamBounds      <- paramBounds
        localConstraintGroups <- constraintGroups

        message(sprintf("Running %d iterations across %d cores...", nRuns, nCoresActual))

        Y <- tryCatch(
            foreach::foreach(
                i         = seq_len(nRuns),
                .packages = "RBBGCMuso",
                .combine  = rbind,
                .export   = character(0)
            ) %dopar% {
                # ---- Per-worker temp directory ----
                iterDir <- file.path(tempdir(),
                                     paste0("morris_", i, "_", Sys.getpid()))
                dir.create(iterDir, showWarnings = FALSE, recursive = TRUE)
                file.copy(list.files(localSettings$inputLoc,
                                     full.names = TRUE, recursive = FALSE),
                          iterDir, recursive = TRUE)

                oldDir <- normalizePath(localSettings$inputLoc, winslash="/", mustWork=FALSE)
                newDir <- normalizePath(iterDir, winslash="/", mustWork=FALSE)
                remap  <- function(ps){
                    if(is.null(ps)) return(NULL)
                    vapply(ps, function(p)
                        gsub(oldDir, newDir,
                             normalizePath(p, winslash="/", mustWork=FALSE),
                             fixed=TRUE),
                        character(1), USE.NAMES=FALSE)
                }
                iterSettings <- localSettings
                iterSettings$inputLoc  <- newDir
                if(!is.null(iterSettings$outputLoc))  iterSettings$outputLoc  <- remap(iterSettings$outputLoc)
                if(!is.null(iterSettings$executable)) iterSettings$executable <- remap(iterSettings$executable)
                if(!is.null(iterSettings$epcInput))   iterSettings$epcInput   <- remap(iterSettings$epcInput)
                if(!is.null(iterSettings$iniInput))   iterSettings$iniInput   <- remap(iterSettings$iniInput)
                if(!is.null(iterSettings$epc))        iterSettings$epc        <- remap(iterSettings$epc)
                if(!is.null(iterSettings$soilFile))   iterSettings$soilFile   <- remap(iterSettings$soilFile)

                # ---- Constraint enforcement (inlined for worker namespace) ----
                rawParams <- setNames(as.numeric(localSaX[i, ]), localParamNames)
                feasible  <- TRUE

                if(!is.null(localConstraintGroups)){
                    for(grp in localConstraintGroups){
                        memberNames  <- grp$members
                        residualName <- grp$residual
                        targetSumVal <- grp$targetSum
                        otherNames   <- setdiff(memberNames, residualName)
                        if(!all(memberNames %in% localParamNames)) next
                        otherSum    <- sum(rawParams[otherNames])
                        residualVal <- targetSumVal - otherSum
                        residualRow <- which(localParamNames == residualName)
                        lo <- localParamBounds[residualRow, 1]
                        hi <- localParamBounds[residualRow, 2]
                        if(residualVal < lo || residualVal > hi){
                            feasible <- FALSE
                            break
                        }
                        rawParams[residualName] <- residualVal
                    }
                }

                if(!feasible){
                    rep(NA_real_, length(localVarIndex))
                } else {
                    result <- tryCatch(
                        calibMuso(settings=iterSettings, parameters=as.numeric(rawParams),
                                  silent=TRUE, skipSpinup=localSkipSpin,
                                  modifyOriginal=localModOut, outVars=localOutVars,
                                  postProcString=localPPS),
                        error=function(e) NA
                    )
                    unlink(iterDir, recursive=TRUE)

                    vapply(localVarIndex, function(j){
                        if(length(dim(result)) >= 1){
                            val <- localFunct[[j]](result[, j])
                            if(localSkipZero && !is.na(val) && val == 0) NA_real_ else val
                        } else NA_real_
                    }, numeric(1))
                }
            },
            finally = parallel::stopCluster(cl)
        )
        message("All iterations complete.")

    } else {
        pb <- txtProgressBar(1, nRuns, style = 3)
        Y  <- matrix(NA_real_, nrow = nRuns, ncol = nTargets)
        for(i in seq_len(nRuns)){
            Y[i, ] <- runModel(i, settings)
            setTxtProgressBar(pb, i)
        }
    }

    # Ensure Y is always a matrix (foreach rbind can drop dim for nTargets=1)
    if(!is.matrix(Y)) Y <- matrix(Y, ncol = nTargets)

    # ------------------------------------------------------------------
    # 6. Compute Morris indices
    # ------------------------------------------------------------------

    # Report NA rate
    naFrac <- mean(is.na(Y))
    nSteps <- npar + 1L   # points per Morris OAT trajectory
    if(naFrac > 0){
        message(sprintf(
            "Note: %.1f%% of model runs returned NA (%d / %d).",
            naFrac * 100, sum(is.na(Y)), length(Y)
        ))
    }
    if(naFrac == 1){
        stop(paste0(
            "All model runs returned NA — Morris indices cannot be computed.\n",
            "Possible causes:\n",
            "  - Model executable not found or not executable in worker temp directories\n",
            "  - constraintGroups bounds too narrow (all design points infeasible)\n",
            "  - settings paths not resolving correctly in parallel workers\n",
            "Run a single sequential test with parallel=FALSE and check for errors."
        ))
    }

    # ------------------------------------------------------------------
    # Compute elementary effects (EEs) manually from consecutive trajectory
    # rows rather than relying on sensitivity::tell().
    #
    # Reason: tell() with multi-output Y is unreliable across package versions
    # (may return 0-row index matrices), and it uses mean(na.rm=FALSE) so a
    # single NA anywhere in a trajectory zeroes out that parameter's indices.
    #
    # Manual approach:
    #   EE(traj, j) = ΔY / ΔX_j   where j is the parameter changed at that step
    # Aggregation uses na.rm=TRUE so partial trajectory failures don't
    # collapse everything to NA.
    # ------------------------------------------------------------------
    ee_arr <- array(NA_real_, dim = c(r, npar, nTargets))

    for(traj in seq_len(r)){
        rows  <- seq.int((traj - 1L) * nSteps + 1L, traj * nSteps)
        Xtraj <- as.matrix(sa$X[rows, , drop = FALSE])
        Ytraj <- Y[rows, , drop = FALSE]

        for(step in seq_len(npar)){
            deltaX <- Xtraj[step + 1L, ] - Xtraj[step, ]
            j      <- which.max(abs(deltaX))      # which parameter was perturbed
            if(abs(deltaX[j]) < .Machine$double.eps * 1e6) next  # degenerate step

            deltaY <- Ytraj[step + 1L, ] - Ytraj[step, ]
            ee_arr[traj, j, ] <- as.numeric(deltaY) / deltaX[j]
        }
    }

    # Aggregate: mean, mean(|EE|), sd  —  with na.rm=TRUE
    mu_mat     <- matrix(NA_real_, npar, nTargets)
    mustar_mat <- matrix(NA_real_, npar, nTargets)
    sigma_mat  <- matrix(NA_real_, npar, nTargets)
    for(k in seq_len(nTargets)){
        for(j in seq_len(npar)){
            ees <- ee_arr[, j, k]
            if(any(!is.na(ees))){
                mu_mat[j, k]     <- mean(ees,       na.rm = TRUE)
                mustar_mat[j, k] <- mean(abs(ees),  na.rm = TRUE)
                sigma_mat[j, k]  <- sd(ees,         na.rm = TRUE)
            }
        }
    }

    # Call tell() purely to complete the sa object (sets sa$y).
    # Wrap in tryCatch — if the package version has a multi-output bug it won't crash.
    tryCatch(sensitivity::tell(sa, Y), error = function(e) NULL)

    allResults <- lapply(seq_len(nTargets), function(k){
        data.frame(
            parameter = paramNames,
            output    = outVarNames[varIndex[k]],
            mu        = mu_mat[, k],
            mu.star   = mustar_mat[, k],
            sigma     = sigma_mat[, k],
            stringsAsFactors = FALSE
        )
    })

    resultDf <- do.call(rbind, allResults)

    # Flag residual parameters so the user knows their EE interpretation differs
    if(!is.null(constraintGroups)){
        residuals <- vapply(constraintGroups, `[[`, character(1), "residual")
        resultDf$residual_param <- resultDf$parameter %in% residuals
    }

    write.csv(resultDf, file = outputFile, row.names = FALSE)

    # ------------------------------------------------------------------
    # 7. Plot: μ*/σ scatter per output variable
    #    Residual parameters are drawn with a different shape (asterisk)
    #    and a caption explains their interpretation.
    # ------------------------------------------------------------------
    useRepel <- requireNamespace("ggrepel", quietly = TRUE)

    residualNames <- if(!is.null(constraintGroups)){
        vapply(constraintGroups, `[[`, character(1), "residual")
    } else character(0)

    for(k in seq_len(nTargets)){
        df      <- allResults[[k]]
        varName <- outVarNames[varIndex[k]]

        ext   <- tools::file_ext(plotName)
        base  <- tools::file_path_sans_ext(plotName)
        pFile  <- if(nTargets > 1) paste0(base, "_", varName, ".", ext) else plotName
        pTitle <- if(nTargets > 1) paste0(plotTitle, "  —  ", varName) else plotTitle

        if(!is.null(influenceThreshold)){
            thresh         <- quantile(df$mu.star, influenceThreshold, na.rm = TRUE)
            df$influential <- df$mu.star >= thresh
        } else {
            df$influential <- TRUE
        }

        df$isResidual <- df$parameter %in% residualNames

        p <- ggplot(df, aes(x = mu.star, y = sigma,
                            colour = influential, label = parameter)) +
            ggplot2::geom_point(aes(shape = isResidual), size = 2.5) +
            ggplot2::scale_shape_manual(
                values = c("FALSE" = 16, "TRUE" = 8),
                labels = c("FALSE" = "Free parameter", "TRUE" = "Residual (constrained)"),
                name   = NULL
            ) +
            {if(useRepel)
                ggrepel::geom_text_repel(
                    data   = df[df$influential, ],
                    size   = 3,
                    colour = "black"
                )
             else
                ggplot2::geom_text(
                    data   = df[df$influential, ],
                    size   = 3,
                    vjust  = -0.7,
                    colour = "black"
                )
            } +
            scale_colour_manual(
                values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
                labels = c("TRUE" = sprintf("Top %d%%", round((1 - influenceThreshold) * 100)),
                           "FALSE" = "Low influence"),
                name   = NULL
            ) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
            ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
            xlab(expression(mu * "*  (mean |EE|)")) +
            ylab(expression(sigma * "  (std dev EE)")) +
            ggtitle(pTitle) +
            theme_bw() +
            theme(legend.position = "bottom")

        if(length(residualNames) > 0){
            p <- p + ggplot2::labs(
                caption = paste0(
                    "* Residual parameter(s): ",
                    paste(residualNames, collapse = ", "),
                    " — elementary effect reflects balance, not an independent perturbation"
                )
            )
        }

        print(p)
        ggsave(pFile, dpi = dpi)
        message(sprintf("Plot saved: %s", pFile))
    }

    message(sprintf("Results saved to '%s'.", outputFile))
    return(invisible(sa))
}
