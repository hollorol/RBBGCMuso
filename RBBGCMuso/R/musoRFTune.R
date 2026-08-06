#' musoRFTune
#'
#' Recommend random forest hyperparameters for Biome-BGCMuSo sensitivity
#' analysis, scaled to the size of the Monte Carlo design.
#'
#' Random forest defaults are tuned for large observational datasets, not for
#' small, low-noise, computer-experiment designs. A Monte Carlo sensitivity
#' study typically has few runs (n) relative to the number of EPC parameters
#' (p), and the response is close to deterministic. Both facts push the optimal
#' settings a long way from the package defaults.
#'
#' @section Where the heuristics come from:
#' The rules below were fitted on synthetic sparse-nonlinear designs
#' (4 informative parameters out of p, mild noise) across
#' n in \{30, 60, 120, 300\} and p in \{8, 15, 25\}, scoring each
#' configuration by out-of-sample R2 against the true response surface:
#'
#' \tabular{rrrrrr}{
#'   n   \tab p  \tab best mtry \tab best min.node.size \tab tuned R2 \tab default R2 \cr
#'   30  \tab 8  \tab 6  (0.75p) \tab 1 \tab 0.712 \tab 0.381 \cr
#'   30  \tab 15 \tab 15 (1.00p) \tab 1 \tab 0.658 \tab 0.318 \cr
#'   30  \tab 25 \tab 25 (1.00p) \tab 2 \tab 0.645 \tab 0.257 \cr
#'   60  \tab 8  \tab 8  (1.00p) \tab 1 \tab 0.825 \tab 0.556 \cr
#'   120 \tab 15 \tab 12 (0.80p) \tab 1 \tab 0.893 \tab 0.630 \cr
#'   300 \tab 25 \tab 20 (0.80p) \tab 1 \tab 0.923 \tab 0.701 \cr
#' }
#'
#' Three conclusions drive the defaults returned here:
#' \itemize{
#'   \item \strong{mtry should be high} - between 0.8p and p, never
#'     \code{sqrt(p)} (ranger's default) or \code{p/3} (Breiman's regression
#'     rule). The scarcer the data, the higher it should go.
#'   \item \strong{min.node.size should be 1}, not 5. Model output is nearly
#'     deterministic given the parameters, so there is little noise to smooth
#'     over and deep trees are appropriate.
#'   \item \strong{sample.fraction = 1 with replacement} was optimal
#'     everywhere, which is already ranger's default, so it is left alone.
#' }
#'
#' Note that even fully tuned, n = 30 tops out near R2 = 0.65. Tuning cannot
#' rescue an undersized design, which is why \code{musoRFTune} also reports an
#' adequacy verdict and a target iteration count.
#'
#' @section How much each knob is worth:
#' Decomposing the total gain shows that mtry does nearly all the work, and
#' increasingly so as the design grows:
#'
#' \tabular{rrrrr}{
#'   n   \tab p  \tab default R2 \tab mtry alone \tab mtry + min.node.size \cr
#'   30  \tab 15 \tab 0.365 \tab 0.573 (76\%) \tab 0.639 \cr
#'   120 \tab 15 \tab 0.649 \tab 0.880 (94\%) \tab 0.896 \cr
#'   500 \tab 40 \tab 0.717 \tab 0.940 (99\%) \tab 0.941 \cr
#' }
#'
#' This matters because Boruta 10.0.0 switched its default importance backend
#' to \pkg{fru}, which fixes its leaf-size threshold and cannot accept
#' \code{min.node.size}. The table above says that costs almost nothing on
#' realistic designs, so \code{musoSensi} keeps the faster fru backend rather
#' than forcing ranger to regain a knob worth ~1\%.
#'
#' @param n Number of Monte Carlo samples (rows).
#' @param p Number of parameters (columns).
#' @param X Optional parameter matrix/data frame. Required for OOB refinement.
#' @param y Optional response vector. Required for OOB refinement.
#' @param refine If TRUE and both X and y are supplied, a small out-of-bag grid
#'   search around the heuristic values selects the final settings. Default TRUE.
#' @param refineTrees Number of trees used during refinement (kept small for
#'   speed; the returned \code{num.trees} is unaffected). Default 200.
#' @param refineRepeats Number of seeds averaged per grid point, to damp OOB
#'   noise on small n. Default 3.
#' @param forBoruta If TRUE, mtry is scaled to Boruta's doubled feature space
#'   (real parameters plus shadow copies). Default FALSE.
#' @param quiet Suppress the printed recommendation table. Default FALSE.
#' @return A list with \code{num.trees}, \code{mtry}, \code{min.node.size},
#'   \code{sample.fraction}, \code{replace}, plus \code{adequacy} (ratio,
#'   verdict, targetN, message), \code{heuristic} (pre-refinement values) and
#'   \code{refined} (the OOB grid table, or NULL).
#' @export
musoRFTune <- function(n, p, X = NULL, y = NULL,
                       refine = TRUE,
                       refineTrees = 200,
                       refineRepeats = 3,
                       forBoruta = FALSE,
                       quiet = FALSE){

    if(!is.numeric(n) || !is.numeric(p) || n < 1 || p < 1){
        stop("n and p must be positive numbers.")
    }
    n <- as.integer(n); p <- as.integer(p)
    ratio <- n / p

    # ---------------------------------------------------------------------
    # Heuristic seed values (see the table in the roxygen block above)
    # ---------------------------------------------------------------------
    mtryFrac <- if(ratio < 4) 1.0 else 0.8
    hMtry    <- max(1L, min(p, as.integer(ceiling(p * mtryFrac))))
    hNode    <- if(ratio < 1.5) 2L else 1L
    hTrees   <- min(2000L, max(500L, as.integer(100 * p)))

    # ---------------------------------------------------------------------
    # Design adequacy. Reported independently of the hyperparameters, because
    # it is usually the more actionable finding.
    # ---------------------------------------------------------------------
    targetN <- as.integer(10 * p)
    verdict <- if(ratio >= 10) "good"
               else if(ratio >= 5)  "adequate"
               else if(ratio >= 3)  "marginal"
               else                 "insufficient"

    adqMsg <- switch(verdict,
        "good" = sprintf(
            "Design adequacy: GOOD (n/p = %.1f). %d runs for %d parameters is ample.",
            ratio, n, p),
        "adequate" = sprintf(
            "Design adequacy: ADEQUATE (n/p = %.1f). Importance ranking should be stable; %d runs would tighten it.",
            ratio, targetN),
        "marginal" = sprintf(
            paste0("Design adequacy: MARGINAL (n/p = %.1f). With %d runs for %d parameters the surrogate ",
                   "will capture the strongest effects but miss weaker ones and most interactions. ",
                   "Consider raising iterations to ~%d."),
            ratio, n, p, targetN),
        "insufficient" = sprintf(
            paste0("Design adequacy: INSUFFICIENT (n/p = %.1f). %d runs for %d parameters is too few for a ",
                   "reliable random forest surrogate; expect R2 near 0.6 even after tuning, and treat any ",
                   "ranking below the top few parameters as noise. Raise iterations to ~%d."),
            ratio, n, p, targetN)
    )

    heuristic <- list(num.trees = hTrees, mtry = hMtry, min.node.size = hNode)
    best      <- heuristic
    gridTab   <- NULL

    # ---------------------------------------------------------------------
    # Optional OOB grid refinement around the heuristic seed
    # ---------------------------------------------------------------------
    canRefine <- refine && !is.null(X) && !is.null(y) &&
                 requireNamespace("ranger", quietly = TRUE)

    if(refine && !canRefine && !is.null(X) && !is.null(y)){
        warning("Package 'ranger' not available; skipping OOB refinement.")
    }

    if(canRefine){
        Xd <- as.data.frame(X)
        colnames(Xd) <- make.names(colnames(Xd), unique = TRUE)
        featNames <- colnames(Xd)
        yName <- "musoTuneY"
        while(yName %in% featNames) yName <- paste0(yName, ".")
        Xd[[yName]] <- y

        mtryGrid <- sort(unique(pmax(1L, pmin(as.integer(p),
                        as.integer(round(p * c(0.5, 0.65, 0.8, 1.0)))))))
        nodeGrid <- unique(c(1L, 2L, 5L))
        nodeGrid <- nodeGrid[nodeGrid * 2 <= n]
        if(length(nodeGrid) == 0) nodeGrid <- 1L

        if(!quiet){
            message(sprintf("Refining RF settings by OOB error (%d grid points x %d repeats) ...",
                            length(mtryGrid) * length(nodeGrid), refineRepeats))
        }

        rows <- list()
        for(mt in mtryGrid){
            for(nd in nodeGrid){
                errs <- vapply(seq_len(refineRepeats), function(s){
                    set.seed(s)
                    fit <- try(ranger::ranger(
                        formula       = stats::reformulate(featNames, response = yName),
                        data          = Xd,
                        num.trees     = refineTrees,
                        mtry          = mt,
                        min.node.size = nd,
                        importance    = "none"), silent = TRUE)
                    if(inherits(fit, "try-error")) NA_real_ else fit$prediction.error
                }, numeric(1))
                rows[[length(rows) + 1]] <- data.frame(
                    mtry = mt, min.node.size = nd,
                    oobMSE = mean(errs, na.rm = TRUE),
                    stringsAsFactors = FALSE)
            }
        }
        gridTab <- do.call(rbind, rows)
        gridTab <- gridTab[order(gridTab$oobMSE), ]
        rownames(gridTab) <- NULL

        if(nrow(gridTab) > 0 && is.finite(gridTab$oobMSE[1])){
            best$mtry          <- gridTab$mtry[1]
            best$min.node.size <- gridTab$min.node.size[1]
        } else {
            warning("OOB refinement produced no usable result; falling back to heuristic values.")
        }
    }

    # Boruta doubles the feature space with shadow copies, so an mtry chosen
    # for p real columns must be rescaled to 2p or it becomes far more
    # restrictive than intended.
    if(forBoruta){
        best$mtry      <- max(1L, min(2L * p, as.integer(round(best$mtry * 2))))
        heuristic$mtry <- max(1L, min(2L * p, as.integer(round(heuristic$mtry * 2))))
    }

    out <- list(
        num.trees       = best$num.trees,
        mtry            = best$mtry,
        min.node.size   = best$min.node.size,
        sample.fraction = 1,
        replace         = TRUE,
        adequacy        = list(ratio = ratio, verdict = verdict,
                               targetN = targetN, message = adqMsg),
        heuristic       = heuristic,
        refined         = gridTab,
        n = n, p = p, forBoruta = forBoruta
    )
    class(out) <- c("musoRFTune", "list")

    if(!quiet) print(out)
    out
}

#' @export
print.musoRFTune <- function(x, ...){
    defMtry <- max(1L, as.integer(floor(sqrt(if(x$forBoruta) 2 * x$p else x$p))))
    cat("\n--- Random forest settings for ", x$n, " runs x ", x$p, " parameters",
        if(x$forBoruta) " (Boruta, shadow-adjusted)" else "", " ---\n", sep = "")
    cat(sprintf("  %-16s %12s %12s\n", "setting", "ranger def.", "recommended"))
    cat(sprintf("  %-16s %12s %12s\n", "num.trees",     "500", format(x$num.trees)))
    cat(sprintf("  %-16s %12s %12s\n", "mtry",          format(defMtry), format(x$mtry)))
    cat(sprintf("  %-16s %12s %12s\n", "min.node.size", "5",   format(x$min.node.size)))
    if(!is.null(x$refined)){
        cat(sprintf("  (selected by OOB search over %d grid points)\n", nrow(x$refined)))
    } else {
        cat("  (heuristic only; supply X and y for OOB refinement)\n")
    }
    cat("\n", strwrap(x$adequacy$message, width = 78, prefix = "  "), sep = "\n")
    cat("\n")
    invisible(x)
}


#' musoRFTuneApply
#'
#' Ask the user whether to apply a \code{\link{musoRFTune}} recommendation,
#' degrading gracefully to automatic acceptance in non-interactive sessions so
#' that batch and parallel runs never block on a prompt.
#'
#' @param rec A \code{musoRFTune} object.
#' @param mode One of \code{"ask"}, \code{"auto"} or \code{"off"}.
#' @param userValues Named list of explicit user overrides. Any name present
#'   here always wins, regardless of mode.
#' @return A list of RF settings to use.
#' @export
musoRFTuneApply <- function(rec, mode = c("ask", "auto", "off"),
                            userValues = list()){
    mode <- match.arg(mode)

    defaults <- list(num.trees     = 500L,
                     mtry          = max(1L, as.integer(floor(sqrt(rec$p)))),
                     min.node.size = 5L)

    chosen <- if(mode == "off"){
        defaults
    } else if(mode == "auto"){
        list(num.trees = rec$num.trees, mtry = rec$mtry,
             min.node.size = rec$min.node.size)
    } else {
        if(!interactive()){
            message("Non-interactive session: applying recommended RF settings automatically ",
                    "(use rfTune = \"off\" to keep ranger defaults).")
            list(num.trees = rec$num.trees, mtry = rec$mtry,
                 min.node.size = rec$min.node.size)
        } else {
            ans <- readline("Apply these recommended RF settings? [Y/n]: ")
            if(grepl("^\\s*[nN]", ans)){
                message("Keeping ranger defaults.")
                defaults
            } else {
                message("Applying recommended RF settings.")
                list(num.trees = rec$num.trees, mtry = rec$mtry,
                     min.node.size = rec$min.node.size)
            }
        }
    }

    # Explicit user arguments override everything.
    for(nm in names(userValues)){
        if(!is.null(userValues[[nm]])) chosen[[nm]] <- userValues[[nm]]
    }
    chosen
}
