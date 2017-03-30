#' modifySL
#' 
#' Recompute the super learner estimator from a \code{SuperLearner} fit
#' based on a restricted set of candidate learners and/or a different loss
#' function than initially fit. 
#' Note that this function does not check for errors in the original SuperLearner
#' fit library as well as the SuperLearner function does. In essence, I'm assuming
#' you've already decided to get rid of the learners that had errors in the initial 
#' fitting procedure for computing this new Super Learner. 
#' 
#' @param fit An object of class \code{SuperLearner}
#' @param newLibrary A character vector of a subset of \code{fit$libraryNames}.
#' @param newMethod A \code{list} structured as a \code{SuperLearner} method. See \code{?write.method.template}.
#' @param obsWeights The weights used to compute the original Super Learner fit. Because these weights are not
#' returned with the \code{SuperLearner} object, they must be input again here. 
#' 

modifySL <- function(fit, newLibrary = fit$libraryNames, newMethod = fit$method,
                     obsWeights = rep(1, length(fit$SL.predict)),verbose = FALSE, 
                     ...){
	call <- match.call()
	#-----------------------------------
	# Code to read in method
	# Taken from SuperLearner function
	#-----------------------------------
	if (is.character(newMethod)) {
	    if (exists(newMethod, mode = "list")) {
	        newMethod <- get(newMethod, mode = "list")
	    }
	    else if (exists(newMethod, mode = "function")) {
	        newMethod <- get(newMethod, mode = "function")()
	    }
	}else if (is.function(newMethod)) {
        newMethod <- newMethod()
    }
    if (!is.list(newMethod)) {
        stop("method is not in the appropriate format. Check out help('method.template')")
    }
    if (!is.null(method$require)) {
        sapply(method$require, function(x) require(force(x), 
            character.only = TRUE))
    }


    #-----------------------------------
    # Modify Z matrix to remove columns 
    #-----------------------------------
    keepInd <- fit$libraryNames %in% newLibrary
    Zmod <- fit$Z[,keepInd,drop=FALSE]

    #-----------------------------------
    # Compute modified coefficients
    #-----------------------------------
    getCoef <- newMethod$computeCoef(Z = Zmod, Y = Y, libraryNames = fit$libraryNames[keepInd], 
                                  obsWeights = obsWeights, control = fit$control, 
                                  verbose = verbose)
    coef <- getCoef$coef
    names(coef) <- fit$libraryNames[keepInd]

    getPred <- newMethod$computePred(predY = fit$library.predict[,keepInd,drop=FALSE], coef = coef, 
            control = control)

    #-----------------------------------
    # format output
    #-----------------------------------
    out <- fit
    # modify the call
    out$call <- call
    out$originalCall <- fit$call
    out$libraryNames <- fit$libraryNames[keepInd]
    out$SL.library <- newLibrary
    out$SL.predict <- getPred
    out$coef <- coef
    out$library.predict <- fit$library.predict[,keepInd,drop=FALSE]
    out$Z <- Zmod 
    out$cvRisk <- getCoef$cvRisk
    out$fitLibrary <- fit$fitLibrary[keepInd]
    out$method <- newMethod
    # figuring out what screens stay in requires a bit of work 
    screenNames <- row.names(fit$whichScreen)
	keepScreen <- rep(NA, length(screenNames))
	for(i in 1:length(screenNames)){
	    keepScreen[i] <- any(grepl(screenNames[i], fit1$libraryNames[keepInd]))
	}
	out$whichScreen <- fit$whichScreen[keepScreen,,drop=FALSE]
	out$metaOptimizer <- getCoef$optimizer
    class(out) <- c("SuperLearner")
    out
}