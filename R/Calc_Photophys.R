#'LabSTAF NPQ
#' @title Calculate NPQ LabSTAF
#' @param frrf A list of frrf data with components for extracting parameters
#' @return NPQ
#' @export
calc.NPQ <- function(frrf) {
  for (j in seq_along(frrf)) {
    # Find the Fm value where E == 0
    if (!is.null(frrf[[j]]$A) && "E" %in% names(frrf[[j]]$A) && "Fm" %in% names(frrf[[j]]$A)) {
      index <- which(frrf[[j]]$A$E == 0)

      if (length(index) > 0) {
        Fm_0 <- frrf[[j]]$A$Fm[index[1]]  # Take the first occurrence
      } else {
        Fm_0 <- NA  # No valid Fm found for E == 0
      }

      # Compute NPQ
      n <- length(frrf[[j]]$A$E)
      frrf[[j]]$A$NPQ.calc <- numeric(n)

      for (i in seq_len(n)) {
        if (!is.na(Fm_0) && i <= length(frrf[[j]]$A$Fm)) {
          frrf[[j]]$A$NPQ.calc[i] <- (Fm_0 - frrf[[j]]$A$Fm[i]) / frrf[[j]]$A$Fm[i]
        } else {
          frrf[[j]]$A$NPQ.calc[i] <- NA  # Assign NA if Fm_0 is missing
        }
      }
    }
  }
  return(frrf)
}

#' Calc ETR_RCII
#' @title Calculate ETR LabSTAF
#' @param frrf A list of frrf data with components for extracting parameters
#' @return ETR
#' @export
calc.ETR_RCII <- function(frrf) {
  for(j in 1:length(frrf)) {
    for(i in 1:length(frrf[[j]]$A$E)) {
      frrf[[j]]$A$ETR[i] <- frrf[[j]]$A$E[i] * frrf[[j]]$A$SigmaPII[1] *
        (frrf[[j]]$A$Fq_Fm[i]/frrf[[j]]$A$Fq_Fm[1]) * 1 * 0.6022
    }
  }
  return(frrf)
}
