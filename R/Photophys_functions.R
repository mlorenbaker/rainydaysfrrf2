#' Extract Photophys Data from FRRf files
#'
#' These functions will extract several base parameters of FRRf data
#'
#'
# FastOcean Functions #####

#' Sigma
#' @title Extract Sigma
#' @param frrf A list of frrf data with components for extracting parameters
#' @return sigma
#' @export
get.sigma <- function(frrf) {
  sigma.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "Sigma" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        sigma.vector[i] <- frrf[[i]]$A$Sigma[index]  # Case-sensitive check
      }
    }
  }

  return(sigma.vector)
}


#'Fv/Fm
#' @title Extract Fv/Fm Act2Run
#' @param frrf A list of frrf data with components for extracting parameters
#' @return fv/fm
#' @export
get.fvfm <- function(frrf) {
  fv.fm.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "Fv.Fm" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        fv.fm.vector[i] <- frrf[[i]]$A$Fv.Fm[index]  # Case-sensitive check
      }
    }
  }

  return(fv.fm.vector)
}



#'Tau
#' @title Extract Tau
#' @param frrf A list of frrf data with components for extracting parameters
#' @return tau
#' @export
get.tau <- function(frrf) {
  tau.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "TauES" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        tau.vector[i] <- frrf[[i]]$A$TauES[index]  # Case-sensitive check
      }
    }
  }

  return(tau.vector)
}


#'Ek
#' @title Extract Ek
#' @param frrf A list of frrf data with components for extracting parameters
#' @return Ek
#' @export
get.Ek <- function(frrf) {
  Ek.vector <- rep(0, length(frrf))
  for (i in seq_along(frrf)) {
    Ek.vector[i] <- frrf[[i]]$S$Ek[1]
  }
  return(Ek.vector)
}

#'Alpha
#' @title Extract Alpha
#' @param frrf A list of frrf data with components for extracting parameters
#' @return Alpha
#' @export
get.Alpha <- function(frrf) {
  Alpha.vector <- rep(0, length(frrf))
  for (i in seq_along(frrf)) {
    Alpha.vector[i] <- frrf[[i]]$S$Alpha[1]
  }
  return(Alpha.vector)
}

#'NSV
#' @title Extract NSV
#' @param frrf A list of frrf data with components for extracting parameters
#' @return NSV
#' @export
get.NSV <- function(frrf) {
  NSV.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "NSV" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        NSV.vector[i] <- frrf[[i]]$A$NSV[index]  # Case-sensitive check
      }
    }
  }

  return(NSV.vector)
}

#'NPQ
#' @title Extract NPQ
#' @param frrf A list of frrf data with components for extracting parameters
#' @return NPQ
#' @export
get.NPQ <- function(frrf) {
  NPQ.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "NPQ" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        NPQ.vector[i] <- frrf[[i]]$A$NPQ[index]  # Case-sensitive check
      }
    }
  }

  return(NPQ.vector)
}

#'Chlorophyll a
#' @title Extract Chla
#' @param frrf A list of frrf data with components for extracting parameters
#' @return Chla
#' @export
get.Chl <- function(frrf) {
  Chl.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "Chl" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        Chl.vector[i] <- frrf[[i]]$A$Chl[index]  # Case-sensitive check
      }
    }
  }

  return(Chl.vector)
}

#'Pm
#' @title Extract Pm
#' @param frrf A list of frrf data with components for extracting parameters
#' @return Pm
#' @export
get.Pm <- function(frrf) {
  Pm.vector <- rep(0, length(frrf))
  for (i in seq_along(frrf)) {
    Pm.vector[i] <- frrf[[i]]$S$Pm[1]
  }
  return(Pm.vector)
}

#'Fm
#' @title Extract Fm
#' @param frrf A list of frrf data with components for extracting parameters
#' @return Fm
#' @export
get.Fm <- function(frrf) {
  Fm.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "Fm" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        Fm.vector[i] <- frrf[[i]]$A$Fm[index]  # Case-sensitive check
      }
    }
  }

  return(Fm.vector)
}

#'Fo
#' @title Extract Fo
#' @param frrf A list of frrf data with components for extracting parameters
#' @return Fo
#' @export
get.Fo <- function(frrf) {
  Fo.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "Fo" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        Fo.vector[i] <- frrf[[i]]$A$Fo[index]  # Case-sensitive check
      }
    }
  }

  return(Fo.vector)
}

#'p
#' @title Extract p
#' @param frrf A list of frrf data with components for extracting parameters
#' @return p
#' @export
get.p <- function(frrf) {
  p.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "p" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        p.vector[i] <- frrf[[i]]$A$p[index]  # Case-sensitive check
      }
    }
  }

  return(p.vector)
}

#'C
#' @title Extract C
#' @param frrf A list of frrf data with components for extracting parameters
#' @return C
#' @export
get.C <- function(frrf) {
  C.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "C" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        C.vector[i] <- frrf[[i]]$A$C[index]  # Case-sensitive check
      }
    }
  }

  return(C.vector)
}



#'QR
#' @title Extract QR
#' @param frrf A list of frrf data with components for extracting parameters
#' @return QR
#' @export
get.QR <- function(frrf) {
  QR.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "QR" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        QR.vector[i] <- frrf[[i]]$A$QR[index]  # Case-sensitive check
      }
    }
  }

  return(QR.vector)
}



# LabSTAF Functions #####


#'LabSTAF Fo
#' @title Extract Fo LabSTAF
#' @param frrf A list of frrf data with components for extracting parameters
#' @return Fo
#' @export
get.F <- function(frrf) {
  F.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "F" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        F.vector[i] <- frrf[[i]]$A$`F`[index]  # Case-sensitive check
      }
    }
  }

  return(F.vector)
}


#'LabSTAF NPQ
#' @title extract calculated NPQ from LabSTAF
#' @param frrf A list of frrf data with components for extracting parameters
#' @return NPQ labSTAF
#' @export
get.NPQ.LS <- function(frrf) {
  NPQ.vector <- rep(NA, length(frrf))

  for (i in seq_along(frrf)) {
    NPQ.vector[i] <- frrf[[i]]$A$NPQ.calc[3]

  }
  return(NPQ.vector)
}

#'Fv/Fm
#' @title Extract Fv/Fm LabSTAF
#' @param frrf A list of frrf data with components for extracting parameters
#' @return fv/fm
#' @export
get.fvfm.LS <- function(frrf) {
  fqfm.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "Fq_Fm" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        fqfm.vector[i] <- frrf[[i]]$A$Fq_Fm[index]  # Case-sensitive check
      }
    }
  }

  return(fqfm.vector)
}

#' SigmaPII LABSTAF
#' @title Extract SigmaPII
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return sigmaPII
#' @export
get.sigmapii <- function(frrf) {
  sigmapii.vector <- rep(NA, length(frrf))  # Initialize with NA

  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "SigmaPII" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        sigmapii.vector[i] <- frrf[[i]]$A$SigmaPII[index]  # Case-sensitive check
      }
    }
  }

  return(sigmapii.vector)
}

#' F LABSTAF
#' @title Calc and extract Fo
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return Fo
#' @export
get.F <- function(frrf) {
  for (j in 1:length(frrf)) {
    for (i in 1:length(frrf[[j]]$A$E)) {
      frrf[[j]]$A$Fo[i] <- frrf[[j]]$A$`F`[1] / (frrf[[j]]$A$Fq_Fm[1] + (frrf[[j]]$A$`F`[1]/frrf[[j]]$A$Fm[i]))
    }
  }
  F.vector <- rep(NA, length(frrf))
  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "Fo" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        F.vector[i] <- frrf[[i]]$A$Fo[index]  # Case-sensitive check
      }
    }
  }
  return(F.vector)
}

#' Fv LABSTAF
#' @title Calc and extract Fv
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return Fv
#' @export
get.Fv <- function(frrf) {
  for (j in 1:length(frrf)) {
    for (i in 1:length(frrf[[j]]$A$E)) {
      frrf[[j]]$A$Fo[i] <- frrf[[j]]$A$`F`[1] / (frrf[[j]]$A$Fq_Fm[1] + (frrf[[j]]$A$`F`[1]/frrf[[j]]$A$Fm[i]))
    }
  }

  for (j in 1:length(frrf)) {
    for (i in 1:length(frrf[[j]]$A$E)) {
      frrf[[j]]$A$Fv[i] <- frrf[[j]]$A$Fm[i] - frrf[[j]]$A$Fo[i]
    }
  }
  Fv.vector <- rep(NA, length(frrf))
  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "Fv" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        Fv.vector[i] <- frrf[[i]]$A$Fv[index]  # Case-sensitive check
      }
    }
  }
  return(Fv.vector)
}

#' NSV LABSTAF
#' @title Calc and extract NSV
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return NSV
#' @export
get.NSV <- function(frrf) {
  # calc F
  for (j in 1:length(frrf)) {
    for (i in 1:length(frrf[[j]]$A$E)) {
      frrf[[j]]$A$Fo[i] <- frrf[[j]]$A$`F`[1] / (frrf[[j]]$A$Fq_Fm[1] + (frrf[[j]]$A$`F`[1]/frrf[[j]]$A$Fm[i]))
    }
  }
  # calc Fv
  for (j in 1:length(frrf)) {
    for (i in 1:length(frrf[[j]]$A$E)) {
      frrf[[j]]$A$Fv[i] <- frrf[[j]]$A$Fm[i] - frrf[[j]]$A$Fo[i]
    }
  }
  # Calc NSV
  for (j in 1:length(frrf)) {
    for (i in 1:length(frrf[[j]]$A$E)) {
      frrf[[j]]$A$NSV[i] <- frrf[[j]]$A$Fo[i] / frrf[[j]]$A$Fv[i]
    }
  }
  NSV.vector <- rep(NA, length(frrf))
  for (i in seq_along(frrf)) {
    if (!is.null(frrf[[i]]$A) && "E" %in% names(frrf[[i]]$A) && "NSV" %in% names(frrf[[i]]$A)) {
      index <- which(frrf[[i]]$A$E == 0)  # Find index where E == 0

      if (length(index) > 0) {  # Ensure index exists
        NSV.vector[i] <- frrf[[i]]$A$NSV[index]  # Case-sensitive check
      }
    }
  }
  return(NSV.vector)

}

# Universal Functions #####

#'File
#' @title Extract File name
#' @param frrf A list of frrf data with components for extracting parameters
#' @return File name
#' @export
get.File <- function(frrf) {
  File.vector <- rep(0, length(frrf))
  for (i in seq_along(frrf)) {
    File.vector[i] <- frrf[[i]]$File
  }
  return(File.vector)
}


#'JVPIIm
#' @title Extract JVPIIm
#' @param frrf A list of frrf data with components for extracting parameters
#' @return JVPIIm
#' @export
get.JVPIIm <- function(frrf) {
  JVPIIm.vector <- rep(0, length(frrf))
  for (i in seq_along(frrf)) {
    JVPIIm.vector[i] <- max(frrf[[i]]$A$JVPII)
  }
  return(JVPIIm.vector)
}

