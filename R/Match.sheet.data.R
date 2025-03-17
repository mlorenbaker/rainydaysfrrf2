#' @title Extract Culture ID
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of each files associated culture ID
#' @export
get.cultureID <- function(frrf) {
  culture.ID.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    culture.ID.vector[i] <- frrf[[i]]$Label$Culture.ID[1]
  }
  return(culture.ID.vector)
}

#' @title Extract Treatments
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of each files associated experimental treatment
#' @export
get.Treatment <- function(frrf) {
  treatment.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    treatment.vector[i] <- frrf[[i]]$Label$Treatment[1]
  }
  return(treatment.vector)
}

#' @title Extract Timepoints
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of each files associated experimental timepoint
#' @export
get.Timepoint <- function(frrf) {
  timepoint.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    timepoint.vector[i] <- frrf[[i]]$Label$Timepoint[1]
  }
  return(timepoint.vector)
}

#' @title Extract LTMHW
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of each files associated experimental LTMHW- ASLO 2025, MLB
#' @export
get.LTMHW <- function(frrf) {
  LTMHW.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    LTMHW.vector[i] <- frrf[[i]]$Label$LTMHW[1]
  }
  return(LTMHW.vector)
}

#' @title Extract Incubator
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of each files associated experimental incubator
#' @export
get.Incubator <- function(frrf) {
  incubator.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    incubator.vector[i] <- frrf[[i]]$Label$Incubator[1]
  }
  return(incubator.vector)
}

#' @title Extract datetime
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of datetime
#' @export
get.datetime <- function(frrf) {
  Datetime.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    Datetime.vector[i] <- frrf[[i]]$Label$Match.time[1]
  }
  return(Datetime.vector)
}

#' @title Extract Cells/mL
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A numeric vector of cells/mL associated with each file
#' @export
get.cellcount <- function(frrf) {
  Cells.vector <- rep(0, length(frrf))
  for (i in seq_along(frrf)) {
    Cells.vector[i] <- frrf[[i]]$Label$Cells[1]
  }
  return(Cells.vector)
}

#' @title Extract Chla extract
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A numeric vector of Chla extracted from harvest
#' @export
get.ChlaExtract <- function(frrf) {
  ChlaExtract.vector <- rep(0, length(frrf))
  for (i in seq_along(frrf)) {
    ChlaExtract.vector[i] <- frrf[[i]]$Label$mol.chlorophyll.a[1]
  }
  return(ChlaExtract.vector)
}

#' @title Extract PAR
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A numeric vector of the PAR data associated with the incubation
#' @export
get.datetime <- function(frrf) {
  PAR.vector <- rep(0, length(frrf))
  for (i in seq_along(frrf)) {
    PAR.vector[i] <- frrf[[i]]$Label$PAR[1]
  }
  return(PAR.vector)
}

#' @title Extract Replicate
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of Replicate
#' @export
get.Replicate <- function(frrf) {
  Replicate.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    Replicate.vector[i] <- frrf[[i]]$Label$Replicate[1]
  }
  return(Replicate.vector)
}

#' @title Extract Intensity
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of Intensity
#' @export
get.Intensity <- function(frrf) {
  Intensity.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    Intensity.vector[i] <- frrf[[i]]$Label$Intensity[1]
  }
  return(Intensity.vector)
}

#' @title Extract Condition
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of light condition
#' @export
get.Condition <- function(frrf) {
  Condition.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    Condition.vector[i] <- frrf[[i]]$Label$Condition[1]
  }
  return(Condition.vector)
}

#' @title Extract Iron
#' @param frrf A list of frrf data with components for extracting parameters
#' @return A character list of Iron level
#' @export
get.Iron <- function(frrf) {
  Iron.vector <- rep("a", length(frrf))
  for (i in seq_along(frrf)) {
    Iron.vector[i] <- frrf[[i]]$Label$Iron[1]
  }
  return(Iron.vector)
}
