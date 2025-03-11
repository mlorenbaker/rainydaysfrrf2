#' Notes- Blank
#' @title Extract Project
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return Blank
#' @export
get.Blank <- function(frrf) {
  blank.vector <- rep(NA, length(frrf))
  for (i in seq_along(frrf)) {
    blank.vector[i] <- frrf[[i]]$Notes$Blank[1]
  }
  return(blank.vector)
}

#' Notes- Project
#' @title Extract Project
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return Project
#' @export
get.Project <- function(frrf) {
  project.vector <- rep(NA, length(frrf))
  for (i in seq_along(frrf)) {
    project.vector[i] <- frrf[[i]]$Notes$Project[1]
  }
  return(project.vector)
}


#' Notes- Reference
#' @title Extract Reference
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return Reference
#' @export
get.Reference <- function(frrf) {
  reference.vector <- rep(NA, length(frrf))
  for (i in seq_along(frrf)) {
    reference.vector[i] <- frrf[[i]]$Notes$Reference[1]
  }
  return(reference.vector)
}

#' Notes- author
#' @title Extract author
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return Run by
#' @export
get.Author <- function(frrf) {
  author.vector <- rep(NA, length(frrf))
  for (i in seq_along(frrf)) {
    author.vector[i] <- frrf[[i]]$Notes$`Run by`[1]
  }
  return(author.vector)
}

#' Notes- Note
#' @title Extract Note
#' @param frrf A list of frrf data with components for extracting parameters- LabSTAF
#' @return Note
#' @export
get.Note <- function(frrf) {
  note.vector <- rep(NA, length(frrf))
  for (i in seq_along(frrf)) {
    note.vector[i] <- frrf[[i]]$Notes$Note[1]
  }
  return(note.vector)
}
