#' @title Match files
#' @param frrf FRRf file matching
#' @param Match.sheet Corresponding match.sheet for file matching
#' @return fully matched files- appended data from Match.sheet into corresponding FRRf timestamped files
#' @export
match.files <- function(frrf, Match.sheet) {
  for (i in seq_along(frrf)) {
    # Extract the filename from the current element
    filename <- frrf[[i]]$File[1]

    # Find the matching rows in Match.sheet based on the filename
    rows <- Match.sheet[Match.sheet$File.name == filename, -1]

    # Append the matching rows to the Label data frame in frrf
    frrf[[i]]$Label <- dplyr::bind_rows(frrf[[i]]$Label, rows)
  }

  # Return the updated frrf list
  return(frrf)
}

