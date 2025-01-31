#' Load LabSTAF
#' @title Load FRRF Datafiles
#' @author HoWan Chan
#' @param input.dir the input directory
#' @param file.names the name of the file in the input directory (default will load all files in that directory)
#' @param verbose honestly I have no idea this was all Tom
#' @export
load.LabSTAF <- function(directory) {
  result <- list()
  files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

  for (file_path in files) {
    cat("Loading files:", basename(file_path),"\n")

    # Extract datetime from file name
    file_name <- basename(file_path)
    datetime <- as.POSIXct(strptime(substr(file_name, 1, 11), format = "%y%m%d-%H%M"), tz = "UTC")

    # Full file name including directory
    full_file_name <- file_path

    # brute force reading lines
    lines <- read_lines(file_path)

    # initialize each section
    Params <- data.frame(Param = character(), Value = numeric(), stringsAsFactors = FALSE)
    rPE <- data.frame(Param = character(), Value = numeric(), stringsAsFactors = FALSE)
    Up <- data.frame()
    Dark <- data.frame()
    Down <- data.frame()
    Notes <- data.frame(Note = character(), Value = character(), stringsAsFactors = FALSE)

    # extract Notes
    notes_lines <- lines[14:18]
    for (line in notes_lines) {
      info <- strsplit(line, ",")[[1]]
      info_name <- trimws(gsub(":", "", info[1]))
      Notes <- rbind(Notes, data.frame(Note = info_name, Value = as.character(info[2])))
    }

    # extract Params
    params_lines <- lines[20:26]
    for (line in params_lines) {
      parts <- strsplit(line, ",")[[1]]
      param_name <- trimws(gsub(":", "", parts[1]))
      Params <- rbind(Params, data.frame(Param = param_name, Value = as.numeric(parts[2])))
    }

    # extract rPE
    rPE_lines <- lines[29:35]
    for (line in rPE_lines) {
      line_fixed <- iconv(line, "UTF-8", "UTF-8", sub = "byte") # the // and \\ are messing up the line read
      parts <- strsplit(line_fixed, ",")[[1]]
      param_name <- trimws(gsub(":", "", parts[6]))
      rPE <- rbind(rPE, data.frame(Param = param_name, Value = as.numeric(parts[7])))
    }

    Params_t <- as.data.frame(t(data.frame(Params$Value, row.names = Params$Param)))
    colnames(Params_t) <- Params$Param
    rPE_t <- as.data.frame(t(data.frame(rPE$Value, row.names = rPE$Param)))
    colnames(rPE_t) <- rPE$Param
    Notes_t <- as.data.frame(t(data.frame(Notes$Value, row.names = Notes$Note)))
    colnames(Notes_t) <- Notes$Note


    # extract Up, Down, Dark
    a_lines <- lines[54:66]
    col_names <- unlist(strsplit(a_lines[1], ","))
    max_cols <- length(col_names)

    data_list <- lapply(a_lines[-1], function(x) {
      parts <- unlist(strsplit(x, ","))
      length(parts) <- max_cols
      parts
    })

    data <- do.call(rbind, data_list)
    colnames(data) <- col_names

    numeric_data <- apply(data, 2, function(column) as.numeric(replace(column, column == "", NA)))

    # remove [, ], ', (, ) for post process
    colnames(numeric_data) <- gsub("\\[", "", colnames(numeric_data))  # Remove [
    colnames(numeric_data) <- gsub("\\]", "", colnames(numeric_data))  # Remove ]
    colnames(numeric_data) <- gsub("'", "", colnames(numeric_data))    # Remove '
    colnames(numeric_data) <- gsub("\\(", "", colnames(numeric_data))  # Remove (
    colnames(numeric_data) <- gsub("\\)", "", colnames(numeric_data))  # Remove )


    # split data into up, down, dark
    Up <- as.data.frame(numeric_data[, 2:36])
    Dark <- as.data.frame(numeric_data[, 37:45])
    Down <- as.data.frame(numeric_data[, 46:ncol(numeric_data)])

    # finalize
    result[[file_name]] <- list(Params = Params_t, Notes = Notes_t, rPE = rPE_t, A = Up, B = Dark, C = Down, Datetime = datetime, File_name = full_file_name)

  }

  return(result)
}
