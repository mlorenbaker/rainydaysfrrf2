#' Universal LabSTAF Loading
#' @title Load FRRF Datafiles- UNIVERSAL
#' @author MB
#' @param directory Path to the input directory containing .csv files
#' @importFrom stats setNames
#' @importFrom readr read_lines
#' @export
load.LabSTAF <- function(directory) {
  result <- list()
  files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

  for (file_path in files) {
    cat("Loading file:", basename(file_path), "\n")

    file_name <- basename(file_path)
    datetime <- as.POSIXct(strptime(substr(file_name, 1, 11),
                                    format = "%y%m%d-%H%M", tz = "UTC"))

    lines <- readr::read_lines(file_path)

    #  find first occurrence of a header
    find_section <- function(patterns) {
      idxs <- unlist(lapply(patterns, function(p)
        stringr::str_which(lines, stringr::regex(p, ignore_case = TRUE))
      ))
      idxs <- sort(unique(idxs))
      if (length(idxs)) idxs[1] else NA_integer_
    }
    section_range <- function(start_idx, next_idxs) {
      if (is.na(start_idx)) return(integer(0))
      # find the smallest "next" that comes after start
      end <- if (any(!is.na(next_idxs))) min(next_idxs, na.rm = TRUE) - 1 else length(lines)
      seq.int(start_idx + 1, end)
    }


    #  Locate sections by header text
    notes_start  <- find_section(c("^\\s*File details\\b"))
    params_start <- find_section(c("^\\s*Data processing\\b"))
    rpe_start    <- find_section(c("^\\s*FLC setup\\b"))
    data_starts <- which(stringr::str_detect(lines, "(^|,|\\t)E(,|\\t)"))
    data_start  <- if (length(data_starts) >= 2) data_starts[2] else data_starts[1]
    notes_lines <- lines[section_range(notes_start, params_start)]
    params_lines <- lines[section_range(params_start, rpe_start)]

    #  find the row where the FIRST column starts with "LabSTAF SN"
    data_end_idx <- which(vapply(lines, function(ln) {
      # convert line to valid UTF-8 (replace bad bytes with "")
      ln_clean <- iconv(ln, from = "", to = "UTF-8", sub = "")

      # split on comma or tab, take first field
      first_field <- trimws(strsplit(ln_clean, "[,\t]", perl = TRUE)[[1]][1])

      if (is.na(first_field) || first_field == "") return(FALSE)
      grepl("^LabSTAF\\s*SN\\b", first_field, ignore.case = TRUE)
    }, logical(1)))


    data_end <- if (length(data_end_idx)) data_end_idx[1] else NA_integer_

    # sanity: don't let a found data_end be before the data_start
    if (!is.na(data_end) && !is.na(data_start) && data_end <= data_start) {
      # something odd — ignore and treat as no explicit end
      data_end <- NA_integer_
    }

    # build data_lines from the header line (data_start) up to just before data_end
    if (!is.na(data_start)) {
      end_line <- if (!is.na(data_end)) data_end - 1L else length(lines)
      data_lines <- lines[seq.int(data_start, end_line)]
    } else {
      data_lines <- character(0)
    }


    #  compute line ranges (one past start header to just before next header)
    section_range2 <- function(start_idx, next_start) {
      if (is.na(start_idx)) return(integer(0))
      end <- if (!is.na(next_start)) next_start - 1 else length(lines)
      if (end < start_idx) return(integer(0))
      seq.int(start_idx + 1, end)
    }

    # - Extract Notes -
    Notes <- data.frame(Note = character(), Value = character(), stringsAsFactors = FALSE)
    if (!is.na(notes_start)) {
      notes_lines <- lines[section_range2(notes_start, params_start)]
      for (ln in notes_lines) {
        parts <- strsplit(ln, ",", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          name <- trimws(gsub(":", "", parts[1]))
          Notes <- rbind(Notes, data.frame(Note = name, Value = as.character(parts[2])))
        }
      }
    }

    # - Extract Params -
    Params <- data.frame(Param = character(), Value = numeric(), stringsAsFactors = FALSE)
    if (!is.na(params_start)) {
      params_lines <- lines[section_range2(params_start, rpe_start)]
      for (ln in params_lines) {
        parts <- strsplit(ln, ",", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          pname <- trimws(gsub(":", "", parts[1]))
          val   <- suppressWarnings(as.numeric(parts[2]))
          Params <- rbind(Params, data.frame(Param = pname, Value = val))
        }
      }
    }

    # - Extract rPE -
    rPE <- data.frame(Param = character(), Value = numeric(), stringsAsFactors = FALSE)
    if (!is.na(rpe_start)) {
      rpe_lines <- lines[section_range2(rpe_start, data_start)]
      for (ln in rpe_lines) {
        ln_fixed <- iconv(ln, "UTF-8", "UTF-8", sub = "byte")
        parts <- strsplit(ln_fixed, ",", fixed = TRUE)[[1]]
        # keep your original column logic: name in col 6, value in col 7
        if (length(parts) >= 7) {
          pname <- trimws(gsub(":", "", parts[6]))
          pval  <- suppressWarnings(as.numeric(parts[7]))
          if (!is.na(pval) && nzchar(pname)) {
            rPE <- rbind(rPE, data.frame(Param = pname, Value = pval))
          }
        }
      }
    }

    # -- function to numeric handle data in the next section for whatever reason (idk)
    to_numeric_df <- function(df) {
      if (ncol(df) > 1) {
        df[-1] <- lapply(df[-1], function(col) suppressWarnings(as.numeric(col)))
      }
      df
    }

    # - Extract Data (Up / Dark / Down) -
    Up <- Down <- Dark <- data.frame()
    if (!is.na(data_start)) {
      # use data_end (or EOF if missing) to stop before next section
      end_line <- if (!is.na(data_end)) data_end - 1 else length(lines)
      data_lines <- lines[data_start:end_line]

      dat <- utils::read.csv(
        text = paste(data_lines, collapse = "\n"),
        header = TRUE,             # now the real header is present
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      # If the instrument repeated the header as first data row, drop it
      if (nrow(dat) && is.character(dat[[1]]) && identical(trimws(dat[[1]][1]), "E")) {
        dat <- dat[-1, , drop = FALSE]
      }

      # Clean column names (remove brackets, dots, etc.)
      clean_names <- function(nms) {
        nms <- gsub("[\\[\\]'()\"]", "", nms)   # remove brackets/quotes/parens
        nms <- gsub("/", "_", nms)              # slashes -> underscores
        nms <- gsub("\\s+", "_", nms)           # spaces -> underscore
        nms <- gsub("\\.+", "", nms)            # remove ALL dots
        nms <- gsub("\\'+", "", nms)            # remove ALL '
        make.unique(nms, sep = "_")             # ensure uniqueness
      }
      names(dat) <- clean_names(names(dat))

      # Convert all but first column to numeric
      to_numeric_df <- function(df) {
        if (ncol(df) > 1) {
          df[-1] <- lapply(df[-1], function(col) suppressWarnings(as.numeric(col)))
        }
        df
      }

      take_or_empty <- function(df, cols) {
        cols <- cols[cols <= ncol(df)]
        if (length(cols)) df[, cols, drop = FALSE] else data.frame()
      }

      # Keep E (col 1) in every section
      Up   <- take_or_empty(dat, c(1, 2:35))  |> to_numeric_df()
      Dark <- take_or_empty(dat, c(1, 36:44)) |> to_numeric_df()
      Down <- take_or_empty(dat, c(1, 45:ncol(dat))) |> to_numeric_df()
    }


    # - Transpose small tables -
    Params_t <- if (nrow(Params)) as.data.frame(t(setNames(Params$Value, Params$Param))) else NULL
    rPE_t    <- if (nrow(rPE))    as.data.frame(t(setNames(rPE$Value, rPE$Param)))       else NULL
    Notes_t  <- if (nrow(Notes))  as.data.frame(t(setNames(Notes$Value, Notes$Note)))    else NULL


    # - Store -
    result[[file_name]] <- list(
      Params   = Params_t,
      Notes    = Notes_t,
      rPE      = rPE_t,
      A        = Up,
      B        = Dark,
      C        = Down,
      Datetime = datetime,
      File     = file_name
    )
  }

  return(result)
}
