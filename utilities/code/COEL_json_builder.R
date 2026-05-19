# COEL Atom JSON builder
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE
#
# Builds COEL Behavioural Atom v2.0 JSON from classified event data.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require("jsonlite")

# ------------------------------------------------------------------------------
# Safe getter that returns NULL (not NA) for missing values
# - This prevents jsonlite from emitting JSON nulls for absent values.
# - Handles list-columns correctly (uses [[i]] rather than [i]).
# ------------------------------------------------------------------------------
g <- function(df, i, nm) {
    # If the data frame or the column does not exist, treat as missing
    if (is.null(df) || !(nm %in% names(df))) return(NULL)
    
    col <- df[[nm]]
    
    # Extract the i-th value:
    # - list-columns need [[i]] to get the element itself
    # - atomic vectors use [i] to preserve type
    v <- if (is.list(col)) col[[i]] else col[i]
    
    # If we got a single missing scalar (NA/NaN), treat as missing
    if (!is.list(v) && length(v) == 1L && (is.na(v) || is.nan(v))) return(NULL)
    
    # If we got a vector, drop missing elements; if nothing remains, treat as missing
    if (!is.list(v) && length(v) > 1L) {
        v <- v[!(is.na(v) | is.nan(v))]
        if (length(v) == 0L) return(NULL)
    }
    
    # Otherwise return as-is
    v
}

# ------------------------------------------------------------------------------
# Recursive cleanup:
# - Removes NULL entries from lists (so keys disappear from JSON)
# - Removes NA/NaN values from atomic vectors
# - Keeps structure otherwise unchanged
# ------------------------------------------------------------------------------
drop_missing <- function(x) {
    # Missing -> NULL
    if (is.null(x)) return(NULL)
    
    # Recurse into lists (including named lists used for JSON objects)
    if (is.list(x)) {
        out <- lapply(x, drop_missing)
        
        # Drop NULL elements after recursion
        keep <- !vapply(out, is.null, logical(1))
        out <- out[keep]
        
        # Preserve names if present (R will keep names through subsetting)
        return(out)
    }
    
    # Atomic scalar: drop NA/NaN
    if (length(x) == 1L && (is.na(x) || is.nan(x))) return(NULL)
    
    # Atomic vector: drop NA/NaN elements; drop vector if empty after filtering
    if (length(x) > 1L) {
        x2 <- x[!(is.na(x) | is.nan(x))]
        if (length(x2) == 0L) return(NULL)
        return(x2)
    }
    
    # Otherwise keep
    x
}

# ------------------------------------------------------------------------------
# EvidenceSystem builder (one-time per stream)
# ------------------------------------------------------------------------------
build_evidence_system <- function(mpi_data) {
    fd <- mpi_data[["file_data"]]
    
    geneacore_ver <- tryCatch(
        as.character(utils::packageVersion("GENEAcore")),
        error = function(e) NA_character_
    )
    
    device    <- fd[["MeasurementDevice"]]
    device_id <- fd[["MeasurementDeviceID"]]
    fw_ver    <- fd[["MeasurementDeviceFirmwareVersion"]]
    wear_loc  <- fd[["WearLocationConfig"]]
    freq_hz   <- fd[["MeasurementFrequency"]]
    
    sprintf(
        "GENEAcore Version: %s; Device: %s; DeviceID: %s; FirmwareVersion: %s; Protocol: %s, %s Hz",
        geneacore_ver,
        device,
        device_id,
        fw_ver,
        wear_loc,
        freq_hz
    )
}

# ------------------------------------------------------------------------------
# Element builders
# ------------------------------------------------------------------------------

build_header <- function(
        atom_version = "2.0",
        atom_iri     = "https://w3id.org/coel/atom/2.0",
        atom_id      = NULL,
        extension_registry = NULL
) {
    h <- list(AtomVersion = atom_version, AtomIRI = atom_iri)
    
    if (!is.null(atom_id) && nzchar(atom_id)) h[["AtomID"]] <- atom_id
    
    # Only add ExtensionRegistry if provided (character vector)
    if (!is.null(extension_registry) && length(extension_registry) > 0L) {
        ext <- unique(as.character(extension_registry))
        ext <- ext[!is.na(ext) & nzchar(ext)]
        if (length(ext) > 0L) h[["ExtensionRegistry"]] <- as.list(ext)
    }
    
    h
}

build_when <- function(events_df, mpi_data, i) {
    time_utc   <- g(events_df, i, "TimeUTC")
    duration   <- g(events_df, i, "Duration")
    utc_offset <- mpi_data[["file_data"]][["TimeOffset"]]  # seconds
    tz_str     <- mpi_data[["file_data"]][["TimeZone"]]    # e.g. "+1:00"
    
    time_iso <- NULL
    if (!is.null(time_utc) && !is.na(time_utc)) {
        off <- if (is.null(utc_offset) || is.na(utc_offset)) 0L else as.integer(utc_offset)
        t_loc <- as.POSIXct(time_utc, origin = "1970-01-01", tz = "UTC") + off
        
        tz_suffix <- if (!is.null(tz_str) && !is.na(tz_str) && nzchar(tz_str)) tz_str else "+00:00"
        
        time_iso <- paste0(
            format(t_loc, "%Y-%m-%dT%H:%M:%S"),
            tz_suffix
        )
    }
    
    list(
        TimeUTC   = time_utc,
        UTCOffset = utc_offset,
        TimeISO   = time_iso,
        Duration  = duration,
        Accuracy  = NULL
    )
}

build_what <- function(
        events_df,
        i,
        ai_model_base_iri,
        coel_code = NULL,
        coel_model_base_iri = "https://w3id.org/coel/models/coel/2.0/",
        label_col           = "behavioural_bouts"
) {
    label <- as.character(events_df[[label_col]][i])
    
    if (length(label) == 0L || is.na(label) || label == "") {
        return(list(Label = list(), LabelIRI = list()))
    }
    
    ai_base   <- sub("/$", "", ai_model_base_iri)
    coel_base <- sub("/$", "", coel_model_base_iri)
    
    labels <- list(label)
    iris   <- list(paste0(
        ai_base, "#", utils::URLencode(label, reserved = TRUE)
    ))
    
    if (!is.null(coel_code) && !is.na(coel_code) && nzchar(coel_code)) {
        labels[[2L]] <- as.character(coel_code)
        iris[[2L]] <- paste0(
            coel_base, "#", utils::URLencode(as.character(coel_code), reserved = TRUE)
        )
    }
    
    list(Label = labels, LabelIRI = iris)
}

build_who <- function(mpi_data, environment_id = NULL) {
    fd <- mpi_data[["file_data"]]
    
    list(
        ParticipantID = unname(fd[["ParticipantID"]]),
        EnvironmentID = unname(environment_id)
    )
}

build_how <- function(
        mpi_data,
        evidence_type            = NULL,
        evidence_system          = NULL,
        classification_model     = NULL,
        classification_model_iri = NULL,
        assessment_time          = NULL,
        include_coel_model       = FALSE,
        coel_model               = "COEL Model v2.0",
        coel_model_iri           = "https://w3id.org/coel/models/coel/2.0/"
) {
    fd <- mpi_data[["file_data"]]
    
    if (is.null(evidence_type)) evidence_type <- 7L
    if (is.null(evidence_system)) evidence_system <- build_evidence_system(mpi_data)
    
    if (is.null(classification_model) || is.null(classification_model_iri)) {
        cm  <- list()
        cmI <- list()
    } else {
        ai_name <- as.character(classification_model)
        ai_iri  <- as.character(classification_model_iri)
        
        if (isTRUE(include_coel_model)) {
            cm  <- list(ai_name, coel_model)
            cmI <- list(ai_iri,  coel_model_iri)
        } else {
            cm  <- list(ai_name)
            cmI <- list(ai_iri)
        }
    }
    
    utc_offset <- fd[["TimeOffset"]]   # seconds
    tz_str     <- fd[["TimeZone"]]     # e.g. "+1:00"
    off <- if (is.null(utc_offset) || is.na(utc_offset)) 0L else as.integer(utc_offset)
    
    t_assess_local <- as.POSIXct(assessment_time, tz = "UTC") + off
    tz_suffix <- if (!is.null(tz_str) && !is.na(tz_str) && nzchar(tz_str)) tz_str else "+00:00"
    
    assessment_iso <- paste0(
        format(t_assess_local, "%Y-%m-%dT%H:%M:%S"),
        tz_suffix
    )
    
    list(
        EvidenceType           = as.integer(evidence_type),
        EvidenceSystem         = evidence_system,
        ClassificationModel    = cm,
        ClassificationModelIRI = cmI,
        AssessmentTimeISO      = assessment_iso,
        EvidenceID             = unname(fd[["UniqueBinFileIdentifier"]])
    )
}

build_extension <- function(events_df, i) {
    list(
        xMean          = g(events_df, i, "xMean"),
        xSD            = g(events_df, i, "xSD"),
        yMean          = g(events_df, i, "yMean"),
        ySD            = g(events_df, i, "ySD"),
        zMean          = g(events_df, i, "zMean"),
        zSD            = g(events_df, i, "zSD"),
        LightMean      = g(events_df, i, "LightMean"),
        LightMax       = g(events_df, i, "LightMax"),
        TempMean       = g(events_df, i, "TempMean"),
        TempSD         = g(events_df, i, "TempSD"),
        AGSAMean       = g(events_df, i, "AGSAMean"),
        ENMOMean       = g(events_df, i, "ENMOMean"),
        UpDownMean     = g(events_df, i, "UpDownMean"),
        UpDownSD       = g(events_df, i, "UpDownSD"),
        DegreesMean    = g(events_df, i, "DegreesMean"),
        DegreesSD      = g(events_df, i, "DegreesSD"),
        StepCount      = g(events_df, i, "StepCount"),
        StepMean       = g(events_df, i, "StepMean"),
        StepSD         = g(events_df, i, "StepSD"),
        StepDiff       = g(events_df, i, "StepDiff"),
        PostureChanges = g(events_df, i, "PostureChanges"),
        MET            = g(events_df, i, "MET")
    )
}

# ------------------------------------------------------------------------------
# MPI-only Rest Activity events builder
# Assumes:
# - default label is "wake"
# - rest_intervals override wake
# - non_wear overrides both rest and wake
# Produces a contiguous events_df with columns: TimeUTC, Duration, rest_activity
# ------------------------------------------------------------------------------

build_rest_activity_events_from_mpi <- function(
        mpi_data,
        wake_label    = "Wake",
        rest_label    = "Rest",
        nonwear_label = "NonWear"
) {
    fd <- mpi_data[["file_data"]]
    
    start_utc <- suppressWarnings(as.numeric(fd[["MeasurementStartTimeUTC"]]))
    end_utc   <- suppressWarnings(as.numeric(fd[["MeasurementEndTimeUTC"]]))
    
    if (is.na(start_utc) || is.na(end_utc) || end_utc <= start_utc) { # Validate start/end are numeric and ordered
        stop("Invalid MeasurementStartTimeUTC / MeasurementEndTimeUTC in mpi_data$file_data.")
    }
    
    nm <- mpi_data[["non_movement"]] # pull non-movement block
    
    # Helper: normalise an interval table to [start_bound, end_bound)
    norm_intervals <- function(x, start_bound, end_bound) { 
        if (is.null(x) || !is.data.frame(x) || nrow(x) == 0L) {
            return(data.frame(start = numeric(0), end = numeric(0)))
        }
        if (!("start_time" %in% names(x)) || !("duration" %in% names(x))) {
            stop("Expected interval table with columns start_time and duration.")
        }
        
        s <- suppressWarnings(as.numeric(x[["start_time"]])) # Extract start_time and coerce to numeric
        d <- suppressWarnings(as.numeric(x[["duration"]])) # Extract duration and coerce to numeric
        
        ok <- !is.na(s) & !is.na(d) & d > 0 # Keep only rows with valid start, valid duration, and positive duration
        s <- s[ok]
        e <- s + d[ok] # Compute end times as start + duration (end is exclusive later)
        
        s <- pmax(s, start_bound) # Clamp starts so they are not before measurement start
        e <- pmin(e, end_bound) # Clamp ends so they are not after measurement end
        
        ok2 <- e > s  # Keep only intervals with positive length after clamping
        data.frame(start = s[ok2], end = e[ok2]) # Return normalised interval table with start/end columns
    }
    
    # Normalise rest + non_wear intervals to common bounds
    rest_iv <- norm_intervals(nm[["rest_intervals"]], start_utc, end_utc)
    nw_iv   <- norm_intervals(nm[["non_wear"]], start_utc, end_utc)
    
    # Breakpoints for contiguous segmentation
    bps <- sort(unique(c( 
        start_utc, end_utc,     
        rest_iv$start, rest_iv$end, # Include every rest interval start/end
        nw_iv$start,   nw_iv$end # Include every non-wear interval start/end
    )))
    
    if (length(bps) < 2L) { # If there are not enough breakpoints to form any segment
        return(data.frame(TimeUTC = numeric(0), Duration = numeric(0), rest_activity = character(0))) # Return empty events table
    }
    
    # ---- Helper: is a midpoint inside any interval? ----
    in_any <- function(t_mid, iv) {
        nrow(iv) > 0L && any(t_mid >= iv$start & t_mid < iv$end)
    }
    
    # Create segments from breakpoints and label them (wake default; non-wear overrides rest)
    n_seg <- length(bps) - 1L
    TimeUTC <- bps[seq_len(n_seg)]
    Duration <- diff(bps)
    mid <- TimeUTC + Duration / 2
    
    Label <- rep(wake_label, n_seg)
    if (nrow(rest_iv) > 0L) Label[vapply(mid, in_any, logical(1), iv = rest_iv)] <- rest_label
    if (nrow(nw_iv)   > 0L) Label[vapply(mid, in_any, logical(1), iv = nw_iv)]   <- nonwear_label
    
    # Return event-style table for Atom builder
    ok <- Duration > 0 & !is.na(TimeUTC)
    data.frame(
        TimeUTC        = TimeUTC[ok],
        Duration       = Duration[ok],
        Classification = Label[ok],
        stringsAsFactors = FALSE
    )
}                                                            

# ------------------------------------------------------------------------------
# Main builder (v1.0)
# - Works for bouts_df (behavioural_bouts/rest_activity column present)
# - Works for MPI-derived rest events_df created above
# ------------------------------------------------------------------------------

build_coel_atoms_json <- function(
        events_df,
        mpi_data,
        coel_map                 = NULL,
        coel_map_path            = NULL,
        environment_id           = NULL,
        evidence_type            = NULL,
        evidence_system          = NULL,
        classification_model     = NULL,
        classification_model_iri = NULL,
        include_extension        = TRUE,
        extension_registry_iri   = NULL,
        report_time              = TRUE
) {
    t_start <- Sys.time()
    
    if (is.null(coel_map) && !is.null(coel_map_path)) {
        coel_map <- read.csv(coel_map_path, stringsAsFactors = FALSE, check.names = FALSE)
    }
    
    if (is.null(classification_model) || is.null(classification_model_iri)) {
        stop("classification_model and classification_model_iri must be supplied.")
    }
    
    # Decide label column from model name
    if (classification_model == "Behavioural Bout Model v1.0") {
        label_col <- "Classification"
    } else if (classification_model == "Rest Activity Model v1.0") {
        label_col <- "Classification"
    } else {
        stop("Unknown classification_model: ", classification_model)
    }
    
    if (!(label_col %in% names(events_df))) {
        stop("events_df does not contain the required label column: ", label_col)
    }
    
    # Precompute mapping lookup (label -> coel_code)
    coel_lookup <- NULL
    if (!is.null(coel_map) && all(c("label","coel_code") %in% names(coel_map))) {
        tmp <- coel_map[, c("label","coel_code")]
        tmp$label <- as.character(tmp$label)
        tmp$coel_code <- as.character(tmp$coel_code)
        
        tmp <- tmp[!is.na(tmp$label) & nzchar(tmp$label), ]
        tmp <- tmp[!is.na(tmp$coel_code) & nzchar(tmp$coel_code), ]
        
        # keep first mapping per label
        tmp <- tmp[!duplicated(tmp$label), ]
        
        coel_lookup <- setNames(tmp$coel_code, tmp$label)
    }
    
    # One-time constants
    assessment_time <- Sys.time()
    fd <- mpi_data[["file_data"]]
    utc_offset <- fd[["TimeOffset"]]
    tz_str     <- fd[["TimeZone"]]
    off <- if (is.null(utc_offset) || is.na(utc_offset)) 0L else as.integer(utc_offset)
    t_assess_local <- as.POSIXct(assessment_time, tz = "UTC") + off
    tz_suffix <- if (!is.null(tz_str) && !is.na(tz_str) && nzchar(tz_str)) tz_str else "+00:00"
    assessment_time_iso <- paste0(format(t_assess_local, "%Y-%m-%dT%H:%M:%S"), tz_suffix)
    
    who <- build_who(mpi_data, environment_id = environment_id)
    
    if (is.null(evidence_system)) evidence_system <- build_evidence_system(mpi_data)
    
    n <- nrow(events_df)
    bouts <- vector("list", n)
    
    for (i in seq_len(n)) {
        
        evidence_id <- unname(mpi_data[["file_data"]][["UniqueBinFileIdentifier"]])
        
        label_i <- as.character(events_df[[label_col]][i])
        
        time_utc_i <- as.character(events_df[["TimeUTC"]][i])
        duration_i <- as.character(events_df[["Duration"]][i])
        
        # Reuse the same assessment time for the batch (already created once)
        # Make AtomID: <index>#<EvidenceID>#<AssessmentTimeISO>#<Label>
        atom_id_i <- paste0(evidence_id, "#", time_utc_i, "#", duration_i, "#", label_i)
        
        header_i <- build_header(
            atom_id = atom_id_i,
            extension_registry = if (isTRUE(include_extension)) extension_registry_iri else NULL
        )
        
        coel_code_i <- NA_character_
        if (!is.null(coel_lookup) && !is.na(label_i) && nzchar(label_i)) {
            coel_code_i <- unname(coel_lookup[label_i])  # safe; returns NA if not present
        }
        
        has_coel <- !is.na(coel_code_i) && nzchar(coel_code_i)
        
        how_i <- build_how(
            mpi_data                 = mpi_data,
            evidence_type            = evidence_type,
            evidence_system          = evidence_system,
            classification_model     = classification_model,
            classification_model_iri = classification_model_iri,
            assessment_time          = assessment_time_iso,
            include_coel_model       = has_coel
        )
        
        atom <- list(
            Header = header_i,
            When   = build_when(events_df, mpi_data, i),
            What   = build_what(
                events_df         = events_df,
                i                 = i,
                ai_model_base_iri = classification_model_iri,
                coel_code         = coel_code_i,
                label_col         = label_col
            ),
            Who = who,
            How = how_i
        )
        
        if (isTRUE(include_extension)) {
            # Build Extension object, then drop missing values
            ext_i <- drop_missing(build_extension(events_df, i))
            
            # Only include Extension if at least one extension field remains
            if (!is.null(ext_i) && length(ext_i) > 0L) {
                atom$Extension <- ext_i
            }
        }
        
        # Drop any remaining missing values anywhere in the Atom
        atom <- drop_missing(atom)
        
        bouts[[i]] <- atom
    }
    
    json_out <- jsonlite::toJSON(
        bouts,
        auto_unbox = TRUE,
        POSIXt     = "ISO8601",
        pretty     = TRUE
    )
    
    if (isTRUE(report_time)) {
        elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
        message(sprintf("Atom creation time: %.3f seconds", elapsed))
    }
    
    json_out
}

# ------------------------------------------------------------------------------
# Payload builder (combine per-participant Atom JSON files)
# ------------------------------------------------------------------------------
read_atoms_file <- function(path) {
    x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (!is.list(x)) stop("File is not a JSON array of Atoms: ", path)
    x
}

check_duplicate_atom_ids <- function(atoms_all) {
    atom_ids <- vapply(
        atoms_all,
        function(a) {
            if (!is.list(a) || is.null(a$Header) || is.null(a$Header$AtomID)) return(NA_character_)
            as.character(a$Header$AtomID)
        },
        character(1)
    )
    dup_ids <- unique(atom_ids[duplicated(atom_ids) & !is.na(atom_ids)])
    if (length(dup_ids) > 0L) {
        message("Duplicate AtomID(s) detected (showing up to 10):")
        print(head(dup_ids, 10))
        stop("Duplicate AtomID detected across inputs.")
    }
    invisible(TRUE)
}

combine_atoms_to_payload <- function(in_dir,
                                     out_json_path,
                                     pattern = "\\.json$",
                                     gzip = TRUE,
                                     report_time = TRUE) {
    t_start <- Sys.time()
    
    files <- list.files(in_dir, pattern = pattern, full.names = TRUE)
    if (length(files) == 0L) stop("No input JSON files found in: ", in_dir)
    
    atoms_list <- lapply(files, read_atoms_file)
    atoms_all <- do.call(c, atoms_list)
    
    check_duplicate_atom_ids(atoms_all)
    
    dir.create(dirname(out_json_path), showWarnings = FALSE, recursive = TRUE)
    payload_txt <- jsonlite::toJSON(atoms_all, auto_unbox = TRUE, pretty = TRUE, na = "null")
    writeLines(payload_txt, out_json_path)
    
    if (isTRUE(gzip)) {
        gz_path <- paste0(out_json_path, ".gz")
        con <- gzfile(gz_path, "wb")
        writeLines(payload_txt, con)
        close(con)
    }
    
    if (isTRUE(report_time))
        message(sprintf("Payload creation time: %.3f seconds", as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
    
    invisible(out_json_path)
}


# Example workflow:
# 1. Load classified event data and metadata into `events_df` and `mpi_data`.
# 2. Load a mapping CSV from `mapping/`.
# 3. Call `build_coel_atoms_json()` for each participant and stream.
# 4. Combine per-participant files with `combine_atoms_to_payload()`.
