# Build COEL Behavioural Atom v2.0 JSON from classified event data.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require("jsonlite")

event_value <- function(df, i, nm) {
    if (is.null(df) || !(nm %in% names(df))) return(NULL)

    col <- df[[nm]]
    v <- if (is.list(col)) col[[i]] else col[i]
    if (!is.list(v) && length(v) == 1L && (is.na(v) || is.nan(v))) return(NULL)
    if (!is.list(v) && length(v) > 1L) {
        v <- v[!(is.na(v) | is.nan(v))]
        if (length(v) == 0L) return(NULL)
    }
    v
}

drop_missing <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) {
        out <- lapply(x, drop_missing)
        keep <- !vapply(out, is.null, logical(1))
        return(out[keep])
    }
    if (length(x) == 1L && (is.na(x) || is.nan(x))) return(NULL)
    if (length(x) > 1L) {
        x2 <- x[!(is.na(x) | is.nan(x))]
        if (length(x2) == 0L) return(NULL)
        return(x2)
    }
    x
}

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

build_header <- function(
        atom_version = "2.0",
        atom_iri     = "https://w3id.org/coel/atom/2.0",
        atom_id      = NULL,
        extension_registry = NULL
) {
    h <- list(AtomVersion = atom_version, AtomIRI = atom_iri)

    if (!is.null(atom_id) && nzchar(atom_id)) h[["AtomID"]] <- atom_id

    if (!is.null(extension_registry) && length(extension_registry) > 0L) {
        ext <- unique(as.character(extension_registry))
        ext <- ext[!is.na(ext) & nzchar(ext)]
        if (length(ext) > 0L) h[["ExtensionRegistry"]] <- as.list(ext)
    }

    h
}

build_when <- function(events_df, mpi_data, i) {
    time_utc   <- event_value(events_df, i, "TimeUTC")
    duration   <- event_value(events_df, i, "Duration")
    utc_offset <- mpi_data[["file_data"]][["TimeOffset"]]
    tz_str     <- mpi_data[["file_data"]][["TimeZone"]]

    time_iso <- NULL
    if (!is.null(time_utc) && !is.na(time_utc)) {
        off <- if (is.null(utc_offset) || is.na(utc_offset)) 0L else as.integer(utc_offset)
        t_loc <- as.POSIXct(time_utc, origin = "1970-01-01", tz = "UTC") + off

        tz_suffix <- if (!is.null(tz_str) && !is.na(tz_str) && nzchar(tz_str)) tz_str else "+00:00"

        time_iso <- paste0(
            format(t_loc, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
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

    utc_offset <- fd[["TimeOffset"]]
    tz_str     <- fd[["TimeZone"]]
    off <- if (is.null(utc_offset) || is.na(utc_offset)) 0L else as.integer(utc_offset)

    if (is.null(assessment_time)) assessment_time <- Sys.time()
    t_assess_local <- as.POSIXct(assessment_time, tz = "UTC") + off
    tz_suffix <- if (!is.null(tz_str) && !is.na(tz_str) && nzchar(tz_str)) tz_str else "+00:00"

    assessment_iso <- paste0(
        format(t_assess_local, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
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
        xMean          = event_value(events_df, i, "xMean"),
        xSD            = event_value(events_df, i, "xSD"),
        yMean          = event_value(events_df, i, "yMean"),
        ySD            = event_value(events_df, i, "ySD"),
        zMean          = event_value(events_df, i, "zMean"),
        zSD            = event_value(events_df, i, "zSD"),
        LightMean      = event_value(events_df, i, "LightMean"),
        LightMax       = event_value(events_df, i, "LightMax"),
        TempMean       = event_value(events_df, i, "TempMean"),
        TempSD         = event_value(events_df, i, "TempSD"),
        AGSAMean       = event_value(events_df, i, "AGSAMean"),
        ENMOMean       = event_value(events_df, i, "ENMOMean"),
        UpDownMean     = event_value(events_df, i, "UpDownMean"),
        UpDownSD       = event_value(events_df, i, "UpDownSD"),
        DegreesMean    = event_value(events_df, i, "DegreesMean"),
        DegreesSD      = event_value(events_df, i, "DegreesSD"),
        StepCount      = event_value(events_df, i, "StepCount"),
        StepMean       = event_value(events_df, i, "StepMean"),
        StepSD         = event_value(events_df, i, "StepSD"),
        StepDiff       = event_value(events_df, i, "StepDiff"),
        PostureChanges = event_value(events_df, i, "PostureChanges"),
        MET            = event_value(events_df, i, "MET")
    )
}

# Non-wear takes precedence over rest when intervals overlap.
build_rest_activity_events_from_mpi <- function(
        mpi_data,
        wake_label    = "Wake",
        rest_label    = "Rest",
        nonwear_label = "NonWear"
) {
    fd <- mpi_data[["file_data"]]

    start_utc <- suppressWarnings(as.numeric(fd[["MeasurementStartTimeUTC"]]))
    end_utc   <- suppressWarnings(as.numeric(fd[["MeasurementEndTimeUTC"]]))

    if (is.na(start_utc) || is.na(end_utc) || end_utc <= start_utc) {
        stop("Invalid MeasurementStartTimeUTC / MeasurementEndTimeUTC in mpi_data$file_data.")
    }

    nm <- mpi_data[["non_movement"]]

    norm_intervals <- function(x, start_bound, end_bound) {
        if (is.null(x) || !is.data.frame(x) || nrow(x) == 0L) {
            return(data.frame(start = numeric(0), end = numeric(0)))
        }
        if (!("start_time" %in% names(x)) || !("duration" %in% names(x))) {
            stop("Expected interval table with columns start_time and duration.")
        }

        s <- suppressWarnings(as.numeric(x[["start_time"]]))
        d <- suppressWarnings(as.numeric(x[["duration"]]))

        ok <- !is.na(s) & !is.na(d) & d > 0
        s <- s[ok]
        e <- s + d[ok]

        s <- pmax(s, start_bound)
        e <- pmin(e, end_bound)

        ok2 <- e > s
        data.frame(start = s[ok2], end = e[ok2])
    }

    rest_iv <- norm_intervals(nm[["rest_intervals"]], start_utc, end_utc)
    nw_iv   <- norm_intervals(nm[["non_wear"]], start_utc, end_utc)

    bps <- sort(unique(c(
        start_utc, end_utc,
        rest_iv$start, rest_iv$end,
        nw_iv$start,   nw_iv$end
    )))

    if (length(bps) < 2L) {
        return(data.frame(TimeUTC = numeric(), Duration = numeric(), Classification = character()))
    }

    in_any <- function(t_mid, iv) {
        nrow(iv) > 0L && any(t_mid >= iv$start & t_mid < iv$end)
    }

    n_seg <- length(bps) - 1L
    TimeUTC <- bps[seq_len(n_seg)]
    Duration <- diff(bps)
    mid <- TimeUTC + Duration / 2

    Label <- rep(wake_label, n_seg)
    if (nrow(rest_iv) > 0L) Label[vapply(mid, in_any, logical(1), iv = rest_iv)] <- rest_label
    if (nrow(nw_iv)   > 0L) Label[vapply(mid, in_any, logical(1), iv = nw_iv)]   <- nonwear_label

    ok <- Duration > 0 & !is.na(TimeUTC)
    data.frame(
        TimeUTC        = TimeUTC[ok],
        Duration       = Duration[ok],
        Classification = Label[ok],
        stringsAsFactors = FALSE
    )
}

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

    accepted_models <- c("Behavioural Bout Model v1.0", "Rest Activity Model v1.0")
    if (!classification_model %in% accepted_models) {
        stop("Unknown classification_model: ", classification_model)
    }
    label_col <- "Classification"

    if (!(label_col %in% names(events_df))) {
        stop("events_df does not contain the required label column: ", label_col)
    }

    coel_lookup <- NULL
    if (!is.null(coel_map) && all(c("label","coel_code") %in% names(coel_map))) {
        tmp <- coel_map[, c("label","coel_code")]
        tmp$label <- as.character(tmp$label)
        tmp$coel_code <- as.character(tmp$coel_code)

        tmp <- tmp[!is.na(tmp$label) & nzchar(tmp$label), ]
        tmp <- tmp[!is.na(tmp$coel_code) & nzchar(tmp$coel_code), ]

        tmp <- tmp[!duplicated(tmp$label), ]

        coel_lookup <- setNames(tmp$coel_code, tmp$label)
    }

    assessment_time <- Sys.time()
    who <- build_who(mpi_data, environment_id = environment_id)

    if (is.null(evidence_system)) evidence_system <- build_evidence_system(mpi_data)

    n <- nrow(events_df)
    bouts <- vector("list", n)

    for (i in seq_len(n)) {

        evidence_id <- unname(mpi_data[["file_data"]][["UniqueBinFileIdentifier"]])

        label_i <- as.character(events_df[[label_col]][i])

        time_utc_i <- as.character(events_df[["TimeUTC"]][i])
        duration_i <- as.character(events_df[["Duration"]][i])

        atom_id_i <- paste0(evidence_id, "#", time_utc_i, "#", duration_i, "#", label_i)

        header_i <- build_header(
            atom_id = atom_id_i,
            extension_registry = if (isTRUE(include_extension)) extension_registry_iri else NULL
        )

        coel_code_i <- NA_character_
        if (!is.null(coel_lookup) && !is.na(label_i) && nzchar(label_i)) {
            coel_code_i <- unname(coel_lookup[label_i])
        }

        has_coel <- !is.na(coel_code_i) && nzchar(coel_code_i)

        how_i <- build_how(
            mpi_data                 = mpi_data,
            evidence_type            = evidence_type,
            evidence_system          = evidence_system,
            classification_model     = classification_model,
            classification_model_iri = classification_model_iri,
            assessment_time          = assessment_time,
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
            ext_i <- drop_missing(build_extension(events_df, i))

            if (!is.null(ext_i) && length(ext_i) > 0L) {
                atom$Extension <- ext_i
            }
        }

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

read_atoms_file <- function(path) {
    x <- read_json_any(path)
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
        stop("Duplicate AtomID(s): ", paste(head(dup_ids, 10), collapse = ", "))
    }
    invisible(TRUE)
}

combine_atoms_to_payload <- function(in_dir,
                                     out_json_path,
                                     pattern = "\\.json(\\.gz)?$",
                                     gzip = TRUE,
                                     report_time = TRUE) {
    t_start <- Sys.time()

    files <- list.files(in_dir, pattern = pattern, full.names = TRUE)
    if (length(files) == 0L) stop("No input JSON files found in: ", in_dir)

    atoms_list <- lapply(files, read_atoms_file)
    atoms_all <- do.call(c, atoms_list)

    check_duplicate_atom_ids(atoms_all)

    dir.create(dirname(out_json_path), showWarnings = FALSE, recursive = TRUE)
    write_json_any(atoms_all, out_json_path, pretty = TRUE)
    if (isTRUE(gzip) && !grepl("\\.gz$", out_json_path, ignore.case = TRUE)) {
        write_json_any(atoms_all, paste0(out_json_path, ".gz"), pretty = TRUE)
    }

    if (isTRUE(report_time))
        message(sprintf("Payload creation time: %.3f seconds", as.numeric(difftime(Sys.time(), t_start, units = "secs"))))

    invisible(out_json_path)
}
