# CQ1-CQ5 defect injection, validation and evaluation.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)
coel_require("jsonlite")
coel_set_utf8_locale()

sample_size_10_30 <- function(n, seed = 1L, min_percent = 10L, max_percent = 30L) {
    set.seed(seed)
    sample(floor(n * min_percent / 100):floor(n * max_percent / 100), 1)
}

set_nested <- function(x, path, value) {
    if (length(path) == 1) { existed <- !is.null(x[[path]]); x[[path]] <- value; return(list(atom = x, existed = existed)) }
    if (is.null(x[[path[1]]])) return(list(atom = x, existed = FALSE))
    changed <- set_nested(x[[path[1]]], path[-1], value); x[[path[1]]] <- changed$atom
    list(atom = x, existed = changed$existed)
}

remove_nested <- function(x, path) {
    if (length(path) == 1) { existed <- !is.null(x[[path]]); x[[path]] <- NULL; return(list(atom = x, existed = existed)) }
    if (is.null(x[[path[1]]])) return(list(atom = x, existed = FALSE))
    changed <- remove_nested(x[[path[1]]], path[-1]); x[[path[1]]] <- changed$atom
    list(atom = x, existed = changed$existed)
}

new_log <- function() data.frame(AtomID = character(), action = character(), path = character(), existed = logical(), stringsAsFactors = FALSE)

inject_cq1 <- function(atoms, k, seed) {
    set.seed(seed); selected <- sample.int(length(atoms), k); actions <- sample(rep(c("remove", "type"), length.out = k))
    log <- data.frame(AtomID = character(k), action = actions, path = character(k), existed = logical(k), stringsAsFactors = FALSE)
    paths <- c("Header.AtomVersion", "When.TimeUTC", "When.Duration", "When.UTCOffset", "What.Label", "Who.ParticipantID")
    for (j in seq_along(selected)) {
        i <- selected[j]
        path <- if (actions[j] == "type") sample(setdiff(paths, "What.Label"), 1) else sample(paths, 1)
        parts <- strsplit(path, ".", fixed = TRUE)[[1]]; id <- atom_id(atoms[[i]])
        if (actions[j] == "remove") changed <- remove_nested(atoms[[i]], parts) else {
            old <- get_in(atoms[[i]], parts); replacement <- if (is.character(old)) 10L else "invalid_type"
            changed <- set_nested(atoms[[i]], parts, replacement)
        }
        atoms[[i]] <- changed$atom
        log$AtomID[j] <- id; log$path[j] <- path; log$existed[j] <- changed$existed
    }
    list(atoms = atoms, log = log)
}

inject_cq2 <- function(atoms, k, seed) {
    set.seed(seed); selected <- sample.int(length(atoms), k)
    log <- data.frame(AtomID = character(k), action = rep("corrupt_label", k), path = rep("What.Label", k), existed = logical(k))
    for (j in seq_along(selected)) {
        i <- selected[j]
        id <- atom_id(atoms[[i]]); labels <- as.list(unlist(get_in(atoms[[i]], c("What", "Label"), character(0)), use.names = FALSE))
        existed <- length(labels) > 0; if (existed) { labels[[1]] <- paste0("INVALID_LABEL_", labels[[1]]); atoms[[i]]$What$Label <- labels }
        log$AtomID[j] <- id; log$existed[j] <- existed
    }
    list(atoms = atoms, log = log)
}

generate_names <- function() {
    path <- coel_path("utilities", "code", "reference_data", "cq3_charlatan_names.csv")
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)$name
}

inject_cq3 <- function(atoms, k, seed) {
    string_paths <- function(x, prefix = "") {
        if (!is.list(x) || is.null(names(x))) return(character())
        output <- character()
        for (key in names(x)) {
            value <- x[[key]]; path <- if (nzchar(prefix)) paste(prefix, key, sep = ".") else key
            if (is.character(value) && length(value)) output <- c(output, path)
            if (is.list(value) && length(value) && all(vapply(value, function(z) is.character(z) && length(z), logical(1)))) output <- c(output, path)
            if (is.list(value) && !(length(value) && all(vapply(value, function(z) is.character(z) && length(z), logical(1))))) output <- c(output, string_paths(value, path))
        }
        output
    }
    names_pool <- generate_names(); set.seed(seed)
    invisible(sample(floor(length(atoms) * 0.10):floor(length(atoms) * 0.30), 1))
    selected <- sample.int(length(atoms), k); invisible(sample(rep("pii_inject", k)))
    log <- data.frame(AtomID = character(k), action = rep("inject_name", k), path = character(k),
        existed = logical(k), injected_name = character(k), stringsAsFactors = FALSE)
    for (j in seq_along(selected)) {
        i <- selected[j]
        candidates <- string_paths(atoms[[i]]); path <- if (length(candidates)) sample(candidates, 1) else NA_character_
        if (is.na(path)) {
            log$AtomID[j] <- atom_id(atoms[[i]]); log$path[j] <- ""; log$existed[j] <- FALSE
        } else {
            parts <- strsplit(path, ".", fixed = TRUE)[[1]]; old <- get_in(atoms[[i]], parts); name <- sample(names_pool, 1)
            replacement <- if (is.list(old)) lapply(old, function(z) if (is.character(z) && length(z)) name else z) else rep(name, length(old))
            changed <- set_nested(atoms[[i]], parts, replacement); atoms[[i]] <- changed$atom
            log$AtomID[j] <- atom_id(atoms[[i]]); log$path[j] <- path; log$existed[j] <- changed$existed; log$injected_name[j] <- name
        }
    }
    list(atoms = atoms, log = log)
}

inject_cq4 <- function(atoms, k, seed) {
    set.seed(seed); selected <- sample.int(length(atoms), k); duplicated <- atoms[selected]
    log <- data.frame(AtomID = vapply(duplicated, atom_id, character(1)), action = "duplicate", path = "", existed = TRUE)
    list(atoms = c(atoms, duplicated), log = log)
}

inject_cq5 <- function(atoms, seed, gaps_per_participant = 3L, removal_fraction = 0.20) {
    pid <- vapply(atoms, atom_pid, character(1)); start <- vapply(atoms, atom_start, numeric(1))
    remove <- integer(); boundaries <- character(); set.seed(seed)
    for (participant in sort(unique(pid))) {
        idx <- which(pid == participant); idx <- idx[order(start[idx])]; n <- length(idx)
        target <- max(gaps_per_participant, round(n * removal_fraction)); widths <- rep(target %/% gaps_per_participant, gaps_per_participant)
        widths[seq_len(target %% gaps_per_participant)] <- widths[seq_len(target %% gaps_per_participant)] + 1L
        anchors <- floor(seq(0.2, 0.8, length.out = gaps_per_participant) * n)
        for (j in seq_len(gaps_per_participant)) {
            width <- max(1L, widths[j]); first <- max(2L, min(n - width, anchors[j] - width %/% 2L)); last <- first + width - 1L
            remove <- c(remove, idx[first:last]); boundaries <- c(boundaries, atom_id(atoms[[idx[first - 1L]]]), atom_id(atoms[[idx[last + 1L]]]))
        }
    }
    remove <- sort(unique(remove)); truth <- unique(boundaries)
    list(atoms = atoms[-remove], log = data.frame(AtomID = truth, action = "gap_boundary", path = "When", existed = TRUE))
}

validate_structure <- function(atoms) {
    bad <- vapply(atoms, function(a) {
        required <- list(get_in(a, c("Header", "AtomVersion")), get_in(a, c("When", "TimeUTC")), get_in(a, c("When", "Duration")),
            get_in(a, c("When", "UTCOffset")), get_in(a, c("What", "Label")), get_in(a, c("Who", "ParticipantID")))
        if (any(vapply(required, is.null, logical(1)))) return(TRUE)
        !is.character(required[[1]]) || !is.numeric(required[[2]]) || !is.numeric(required[[3]]) ||
            !is.numeric(required[[4]]) || !(is.list(required[[5]]) || is.character(required[[5]])) || !is.character(required[[6]])
    }, logical(1))
    unique(vapply(atoms[bad], atom_id, character(1)))
}

validate_labels <- function(atoms, allowed) {
    labels <- vapply(atoms, atom_label, character(1)); unique(vapply(atoms[is.na(labels) | !(tolower(labels) %in% allowed)], atom_id, character(1)))
}

read_name_set <- function(path) {
    data <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    wanted <- c("Localized Name", "Romanized Name", "Romantized Name")
    columns <- names(data)[tolower(gsub("[^a-z]", "", names(data))) %in% tolower(gsub("[^a-z]", "", wanted))]
    values <- tolower(trimws(unlist(data[columns], use.names = FALSE)))
    unique(values[!is.na(values) & nzchar(values)])
}

all_strings <- function(x) {
    if (is.character(x)) return(x)
    if (!is.list(x)) return(character())
    unlist(lapply(x, all_strings), use.names = FALSE)
}

contains_person_name <- function(atom, forenames, surnames, window = 6L) {
    strings <- tolower(trimws(all_strings(atom))); all_names <- unique(c(forenames, surnames))
    if (any(strings %in% all_names)) return(TRUE)
    words <- unlist(regmatches(paste(strings, collapse = " "), gregexpr("\\p{L}+", paste(strings, collapse = " "), perl = TRUE)), use.names = FALSE)
    words <- tolower(words); if (length(words) < 2) return(FALSE)
    is_forename <- words %in% forenames; is_surname <- words %in% surnames
    if (any((is_forename[-length(words)] & is_surname[-1]) | (is_surname[-length(words)] & is_forename[-1]))) return(TRUE)
    positions <- which(is_forename | is_surname)
    length(positions) >= 2 && any(diff(positions) <= window)
}

validate_names <- function(atoms, config) {
    ref <- file.path(config$repo_root, "utilities", "code", "reference_data", "names")
    forenames <- read_name_set(file.path(ref, "common-forenames-by-country.csv"))
    surnames <- read_name_set(file.path(ref, "common-surnames-by-country.csv"))
    hit <- vapply(atoms, contains_person_name, logical(1), forenames = forenames, surnames = surnames)
    unique(vapply(atoms[hit], atom_id, character(1)))
}

validate_duplicates <- function(atoms) {
    signatures <- vapply(atoms, atom_signature, character(1)); hit <- duplicated(signatures) | duplicated(signatures, fromLast = TRUE)
    unique(vapply(atoms[hit], atom_id, character(1)))
}

validate_gaps <- function(atoms, threshold) {
    pid <- vapply(atoms, atom_pid, character(1)); start <- vapply(atoms, atom_start, numeric(1)); duration <- vapply(atoms, atom_duration, numeric(1))
    ids <- vapply(atoms, atom_id, character(1)); hits <- character()
    for (participant in unique(pid)) {
        idx <- which(pid == participant); idx <- idx[order(start[idx])]
        if (length(idx) < 2) next
        gap <- start[idx[-1]] - (start[idx[-length(idx)]] + duration[idx[-length(idx)]])
        where <- which(gap > threshold); if (length(where)) hits <- c(hits, ids[idx[where]], ids[idx[where + 1L]])
    }
    unique(hits)
}

score_predictions <- function(log, predicted, total_atoms) {
    truth <- unique(log$AtomID[log$existed & !is.na(log$AtomID) & nzchar(log$AtomID)])
    predicted <- unique(predicted[!is.na(predicted) & nzchar(predicted)])
    tp <- length(intersect(truth, predicted)); fp <- length(setdiff(predicted, truth)); fn <- length(setdiff(truth, predicted)); tn <- total_atoms - tp - fp - fn
    precision <- if (tp + fp) tp / (tp + fp) else NA_real_; recall <- if (tp + fn) tp / (tp + fn) else NA_real_
    f1 <- if (is.na(precision) || is.na(recall) || precision + recall == 0) NA_real_ else 2 * precision * recall / (precision + recall)
    data.frame(TP = tp, FP = fp, FN = fn, TN = tn, Accuracy = (tp + tn) / total_atoms,
        Precision = precision, Sensitivity = recall, Specificity = if (tn + fp) tn / (tn + fp) else NA_real_, F1 = f1)
}

run_validation_stream <- function(stream, config) {
    baseline <- read_json_any(stream$payload); k <- sample_size_10_30(length(baseline), config$seed,
        config$sample_percent_min, config$sample_percent_max)
    baseline_labels <- unique(tolower(vapply(baseline, atom_label, character(1))))
    out <- file.path(config$results_root, "validation", stream$key); metrics <- list()
    for (cq in paste0("CQ", 1:5)) {
        injection <- switch(cq,
            CQ1 = inject_cq1(baseline, k, config$seed),
            CQ2 = inject_cq2(baseline, k, config$seed),
            CQ3 = inject_cq3(baseline, k, config$seed),
            CQ4 = inject_cq4(baseline, k, config$seed),
            CQ5 = inject_cq5(baseline, config$seed))
        predicted <- switch(cq,
            CQ1 = validate_structure(injection$atoms),
            CQ2 = validate_labels(injection$atoms, baseline_labels),
            CQ3 = validate_names(injection$atoms, config),
            CQ4 = validate_duplicates(injection$atoms),
            CQ5 = validate_gaps(injection$atoms, config$gap_threshold_seconds))
        metric <- score_predictions(injection$log, predicted, length(injection$atoms)); metric$cq <- cq; metric$stream <- stream$key
        metric$seed <- config$seed; metric$k_drawn <- if (cq == "CQ5") nrow(injection$log) else k
        metrics[[cq]] <- metric
        write_csv(injection$log, file.path(out, cq, paste0("changelog_", cq, ".csv")))
        write_csv(data.frame(AtomID = predicted), file.path(out, cq, paste0("violations_", cq, ".csv")))
        write_csv(metric, file.path(out, cq, paste0("metrics_any_", cq, ".csv")))
        if (config$write_seeded_payloads) write_json_any(injection$atoms, file.path(out, cq, paste0("payload_seeded_", cq, ".json.gz")))
        rm(injection); gc(verbose = FALSE)
    }
    metrics <- do.call(rbind, metrics); write_csv(metrics, file.path(out, "metrics_any_all_cq.csv")); metrics
}

run_validation_cqs <- function(config) do.call(rbind, lapply(config$streams, run_validation_stream, config = config))
