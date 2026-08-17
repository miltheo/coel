# Manuscript payload and mapping summary tables.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)
coel_require("jsonlite")

payload_rows <- function(atoms, stream) {
    rows <- lapply(atoms, function(a) {
        labels <- as.character(unlist(get_in(a, c("What", "Label"), character(0)), use.names = FALSE))
        data.frame(stream = stream, participant = atom_pid(a), start = atom_start(a),
            duration = atom_duration(a), native_label = labels[1] %||% NA_character_,
            coel_code = if (length(labels) >= 2) labels[2] else NA_character_, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

mapping_summary <- function(rows) {
    by_label <- split(rows, rows$native_label)
    mapped <- vapply(by_label, function(x) any(!is.na(x$coel_code) & nzchar(x$coel_code)), logical(1))
    data.frame(`Atom Stream` = rows$stream[1], `Event labels (n)` = length(by_label),
        `Mapped labels (n)` = sum(mapped), `Unmapped labels (n)` = sum(!mapped),
        `Mapping Coverage (%)` = round(100 * mean(mapped), 1),
        `COEL codes (n)` = length(unique(na.omit(rows$coel_code[nzchar(rows$coel_code)]))),
        `Unmapped labels (examples)` = paste(names(by_label)[!mapped], collapse = "; "),
        check.names = FALSE, stringsAsFactors = FALSE)
}

payload_summary <- function(rows, payload_path) {
    data.frame(`Atom Stream` = rows$stream[1], `Atoms (n)` = nrow(rows),
        `Participants (n)` = length(unique(na.omit(rows$participant))),
        `Event labels (n)` = length(unique(na.omit(rows$native_label))),
        `COEL codes (n)` = length(unique(na.omit(rows$coel_code[nzchar(rows$coel_code)]))),
        `Median dur (s)` = stats::median(rows$duration, na.rm = TRUE),
        `Total dur (h)` = round(sum(rows$duration, na.rm = TRUE) / 3600, 1),
        `JSON (MB)` = round(gzip_uncompressed_size(payload_path) / 1024^2, 2),
        `Gzip (MB)` = round(file.info(payload_path)$size / 1024^2, 2),
        check.names = FALSE, stringsAsFactors = FALSE)
}

run_payload_summaries <- function(config) {
    all_rows <- lapply(config$streams, function(stream) payload_rows(read_json_any(stream$payload), stream$display))
    coverage <- do.call(rbind, lapply(all_rows, mapping_summary))
    summary <- do.call(rbind, Map(function(rows, stream) payload_summary(rows, stream$payload), all_rows, config$streams))
    summary <- merge(summary, coverage[, c("Atom Stream", "Mapping Coverage (%)")], by = "Atom Stream", sort = FALSE)
    summary <- summary[, c("Atom Stream", "Atoms (n)", "Participants (n)", "Event labels (n)", "COEL codes (n)",
                           "Mapping Coverage (%)", "Median dur (s)", "Total dur (h)", "JSON (MB)", "Gzip (MB)")]
    out <- file.path(config$results_root, "tables")
    write_csv(summary, file.path(out, "Table_atom_stream_and_payload_summary.csv"))
    write_csv(coverage, file.path(out, "Table_mapping_coverage_in_payload.csv"))
    list(payload = summary, mapping = coverage)
}
