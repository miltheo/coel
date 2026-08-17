# Shared helpers for the COEL manuscript reproducibility pipeline.

coel_repo_root <- function(start = getwd()) {
    current <- normalizePath(start, winslash = "/", mustWork = TRUE)
    repeat {
        if (file.exists(file.path(current, "README.md")) &&
            dir.exists(file.path(current, "models")) && dir.exists(file.path(current, "atom"))) return(current)
        parent <- dirname(current)
        if (identical(parent, current)) stop("Could not locate the COEL repository root from ", start, call. = FALSE)
        current <- parent
    }
}

coel_path <- function(...) file.path(coel_repo_root(), ...)

coel_use_local_library <- function() {
    local_library <- coel_path("utilities", ".r-library")
    if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
    invisible(.libPaths())
}

coel_require <- function(packages) {
    coel_use_local_library()
    missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) stop("Missing R package(s): ", paste(missing, collapse = ", "),
                              ". Restore them with renv::restore(project = 'utilities').", call. = FALSE)
    invisible(TRUE)
}

coel_set_utf8_locale <- function() {
    candidates <- c("C.UTF-8", "English_United Kingdom.utf8", "en_GB.UTF-8", "en_US.UTF-8")
    for (candidate in candidates) {
        value <- suppressWarnings(try(Sys.setlocale("LC_CTYPE", candidate), silent = TRUE))
        if (!inherits(value, "try-error") && !is.na(value) && nzchar(value)) return(invisible(value))
    }
    stop("A UTF-8 LC_CTYPE locale is required for multilingual CQ3 tokenisation.", call. = FALSE)
}

ensure_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    invisible(path)
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

get_in <- function(x, path, default = NULL) {
    for (key in path) {
        if (!is.list(x) || is.null(x[[key]])) return(default)
        x <- x[[key]]
    }
    x
}

read_json_any <- function(path) {
    if (!file.exists(path)) stop("Missing JSON input: ", path, call. = FALSE)
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rb") else file(path, "rb")
    on.exit(close(con), add = TRUE)
    jsonlite::fromJSON(con, simplifyVector = FALSE)
}

write_json_any <- function(x, path, pretty = FALSE) {
    ensure_dir(dirname(path))
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "wb") else file(path, "wb")
    on.exit(close(con), add = TRUE)
    jsonlite::write_json(x, con, auto_unbox = TRUE, pretty = pretty, na = "null")
    invisible(path)
}

gzip_uncompressed_size <- function(path, chunk_size = 1024L * 1024L) {
    con <- gzfile(path, "rb")
    on.exit(close(con), add = TRUE)
    total <- 0
    repeat {
        block <- readBin(con, "raw", n = chunk_size)
        if (!length(block)) break
        total <- total + length(block)
    }
    total
}

atom_id <- function(a) as.character(get_in(a, c("Header", "AtomID"), NA_character_))[1]
atom_pid <- function(a) as.character(get_in(a, c("Who", "ParticipantID"), NA_character_))[1]
atom_start <- function(a) suppressWarnings(as.numeric(get_in(a, c("When", "TimeUTC"), NA_real_))[1])
atom_duration <- function(a) suppressWarnings(as.numeric(get_in(a, c("When", "Duration"), NA_real_))[1])
atom_label <- function(a) {
    value <- unlist(get_in(a, c("What", "Label"), character(0)), use.names = FALSE)
    if (length(value)) as.character(value[1]) else NA_character_
}

canonicalise <- function(x) {
    if (!is.list(x)) return(x)
    if (is.null(names(x))) return(lapply(x, canonicalise))
    x <- x[order(names(x))]
    lapply(x, canonicalise)
}

atom_signature <- function(a) jsonlite::toJSON(canonicalise(a), auto_unbox = TRUE, pretty = FALSE, na = "null")

write_csv <- function(x, path) {
    ensure_dir(dirname(path))
    utils::write.csv(x, path, row.names = FALSE, na = "")
    invisible(path)
}

utc_iso <- function(seconds) format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"),
                                     "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
