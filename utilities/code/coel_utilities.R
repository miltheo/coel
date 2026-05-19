# COEL utility helpers
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

coel_repo_root <- function(start = getwd()) {
    start <- normalizePath(start, winslash = "/", mustWork = TRUE)
    current <- start

    repeat {
        has_repo_markers <- file.exists(file.path(current, "README.md")) &&
            dir.exists(file.path(current, "models")) &&
            dir.exists(file.path(current, "atom"))

        if (has_repo_markers) return(current)

        parent <- dirname(current)
        if (identical(parent, current)) {
            stop("Could not locate the COEL repository root from: ", start, call. = FALSE)
        }
        current <- parent
    }
}

coel_path <- function(...) {
    file.path(coel_repo_root(), ...)
}

coel_require <- function(packages) {
    missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) {
        stop(
            "Install required R package(s) before running this script: ",
            paste(missing, collapse = ", "),
            call. = FALSE
        )
    }

    invisible(lapply(packages, library, character.only = TRUE))
}

ensure_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    invisible(path)
}

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L) y else x
}

read_json_any <- function(path) {
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
        gzfile(path, "rb")
    } else {
        file(path, "rb")
    }
    on.exit(close(con), add = TRUE)

    txt <- readLines(con, warn = FALSE, encoding = "UTF-8")
    jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
}

write_json_any <- function(x, path, gzip = TRUE) {
    ensure_dir(dirname(path))

    txt <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, na = "null")
    writeLines(txt, path, useBytes = TRUE)

    if (isTRUE(gzip)) {
        con <- gzfile(paste0(path, ".gz"), "wb")
        on.exit(close(con), add = TRUE)
        writeLines(txt, con, useBytes = TRUE)
    }

    invisible(path)
}
