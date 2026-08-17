# CQ6 temporal retrieval and CQ7 duration summaries.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)
coel_require("jsonlite")

retrieve_window <- function(atoms, participant_id, window_start, window_end) {
    pid <- vapply(atoms, atom_pid, character(1)); start <- vapply(atoms, atom_start, numeric(1))
    duration <- vapply(atoms, atom_duration, numeric(1)); end <- start + duration
    idx <- which(pid == participant_id & start < window_end & end > window_start)
    result <- data.frame(index = idx, AtomID = vapply(atoms[idx], atom_id, character(1)),
        start_utc = start[idx], end_utc = end[idx], duration_sec = duration[idx],
        label = vapply(atoms[idx], atom_label, character(1)), stringsAsFactors = FALSE)
    if (nrow(result)) {
        result$exceeds_left <- result$start_utc < window_start
        result$exceeds_right <- result$end_utc > window_end
        result <- result[order(result$start_utc), ]
    }
    result
}

summarise_durations <- function(window_rows) {
    out <- stats::aggregate(duration_sec ~ label, window_rows, sum)
    out$duration_h <- out$duration_sec / 3600
    out[order(out$duration_sec, decreasing = TRUE), ]
}

plot_timeline <- function(rows, start, end, path, title) {
    ensure_dir(dirname(path)); grDevices::png(path, width = 10, height = 5, units = "in", res = 300)
    on.exit(grDevices::dev.off(), add = TRUE)
    labels <- unique(rows$label); y <- match(rows$label, labels)
    graphics::plot(as.POSIXct(c(start, end), origin = "1970-01-01", tz = "UTC"), c(1, length(labels)),
        type = "n", xlab = "UTC time", ylab = "", yaxt = "n", main = title)
    graphics::axis(2, at = seq_along(labels), labels = labels, las = 1)
    graphics::segments(as.POSIXct(rows$start_utc, origin = "1970-01-01", tz = "UTC"), y,
        as.POSIXct(rows$end_utc, origin = "1970-01-01", tz = "UTC"), y, lwd = 3)
    graphics::abline(v = as.POSIXct(c(start, end), origin = "1970-01-01", tz = "UTC"), lty = 2)
}

run_temporal_cqs <- function(config) {
    results <- list()
    for (stream in config$streams) {
        atoms <- read_json_any(stream$payload)
        participant_atoms <- atoms[vapply(atoms, atom_pid, character(1)) == config$participant_id]
        if (!length(participant_atoms)) stop("Configured participant is absent from ", stream$key, call. = FALSE)
        start <- min(vapply(participant_atoms, atom_start, numeric(1)), na.rm = TRUE)
        end <- start + config$window_seconds
        cq6 <- retrieve_window(atoms, config$participant_id, start, end)
        cq7 <- summarise_durations(cq6)
        out6 <- file.path(config$results_root, "CQ6")
        out7 <- file.path(config$results_root, "CQ7", stream$key)
        write_csv(cq6, file.path(out6, paste0("CQ6_", stream$key, ".csv")))
        write_csv(cq7, file.path(out7, "CQ7_duration_summary.csv"))
        plot_timeline(cq6, start, end, file.path(out6, paste0("CQ6_", stream$key, ".png")),
                      paste0(stream$display, ": first 48 hours"))
        results[[stream$key]] <- list(cq6 = cq6, cq7 = cq7)
    }
    results
}
