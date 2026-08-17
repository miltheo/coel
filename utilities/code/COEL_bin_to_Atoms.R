# Convert GENEAcore event outputs into classified behavioural bouts.

get_cut_times <- function(cut_time_24hr, MPI) {
    cut_time_24hr <- as.character(cut_time_24hr)[1]
    if (is.na(cut_time_24hr) || !grepl("^\\d{2}:\\d{2}$", cut_time_24hr)) {
        stop("cut_time_24hr must use HH:MM format.")
    }
    hour <- as.integer(substr(cut_time_24hr, 1, 2))
    minute <- as.integer(substr(cut_time_24hr, 4, 5))
    if (hour > 23L || minute > 59L) stop("cut_time_24hr must be between 00:00 and 23:59.")

    file_data <- MPI$file_data
    offset <- 3600L * hour + 60L * minute
    first_cut <- file_data$FirstLocalMidnightTimeUTC + offset
    if (file_data$MeasurementStartTimeUTC <= first_cut) first_cut <- first_cut - 86400L

    if (first_cut < file_data$MeasurementEndTimeUTC) {
        cut_times <- seq(first_cut, file_data$MeasurementEndTimeUTC, by = 86400L)
        cut_times <- c(file_data$MeasurementStartTimeUTC, cut_times, file_data$MeasurementEndTimeUTC)
    } else {
        cut_times <- c(file_data$MeasurementStartTimeUTC, file_data$MeasurementEndTimeUTC)
    }
    cut_times <- sort(unique(cut_times[cut_times >= file_data$MeasurementStartTimeUTC]))
    cut_times[c(TRUE, diff(cut_times) > 5)]
}

find_rest_intervals <- function(expansion_percent, cut_time_24hr, MPI) {
    expansion <- expansion_percent / 100
    still_bouts <- MPI$non_movement$still_bouts
    non_wear <- MPI$non_movement$non_wear
    if (is.null(still_bouts) || !nrow(still_bouts)) {
        MPI$non_movement$rest_intervals <- data.frame(
            start_time = numeric(), duration = numeric(), day_number = integer()
        )
        return(MPI)
    }

    still_bouts$end_time <- still_bouts$start_time + still_bouts$duration
    still_bouts$start_adj <- still_bouts$start_time - floor(still_bouts$duration * 2 * expansion)
    still_bouts$end_adj <- still_bouts$end_time + floor(still_bouts$duration * expansion)
    if (is.null(non_wear) || !nrow(non_wear)) {
        non_wear <- data.frame(start_time = numeric(), end_time = numeric())
    } else {
        non_wear$end_time <- non_wear$start_time + non_wear$duration
    }

    overlaps_non_wear <- function(start_time, end_time) {
        if (!nrow(non_wear)) return(FALSE)
        any(start_time <= non_wear$end_time & end_time >= non_wear$start_time)
    }

    result <- data.frame(start_time = numeric(), duration = numeric(), day_number = integer())
    cut_times <- get_cut_times(cut_time_24hr, MPI)
    for (day_number in seq_len(length(cut_times) - 1L)) {
        day_start <- cut_times[day_number]
        day_end <- cut_times[day_number + 1L]
        day_bouts <- still_bouts[
            still_bouts$start_time >= day_start & still_bouts$end_time < day_end,
            ,
            drop = FALSE
        ]
        if (!nrow(day_bouts)) next

        day_bouts <- day_bouts[order(day_bouts$start_time), , drop = FALSE]
        day_bouts$group <- 1L
        if (nrow(day_bouts) > 1L) {
            for (row in seq.int(2L, nrow(day_bouts))) {
                current_start <- day_bouts$start_adj[row]
                current_end <- day_bouts$end_adj[row]
                previous_end <- day_bouts$end_adj[row - 1L]
                new_group <- overlaps_non_wear(current_start, current_end) || current_start > previous_end
                day_bouts$group[row] <- day_bouts$group[row - 1L] + as.integer(new_group)
                if (!new_group) day_bouts$end_adj[row] <- max(current_end, previous_end)
            }
        }

        group_start <- aggregate(start_time ~ group, day_bouts, min)
        group_end <- aggregate(end_time ~ group, day_bouts, max)
        duration <- group_end$end_time - group_start$start_time
        primary <- which.max(duration)
        result <- rbind(
            result,
            data.frame(
                start_time = group_start$start_time[primary],
                duration = duration[primary],
                day_number = day_number
            )
        )
    }
    MPI$non_movement$rest_intervals <- result
    MPI
}

bouts_decision_tree <- function(bouts,
                                SDduration_threshold = 5.7e-5,
                                AGSA_threshold = 0.0625,
                                running_threshold = 0.407) {
    required <- c(
        "xSD", "ySD", "zSD", "Duration", "AGSAMean", "StepMean",
        "nonwear.time", "rest.time"
    )
    missing_columns <- setdiff(required, names(bouts))
    if (length(missing_columns)) {
        stop("Missing bout column(s): ", paste(missing_columns, collapse = ", "))
    }

    bouts$SDOverDuration <- (bouts$xSD + bouts$ySD + bouts$zSD) / (3 * bouts$Duration)
    bouts$NonWear <- bouts$nonwear.time > bouts$Duration / 2
    bouts$Rest <- bouts$rest.time > bouts$Duration / 2
    bouts$Active <- bouts$AGSAMean > AGSA_threshold
    bouts$Ambulatory <- bouts$Duration > 9 & bouts$StepMean > 45
    bouts$Vigorous <- bouts$AGSAMean > running_threshold
    bouts$FastWalk <- bouts$StepMean > 70
    bouts$Sleep <- bouts$SDOverDuration < SDduration_threshold
    bouts$Classification <- ifelse(
        bouts$NonWear,
        "NonWear",
        ifelse(
            bouts$Active,
            ifelse(
                bouts$Ambulatory,
                ifelse(bouts$Vigorous, "Run", ifelse(bouts$FastWalk, "FastWalk", "SlowWalk")),
                "Active"
            ),
            ifelse(bouts$Sleep, "Sleep", "Sedentary")
        )
    )
    names(bouts)[names(bouts) == "nonwear.time"] <- "NonWearTime"
    names(bouts)[names(bouts) == "rest.time"] <- "RestTime"
    bouts
}

interval_indicator <- function(intervals, measurement_start, measurement_end) {
    total_seconds <- as.integer(round(measurement_end - measurement_start))
    indicator <- integer(total_seconds)
    if (is.null(intervals) || !nrow(intervals)) return(indicator)

    for (i in seq_len(nrow(intervals))) {
        first <- max(1L, as.integer(floor(intervals$start_time[i] - measurement_start)) + 1L)
        last <- min(
            total_seconds,
            as.integer(ceiling(intervals$start_time[i] + intervals$duration[i] - measurement_start))
        )
        if (first <= last) indicator[first:last] <- 1L
    }
    indicator
}

mt_geneabout <- function(data_folder,
                         rest_expansion_percent = 13,
                         SDduration_threshold = 5.7e-5,
                         AGSA_threshold = 0.0625,
                         running_threshold = 0.407) {
    subfolders <- list.dirs(data_folder, full.names = TRUE, recursive = TRUE)
    for (folder in subfolders) {
        mpi_path <- list.files(folder, pattern = "MPI\\.rds$", full.names = TRUE)
        events_path <- list.files(folder, pattern = "events\\.rds$", full.names = TRUE)
        if (!length(mpi_path) || !length(events_path)) next

        MPI <- readRDS(mpi_path[1])
        bouts <- readRDS(events_path[1])
        MPI <- find_rest_intervals(rest_expansion_percent, MPI$file_data$CutTime24Hr, MPI)
        MPI$file_history <- rbind(
            MPI$file_history,
            paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " rest intervals calculated")
        )
        saveRDS(MPI, mpi_path[1])

        file_data <- MPI$file_data
        non_wear <- interval_indicator(
            MPI$non_movement$non_wear,
            file_data$MeasurementStartTimeUTC,
            file_data$MeasurementEndTimeUTC
        )
        rest <- interval_indicator(
            MPI$non_movement$rest_intervals,
            file_data$MeasurementStartTimeUTC,
            file_data$MeasurementEndTimeUTC
        )
        bout_lengths <- as.integer(round(diff(MPI$transitions$time)))
        if (sum(bout_lengths) != length(non_wear)) {
            stop("Transition durations do not span the measurement interval in ", mpi_path[1])
        }
        bout_id <- inverse.rle(list(lengths = bout_lengths, values = seq_along(bout_lengths)))
        coverage <- data.frame(bout = bout_id, non_wear = non_wear, rest = rest)
        bouts$nonwear.time <- aggregate(non_wear ~ bout, coverage, sum)$non_wear
        bouts$rest.time <- aggregate(rest ~ bout, coverage, sum)$rest
        bouts <- bouts_decision_tree(
            bouts,
            SDduration_threshold,
            AGSA_threshold,
            running_threshold
        )

        output_base <- file.path(folder, paste0(file_data$UniqueBinFileIdentifier, "_bouts"))
        saveRDS(bouts, paste0(output_base, ".rds"))
        utils::write.csv(bouts, paste0(output_base, ".csv"), row.names = FALSE)
    }
    invisible(TRUE)
}
