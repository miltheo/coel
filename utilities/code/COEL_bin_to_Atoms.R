# COEL bin-to-Atom preparation utilities
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require(c("readr", "dplyr", "jsonlite", "GENEAcore"))

#-------------------------------------------------------------------------------

# Functions=====================================================================
builder_path <- file.path("utilities", "code", "COEL_json_builder.R")
if (!file.exists(builder_path)) builder_path <- "COEL_json_builder.R"
source(builder_path)

# Function to prototype conversion of GENEAcore events into behavioural bouts
proto_geneabout <- function(data_folder) {

    # find all the output folders from the processed bin files in the data_folder
    subfolders = list.dirs(data_folder, full.names = TRUE, recursive = TRUE)

    # for each binfile
    for (a in 2:length(subfolders)) {

        # extract the MPI and event data to be classified
        mpi_path = list.files(subfolders[a], pattern = "MPI.rds", full.names = TRUE)
        events_path = list.files(subfolders[a], pattern = "events.rds", full.names = TRUE)
        MPI = readRDS(mpi_path)
        bouts = readRDS(events_path)

        # add in the rest intervals
        MPI = find_rest_intervals(13, MPI$file_data$CutTime24Hr, MPI)
        MPI$file_history = rbind(MPI$file_history, paste0(substr(Sys.time(), 0, 23), " rest intervals calculated"))
        saveRDS(MPI, mpi_path)

        # create non-wear time by bout
        if (nrow(MPI$non_movement$non_wear) != 0) {
            one_sequence = data.frame(
                start_time = MPI$non_movement$non_wear$start_time,
                lengths = MPI$non_movement$non_wear$duration,
                values = 1
            )
            wear_start = append(MPI$file_data$MeasurementStartTimeUTC, (MPI$non_movement$non_wear$start_time + MPI$non_movement$non_wear$duration))
            wear_end = append(MPI$non_movement$non_wear$start_time, MPI$file_data$MeasurementEndTimeUTC)
            zero_sequence = data.frame(
                start_time = wear_start,
                lengths = wear_end - wear_start,
                values = 0
            )
            non_wear_sequence = rbind(zero_sequence, one_sequence)
            non_wear_sequence = non_wear_sequence[order(non_wear_sequence$start_time),]
            non_wear_sequence = subset(non_wear_sequence, select = -start_time)
            non_wear_sequence = inverse.rle(non_wear_sequence)
        } else {
            non_wear_sequence = rep(0, MPI$file_data$MeasurementEndTimeUTC-MPI$file_data$MeasurementStartTimeUTC)
        }

        # create rest interval time by bout
        if (nrow(MPI$non_movement$rest_intervals) != 0) {
            one_sequence = data.frame(
                start_time = MPI$non_movement$rest_intervals$start_time,
                lengths = MPI$non_movement$rest_intervals$duration,
                values = 1
            )
            move_start = append(MPI$file_data$MeasurementStartTimeUTC, (MPI$non_movement$rest_intervals$start_time + MPI$non_movement$rest_intervals$duration))
            move_end = append(MPI$non_movement$rest_intervals$start_time, MPI$file_data$MeasurementEndTimeUTC)
            zero_sequence = data.frame(
                start_time = move_start,
                lengths = move_end - move_start,
                values = 0
            )
            rest_sequence = rbind(zero_sequence, one_sequence)
            rest_sequence = rest_sequence[order(rest_sequence$start_time),]
            rest_sequence = subset(rest_sequence, select = -start_time)
            rest_sequence = inverse.rle(rest_sequence)
        } else {
            rest_sequence = rep(0, MPI$file_data$MeasurementEndTimeUTC-MPI$file_data$MeasurementStartTimeUTC)
        }

        # allocate non-wear & rest interval time to bouts
        bout_sequence = data.frame(
            lengths = diff(MPI$transitions$time),
            values = seq(1,length(diff(MPI$transitions$time)))
        )
        bout_sequence = inverse.rle(bout_sequence)
        bout_sequence = data.frame(
            bout_sequence,
            non_wear_sequence,
            rest_sequence
        )
        bout_non_wear = aggregate(data = bout_sequence, non_wear_sequence ~ bout_sequence, FUN = sum)
        bout_rest = aggregate(data = bout_sequence, rest_sequence ~ bout_sequence, FUN = sum)

        bouts$nonwear.time = bout_non_wear$non_wear_sequence
        bouts$rest.time = bout_rest$rest_sequence

        # decision tree thresholds
        bouts$xyz_sd <- (bouts$xSD + bouts$ySD + bouts$zSD) / 3
        bouts$non_wear <- ifelse(bouts$nonwear.time > bouts$Duration / 2, TRUE, FALSE)
        bouts$rest <- ifelse(bouts$rest.time > bouts$Duration / 2, TRUE, FALSE)
        bouts$active <- ifelse(bouts$AGSAMean > 0.0625, TRUE, FALSE)
        bouts$ambulatory <- ifelse((bouts$Duration > 9) & (bouts$StepMean > 45), TRUE, FALSE)
        bouts$vigourous <- ifelse(bouts$AGSAMean > 0.407, TRUE, FALSE)
        bouts$fastwalk <- ifelse(bouts$StepMean > 70, TRUE, FALSE)
        bouts$rest_activity <- ifelse(bouts$non_wear, "non_wear",
                                      ifelse(bouts$rest, "rest", "wake")
        )
        bouts$behavioural_bouts <- ifelse(
            bouts$xyz_sd < 0.04 & bouts$Duration >= 300 & bouts$rest, "sleep",
            ifelse(bouts$non_wear, "non_wear",
                   ifelse(bouts$active,
                          ifelse(bouts$ambulatory,
                                 ifelse(bouts$vigourous, "run",
                                        ifelse(bouts$fastwalk, "fast_walk", "slow_walk")
                                 ),
                                 "active"
                          ),
                          "sedentary"
                   )
            )
        )

        # write out the results
        bouts_path = paste0(subfolders[a], "/", MPI$file_data$UniqueBinFileIdentifier, "_bouts")
        saveRDS(bouts, paste0(bouts_path, ".rds"))
        write.csv(bouts, paste0(bouts_path, ".csv"), row.names = FALSE)

    }
}


# Function to determine recommended primary rest at given expansion with twice the expansion before than after
find_rest_intervals <- function(expansion_percent, cut_time_24hr, MPI) {
    expansion <- expansion_percent / 100
    still_bouts <- MPI[["non_movement"]][["still_bouts"]]
    non_wear <- MPI[["non_movement"]][["non_wear"]]
    non_wear$end <- non_wear$start + non_wear$duration

    still_bouts$end_time <- still_bouts$start_time + still_bouts$duration
    still_bouts$start_adj <- still_bouts$start_time - floor(still_bouts$duration * 2 * expansion)
    still_bouts$end_adj <- still_bouts$start_time + still_bouts$duration + floor(still_bouts$duration * expansion)

    result <- data.frame()
    cut_times <- get_cut_times(cut_time_24hr, MPI)

    is_in_non_wear <- function(start_time, end_time, non_wear) {
        any(
            (start_time >= non_wear$start & start_time <= non_wear$end) |
                (end_time >= non_wear$start & end_time <= non_wear$end) |
                (start_time <= non_wear$start & end_time >= non_wear$end)
        )
    }

    for (day_number in 1:(length(cut_times) - 1)) {
        day_start <- cut_times[day_number]
        day_end <- cut_times[day_number + 1]

        day_still_bouts <- still_bouts[(still_bouts$start_time >= day_start & still_bouts$end_time < day_end), ]
        day_still_bouts$StartTime <- as.POSIXct(day_still_bouts$start_time, origin="1970-01-01")
        day_still_bouts$EndTime <- as.POSIXct(day_still_bouts$end_time, origin="1970-01-01")
        day_still_bouts$StartAdj <- as.POSIXct(day_still_bouts$start_adj, origin="1970-01-01")
        day_still_bouts$EndAdj <- as.POSIXct(day_still_bouts$end_adj, origin="1970-01-01")
        if (nrow(day_still_bouts) > 0) {
            day_still_bouts <- day_still_bouts[order(day_still_bouts$start_time), ]
            day_still_bouts$group <- 1
            if (nrow(day_still_bouts) > 1) {
                for (row in 2:nrow(day_still_bouts)) {
                    current_start <- day_still_bouts$start_adj[row]
                    current_end <- day_still_bouts$end_adj[row]
                    prev_end <- day_still_bouts$end_adj[row - 1]

                    # Check if current interval touches a non_wear period
                    if (is_in_non_wear(current_start, current_end, non_wear)) {
                        print(paste(MPI$file_data$BinfileName, "Day", day_number, "Row", row))
                        # Start a new group due to overlap with non_wear
                        day_still_bouts$group[row] <- day_still_bouts$group[row - 1] + 1
                    } else if (current_start <= prev_end) {
                        # Overlaps with previous bout and no non_wear, place in same group
                        day_still_bouts$group[row] <- day_still_bouts$group[row - 1]
                        day_still_bouts$end_adj[row] <- max(current_end, prev_end)
                    } else {
                        # No overlap, place in new group
                        day_still_bouts$group[row] <- day_still_bouts$group[row - 1] + 1
                    }
                }
            }

            day_still_bouts$end_time <- day_still_bouts$start_time + day_still_bouts$duration
            group_starts <- aggregate(start_time ~ group, data = day_still_bouts, FUN = min)
            group_ends <- aggregate(end_time ~ group, data = day_still_bouts, FUN = max)

            rest_bouts <- data.frame(
                start = group_starts$start_time,
                end = group_ends$end_time,
                duration = group_ends$end_time - group_starts$start_time
            )

            if (nrow(rest_bouts) > 0) {
                primary <- rest_bouts[which.max(rest_bouts$duration), ]
                result <- rbind(result, data.frame(
                    start_time = primary$start,
                    duration = primary$duration,
                    day_number = day_number
                ))
            }
        }
    }

    MPI$non_movement[["rest_intervals"]] <- result
    return(MPI)
}


# Function to determine UTC 24hr cut times in bin file from time of day setting
get_cut_times <- function(cut_time_24hr, MPI) {

    # inline validation (replaces missing check_time_format)
    cut_time_24hr <- as.character(cut_time_24hr)[1]
    if (is.na(cut_time_24hr) || !nzchar(cut_time_24hr)) stop("cut_time_24hr is missing.")
    if (!grepl("^\\d{2}:\\d{2}$", cut_time_24hr)) stop("cut_time_24hr must be 'HH:MM'.")

    hh <- as.numeric(substr(cut_time_24hr, 1, 2))
    mm <- as.numeric(substr(cut_time_24hr, 4, 5))
    if (!is.finite(hh) || !is.finite(mm) || hh < 0 || hh > 23 || mm < 0 || mm > 59) stop("cut_time_24hr out of range 00:00-23:59.")

    cut_time_24hr_offset <- 60 * (60 * hh + mm)

    if ((MPI$file_data[["MeasurementStartTimeUTC"]] - MPI$file_data[["FirstLocalMidnightTimeUTC"]]) > cut_time_24hr_offset) {
        local_first_cut_time <- MPI$file_data[["FirstLocalMidnightTimeUTC"]] + cut_time_24hr_offset
    } else {
        local_first_cut_time <- MPI$file_data[["FirstLocalMidnightTimeUTC"]] - (24 * 60 * 60 - cut_time_24hr_offset)
    }

    if (local_first_cut_time < MPI$file_data[["MeasurementEndTimeUTC"]]) {
        cut_times <- seq(local_first_cut_time, MPI$file_data[["MeasurementEndTimeUTC"]], 24 * 60 * 60)
        cut_times <- c(MPI$file_data[["MeasurementStartTimeUTC"]], cut_times, MPI$file_data[["MeasurementEndTimeUTC"]])
    } else {
        cut_times <- c(MPI$file_data[["MeasurementStartTimeUTC"]], MPI$file_data[["MeasurementEndTimeUTC"]])
    }

    cut_times <- sort(unique(cut_times))
    cut_times <- cut_times[cut_times >= MPI$file_data[["MeasurementStartTimeUTC"]]]
    cut_times <- cut_times[c(TRUE, diff(cut_times) > 5)]

    return(cut_times)
}


#' Bouts Decision Tree
#'
#' @param bouts Aggregated events after non-wear and rest coverage have been assigned.
#' @param SDduration_threshold Threshold for sleep classification. Uses the average x,y,z standard deviation over duration metric.
#' @param AGSA_threshold Threshold for active classification.
#' @returns Classified bouts
#' @keywords utilities
bouts_decision_tree <- function(bouts,
                                SDduration_threshold = 5.7e-5,
                                AGSA_threshold = 0.0625,
                                running_threshold = 0.407) {
    bouts$sd_over_dur <- (bouts$xSD + bouts$ySD + bouts$zSD) / (3 * bouts$Duration)

    # decision tree thresholds
    bouts$non_wear <- ifelse(bouts$nonwear.time > bouts$Duration / 2, TRUE, FALSE)
    bouts$rest <- ifelse(bouts$rest.time > bouts$Duration / 2, TRUE, FALSE)
    bouts$active <- ifelse(bouts$AGSAMean > AGSA_threshold, TRUE, FALSE)
    bouts$ambulatory <- ifelse((bouts$Duration > 9) & (bouts$StepMean > 45), TRUE, FALSE)
    bouts$vigourous <- ifelse(bouts$AGSAMean > running_threshold, TRUE, FALSE)
    bouts$fastwalk <- ifelse(bouts$StepMean > 70, TRUE, FALSE)
    bouts$sleep <- ifelse(bouts$sd_over_dur < SDduration_threshold, TRUE, FALSE)

    # bout classification
    bouts$classification <- ifelse(bouts$non_wear, "non_wear",
                                   ifelse(bouts$active,
                                          ifelse(bouts$ambulatory,
                                                 ifelse(bouts$vigourous, "run",
                                                        ifelse(bouts$fastwalk, "fast_walk",
                                                               "slow_walk"
                                                        )
                                                 ),
                                                 "active"
                                          ),
                                          ifelse(bouts$sleep, "sleep", "sedentary")
                                   )
    )

    new_names <- c(
        nonwear.time = "NonWearTime",
        rest.time = "RestTime",
        sd_over_dur = "SDOverDuration",
        non_wear = "NonWear",
        rest = "Rest",
        active = "Active",
        ambulatory = "Ambulatory",
        vigourous = "Vigorous",
        fastwalk = "FastWalk",
        sleep = "Sleep",
        classification = "Classification"
    )

    names(bouts)[names(bouts) %in% names(new_names)] <-
        new_names[names(bouts)[names(bouts) %in% names(new_names)]]

    return(bouts)
}

# Function combining proto_geneabout() workflow with the updated decision tree (clean + single output schema)
mt_geneabout <- function(data_folder,
                         rest_expansion_percent = 13,
                         SDduration_threshold = 5.7e-5,
                         AGSA_threshold = 0.0625,
                         running_threshold = 0.407) {

    subfolders <- list.dirs(data_folder, full.names = TRUE, recursive = TRUE)                    # processed subfolders

    for (a in 2:length(subfolders)) {                                                            # loop subfolders

        mpi_path    <- list.files(subfolders[a], pattern = "MPI.rds$",    full.names = TRUE)       # MPI file
        events_path <- list.files(subfolders[a], pattern = "events.rds$", full.names = TRUE)       # events file
        if (!length(mpi_path) || !length(events_path)) next                                        # skip

        MPI   <- readRDS(mpi_path[1])                                                              # read MPI
        bouts <- readRDS(events_path[1])                                                           # read events as bouts

        MPI <- find_rest_intervals(rest_expansion_percent, MPI$file_data$CutTime24Hr, MPI)         # rest intervals
        MPI$file_history <- rbind(MPI$file_history, paste0(substr(Sys.time(), 0, 23), " rest intervals calculated")) # log
        saveRDS(MPI, mpi_path[1])                                                                  # save MPI

        if (nrow(MPI$non_movement$non_wear) != 0) {                                                 # non-wear vector
            one <- data.frame(start_time = MPI$non_movement$non_wear$start_time,
                              lengths    = MPI$non_movement$non_wear$duration,
                              values     = 1)
            ws  <- append(MPI$file_data$MeasurementStartTimeUTC, MPI$non_movement$non_wear$start_time + MPI$non_movement$non_wear$duration)
            we  <- append(MPI$non_movement$non_wear$start_time, MPI$file_data$MeasurementEndTimeUTC)
            zero <- data.frame(start_time = ws, lengths = we - ws, values = 0)
            nw   <- rbind(zero, one); nw <- nw[order(nw$start_time), ]; nw <- inverse.rle(subset(nw, select = -start_time))
        } else nw <- rep(0, MPI$file_data$MeasurementEndTimeUTC - MPI$file_data$MeasurementStartTimeUTC)

        if (nrow(MPI$non_movement$rest_intervals) != 0) {                                           # rest vector
            one <- data.frame(start_time = MPI$non_movement$rest_intervals$start_time,
                              lengths    = MPI$non_movement$rest_intervals$duration,
                              values     = 1)
            ms  <- append(MPI$file_data$MeasurementStartTimeUTC, MPI$non_movement$rest_intervals$start_time + MPI$non_movement$rest_intervals$duration)
            me  <- append(MPI$non_movement$rest_intervals$start_time, MPI$file_data$MeasurementEndTimeUTC)
            zero <- data.frame(start_time = ms, lengths = me - ms, values = 0)
            rs   <- rbind(zero, one); rs <- rs[order(rs$start_time), ]; rs <- inverse.rle(subset(rs, select = -start_time))
        } else rs <- rep(0, MPI$file_data$MeasurementEndTimeUTC - MPI$file_data$MeasurementStartTimeUTC)

        bs <- inverse.rle(data.frame(lengths = diff(MPI$transitions$time), values = seq_len(length(diff(MPI$transitions$time))))) # bout id per second
        bx <- data.frame(bout = bs, nw = nw, rs = rs)                                                   # second-level table
        bouts$nonwear.time <- aggregate(nw ~ bout, bx, sum)$nw                                            # non-wear seconds
        bouts$rest.time    <- aggregate(rs ~ bout, bx, sum)$rs                                            # rest seconds

        bouts$SDOverDuration <- (bouts$xSD + bouts$ySD + bouts$zSD) / (3 * bouts$Duration)                # sd/duration
        bouts$NonWear   <- bouts$nonwear.time > (bouts$Duration / 2)                                      # flags
        bouts$Rest      <- bouts$rest.time    > (bouts$Duration / 2)
        bouts$Active    <- bouts$AGSAMean     > AGSA_threshold
        bouts$Ambulatory<- (bouts$Duration > 9) & (bouts$StepMean > 45)
        bouts$Vigorous  <- bouts$AGSAMean     > running_threshold
        bouts$FastWalk  <- bouts$StepMean     > 70
        bouts$Sleep     <- bouts$SDOverDuration < SDduration_threshold

        cls <- ifelse(bouts$NonWear, "NonWear",
                      ifelse(bouts$Active,
                             ifelse(bouts$Ambulatory,
                                    ifelse(bouts$Vigorous, "Run", ifelse(bouts$FastWalk, "FastWalk", "SlowWalk")),
                                    "Active"),
                             ifelse(bouts$Sleep, "Sleep", "Sedentary")))
        bouts$Classification <- cls                                                                      # PascalCase values

        names(bouts)[names(bouts) == "nonwear.time"] <- "NonWearTime"                                    # final names
        names(bouts)[names(bouts) == "rest.time"]    <- "RestTime"
        bouts$nonwear.time <- NULL; bouts$rest.time <- NULL                                               # remove raw names

        out_base <- file.path(subfolders[a], paste0(MPI$file_data$UniqueBinFileIdentifier, "_bouts"))     # output path
        saveRDS(bouts, paste0(out_base, ".rds"))                                                          # save RDS
        write.csv(bouts, paste0(out_base, ".csv"), row.names = FALSE)                                     # save CSV
    }
}

# Usage:
# - Run GENEAcore on a local source data folder outside the repository.
# - Use `mt_geneabout(data_folder)` to derive Behavioural Bout classifications.
# - Use functions in `COEL_json_builder.R` to create per-participant Atom JSON.
# - Combine public example Atom files with `combine_atoms_to_payload()`.
