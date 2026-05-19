# COEL temporal retrieval and summary utilities
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require("jsonlite")

repo_root <- coel_repo_root()
default_payload <- file.path(repo_root, "utilities", "data", "atoms_payload", "rest_activity", "coel_atoms_payload.json")
atoms <- read_json_any(default_payload)

okabe_ito <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")       # colour-blind friendly palette
pal_rest_activity <- c(
    wake     = "#4E79A7",  # sober blue
    rest     = "#F28E2B",  # sober orange
    non_wear = "#000000"   # black
)

get_pid <- function(a) a$Who$ParticipantID[1]                                              # extract participant id
get_t0  <- function(a) as.numeric(a$When$TimeUTC[1])                                       # extract start utc (sec)
get_dur <- function(a) as.numeric(a$When$Duration[1])                                      # extract duration (sec)

get_label <- function(a) {                                                                 # extract label (safe)
    x <- a$What$Label                                                                      # label field
    if (is.null(x)) "" else as.character(unlist(x))[1]                                     # first label or empty
}

get_ev <- function(a) {                                                                    # extract evidence type (safe)
    x <- a$How$EvidenceType                                                                # evidence field
    if (is.null(x)) "" else as.character(x[1])                                             # first value or empty
}

get_md <- function(a) {                                                                    # extract classification model (safe)
    x <- a$How$ClassificationModel                                                         # model field
    if (is.null(x)) "" else as.character(x[1])                                             # first value or empty
}

get_atom_id <- function(a) a$Header$AtomID[1]                                              # extract AtomID

# ------------------------------------------------------------
# Participant slice: indices, atoms, and coverage summary
# ------------------------------------------------------------
participant_slice <- function(atoms, participant_id) {                                     # slice payload to one participant
    pid  <- vapply(atoms, get_pid, "")                                                     # participant ids (vector)
    keep <- which(pid == participant_id)                                                   # indices for participant
    if (!length(keep)) {                                                                   # handle missing participant
        return(list(indices = integer(0), atoms = list(), coverage = data.frame()))        # return empty
    }
    t0  <- vapply(atoms[keep], get_t0, 0)                                                  # start times for participant
    dur <- vapply(atoms[keep], get_dur, 0)                                                 # durations for participant
    t1  <- t0 + dur                                                                        # end times for participant
    ord <- order(t0)                                                                       # order by time
    keep <- keep[ord]                                                                      # reorder indices
    data_cov <- data.frame(                                                                # build coverage row
        participant_id = participant_id,                                                   # participant id
        n_atoms        = length(keep),                                                     # number of atoms
        start_utc      = min(t0),                                                          # earliest start
        end_utc        = max(t1),                                                          # latest end
        span_sec       = max(t1) - min(t0),                                                # span seconds
        span_hours     = (max(t1) - min(t0)) / 3600,                                       # span hours
        span_days      = (max(t1) - min(t0)) / 86400,                                      # span days
        stringsAsFactors = FALSE                                                           # keep as strings
    )
    list(indices = keep, atoms = atoms[keep], coverage = data_cov)                         # return slice + coverage
}

# ------------------------------------------------------------
# CQ6: retrieve atoms within time window + timeline plot_fn
# ------------------------------------------------------------
retriever <- function(atoms, participant_id, window_start_utc, window_end_utc, plot = FALSE, tz = "UTC") {
    pid <- vapply(atoms, get_pid, "")                                                      # participant ids
    t0  <- vapply(atoms, get_t0, 0)                                                        # start utc (sec)
    dur <- vapply(atoms, get_dur, 0)                                                       # duration (sec)
    t1  <- t0 + dur                                                                        # end utc (sec)
    hit <- (pid == participant_id) & (t0 < window_end_utc) & (t1 > window_start_utc)       # window overlap
    idx <- which(hit)                                                                      # indices retained
    
    tab <- data.frame(                                                                     # audit table for CQ6
        index        = idx,                                                                # original index
        AtomID       = vapply(atoms[idx], get_atom_id, ""),                                # AtomID
        start_utc    = t0[idx],                                                            # start utc
        end_utc      = t1[idx],                                                            # end utc
        duration_sec = dur[idx],                                                           # duration
        label        = vapply(atoms[idx], get_label, ""),                                  # label
        stringsAsFactors = FALSE                                                           # keep as strings
    )
    
    if (nrow(tab)) {                                                                       # boundary annotations
        tab$exceeds_left  <- tab$start_utc < window_start_utc                              # starts before window
        tab$exceeds_right <- tab$end_utc   > window_end_utc                                # ends after window
        tab$overlap_class <- ifelse(!tab$exceeds_left & !tab$exceeds_right, "within",
                                    ifelse( tab$exceeds_left & !tab$exceeds_right, "left_boundary",
                                            ifelse(!tab$exceeds_left &  tab$exceeds_right, "right_boundary",
                                                   "spans_window")))
        tab <- tab[order(tab$start_utc), , drop = FALSE]                                   # sort by start time
    }
    
    plot_fn <- if (nrow(tab)) {                                                            # build plot function if data exists
        function() {
            x0 <- as.POSIXct(tab$start_utc, origin = "1970-01-01", tz = tz)
            x1 <- as.POSIXct(tab$end_utc,   origin = "1970-01-01", tz = tz)
            
            lab_raw <- trimws(tab$label)                                                         # keep PascalCase, trim only
            lab_present <- unique(lab_raw)                                                       # present labels (PascalCase)
            
            # Desired orders are written bottom -> top (first = bottom of y-axis)
            rest_bottom_to_top <- c("NonWear","Rest","Wake")                              # bottom -> top (rest stream)
            bout_bottom_to_top <- c("NonWear","Sleep","Sedentary","Active","SlowWalk","FastWalk","Run")  # bottom -> top (bout stream)
            
            bout_keys <- c("Run","FastWalk","SlowWalk","Active","Sedentary","Sleep")                    # exclude NonWear (shared)
            rest_keys <- c("Wake","Rest")                                                               # exclude NonWear (shared)
            
            mode <- if (any(lab_present %in% rest_keys)) "rest" else if (any(lab_present %in% bout_keys)) "bout" else "other"
            lev0 <- if (mode == "bout") bout_bottom_to_top else if (mode == "rest") rest_bottom_to_top else character(0)
            
            lev  <- c(lev0[lev0 %in% lab_present], setdiff(lab_present, lev0))                    # preserve order + append unknowns
            y0   <- match(lab_raw, lev)
            
            set.seed(1)                                                                    # deterministic jitter
            y   <- y0 + runif(length(y0), -0.12, 0.12)                                     # jitter to reduce overplotting
            
            op <- par(mar = c(7, 10, 2, 1) + 0.1, cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
            
            plot(range(c(x0, x1)), c(0.5, length(lev) + 0.5),                              # plot bounds
                 type = "n", xlab = "", ylab = "", yaxt = "n", xaxt = "n")                 # blank canvas
            axis(2, at = seq_along(lev), labels = lev, las = 2, cex.axis = 0.8)            # y labels
            mtext("Label", side = 2, line = 5)                                             # y-axis title
            
            ws <- as.POSIXct(window_start_utc, origin = "1970-01-01", tz = tz)             # window start (time)
            we <- as.POSIXct(window_end_utc,   origin = "1970-01-01", tz = tz)             # window end (time)
            span <- as.numeric(difftime(we, ws, units = "secs"))                           # window length (sec)
            
            by  <- if (span <= 36 * 3600) "6 hours" else "1 day"                           # tick spacing
            fmt <- if (span <= 36 * 3600) "%d/%m %H:%M" else "%d/%m/%Y"                    # tick labels
            ticks <- seq(ws, we, by = by)                                                  # ticks
            if (length(ticks) < 2) ticks <- c(ws, we)                                      # ensure labels for short windows
            
            axis.POSIXct(1, at = ticks, format = fmt, las = 2, cex.axis = 0.8)             # x axis
            mtext(if (span <= 36 * 3600) "Date and time" else "Date", side = 1, line = 5)  # x title
            
            abline(v = c(ws, we), lty = 2)                                                 # window bounds
            segments(x0, y, x1, y, lwd = 3)                                                # intervals
            par(op)                                                                        # restore par
        }
    } else NULL                                                                            # no plot if empty
    
    if (plot && !is.null(plot_fn)) plot_fn()                                               # draw now if requested
    
    list(indices = idx, atoms = atoms[idx], table = tab, plot_fn = plot_fn)                # return results + plot function
}

# ------------------------------------------------------------
# CQ7: summarise durations by label (and optionally per day)
# ------------------------------------------------------------
summariser <- function(atoms, participant_id, by_evidence = FALSE, by_model = FALSE,
                       per_day = FALSE, plot = FALSE, top_n = 12,
                       day_cut_hour = 3, palette_map = NULL) {
    
    pid  <- vapply(atoms, get_pid, "")                                                     # participant ids
    keep <- which(pid == participant_id)                                                   # indices for participant
    if (!length(keep)) return(list(summary = data.frame(), plot_fn = NULL))                # return empty if none
    
    lb  <- vapply(atoms[keep], get_label, "")                                              # labels
    dur <- vapply(atoms[keep], get_dur, 0)                                                 # durations (sec)
    ev  <- vapply(atoms[keep], get_ev, "")                                                 # evidence types
    md  <- vapply(atoms[keep], get_md, "")                                                 # classification models
    t0  <- vapply(atoms[keep], get_t0, 0)                                                  # start utc (sec)
    off <- vapply(atoms[keep], \(a) as.numeric(a$When$UTCOffset[1]), 0)                    # utc offset (sec)
    
    cut_sec <- as.numeric(day_cut_hour) * 3600                                             # day cut (sec)
    day <- as.Date(as.POSIXct(t0 + off - cut_sec, origin = "1970-01-01", tz = "UTC"))      # local day (cut)
    
    df <- data.frame(                                                                      # rowwise data for aggregation
        label                = lb,                                                         # label
        duration_sec         = dur,                                                        # duration seconds
        evidence_type        = ev,                                                         # evidence type
        classification_model = md,                                                         # model
        day                  = day,                                                        # local day (cut)
        stringsAsFactors     = FALSE                                                       # keep as strings
    )
    df <- df[nzchar(df$label) & !is.na(df$duration_sec) & df$duration_sec >= 0, , drop = FALSE] # basic cleaning
    
    grp <- c(if (per_day) "day", "label", if (by_evidence) "evidence_type", if (by_model) "classification_model")
    agg <- aggregate(df$duration_sec, df[grp], sum)
    names(agg)[names(agg) == "x"] <- "duration_sum_sec"
    agg$duration_sum_hrs <- agg$duration_sum_sec / 3600
    
    if (per_day) {
        agg <- agg[order(agg$day, -agg$duration_sum_sec), , drop = FALSE]
    } else {
        agg <- agg[order(-agg$duration_sum_sec), , drop = FALSE]
    }
    
    plot_fn <- if (nrow(agg)) {
        a <- agg; pd <- per_day; tn <- top_n; pm <- palette_map
        function() {
            if (!pd) {
                top  <- head(a, tn)
                xmax <- max(top$duration_sum_hrs, na.rm = TRUE) * 1.05
                
                lab_u <- unique(top$label)
                if (!is.null(pm) && length(names(pm))) {
                    bar_cols <- unname(pm[top$label])
                    miss <- is.na(bar_cols)
                    if (any(miss)) bar_cols[miss] <- rep(okabe_ito, length.out = sum(miss))
                } else {
                    col_map  <- setNames(rep(okabe_ito, length.out = length(lab_u)), lab_u)
                    bar_cols <- col_map[top$label]
                }
                
                op <- par(mar = c(5, 12, 2, 1) + 0.1, cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
                
                barplot(rev(top$duration_sum_hrs),
                        horiz = TRUE,
                        names.arg = rev(top$label),
                        col = rev(bar_cols),
                        border = NA,
                        las = 1,
                        xlim = c(0, xmax),
                        xaxt = "n",
                        xlab = "Duration (hours)",
                        ylab = "",
                        main = "Behaviour duration by label")
                
                axis(1, at = pretty(c(0, xmax)))
                mtext("Behaviour", side = 2, line = 6)
                par(op)
                
            } else {
                tmp <- aggregate(duration_sum_hrs ~ day + label, a, sum)
                top_lab <- head(names(sort(tapply(tmp$duration_sum_hrs, tmp$label, sum), decreasing = TRUE)), tn)
                tmp$label2 <- ifelse(tmp$label %in% top_lab, tmp$label, "Other")
                tmp2 <- aggregate(duration_sum_hrs ~ day + label2, tmp, sum)
                mat <- xtabs(duration_sum_hrs ~ label2 + day, tmp2)
                mat <- mat[order(rowSums(mat), decreasing = TRUE), , drop = FALSE]
                
                d <- as.Date(colnames(mat))
                o <- order(d)
                mat <- mat[, o, drop = FALSE]
                colnames(mat) <- format(d[o], "%d/%m/%Y")
                
                labs <- rownames(mat)
                if (!is.null(pm) && length(names(pm))) {
                    cols <- unname(pm[labs])
                    miss <- is.na(cols)
                    if (any(miss)) cols[miss] <- rep(okabe_ito, length.out = sum(miss))
                } else {
                    cols <- rep(okabe_ito, length.out = length(labs))
                }
                if ("Other" %in% labs) cols[labs == "Other"] <- "#BDBDBD"
                
                op <- par(mar = c(10, 4, 3, 10) + 0.1, xpd = TRUE, cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
                
                max_y <- max(colSums(mat), na.rm = TRUE)                       # tallest stacked day
                yt    <- pretty(c(0, max_y * 1.05))                            # propose nice ticks (often includes 25)
                ymax  <- max(yt)                                               # use top tick as axis max
                ylim  <- c(0, ymax)
                
                barplot(mat, beside = FALSE, col = cols, border = NA,
                        las = 2, cex.names = 0.8,
                        ylab = "Duration (hours)",
                        ylim = ylim,
                        yaxt = "n",
                        main = "Behaviour by day (stacked)")
                
                axis(2, at = yt, las = 1)
                
                mtext("Date", side = 1, line = 5)
                
                legend("topright", inset = c(-0.1, 0), bty = "n", cex = 0.8,
                       fill = rev(cols), legend = rev(labs))
                par(op)
            }
        }
    } else NULL
    
    if (plot && !is.null(plot_fn)) plot_fn()
    
    list(summary = agg, plot_fn = plot_fn)
}

run_cq6_cq7_example <- function(participant_id,
                                payload = default_payload,
                                out_dir = file.path(repo_root, "utilities", "plots")) {
    example_atoms <- read_json_any(payload)
    ensure_dir(out_dir)

    ps <- participant_slice(example_atoms, participant_id)
    if (!nrow(ps$coverage)) stop("Participant not found in payload: ", participant_id)

    cq6 <- retriever(
        example_atoms,
        ps$coverage$participant_id,
        ps$coverage$start_utc,
        ps$coverage$start_utc + 48 * 60 * 60,
        plot = FALSE
    )

    cq7_day <- summariser(
        example_atoms,
        participant_id,
        per_day = TRUE,
        plot = FALSE,
        day_cut_hour = 3,
        palette_map = pal_rest_activity
    )

    png(file.path(out_dir, "CQ6_timeline.png"), 10, 5, units = "in", res = 600, pointsize = 12)
    cq6$plot_fn()
    dev.off()

    png(file.path(out_dir, "CQ7_duration_by_day.png"), 10, 8, units = "in", res = 600, pointsize = 12)
    cq7_day$plot_fn()
    dev.off()

    list(cq6 = cq6$table, cq7 = cq7_day$summary)
}

