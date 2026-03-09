# app.R
# COEL Web Application
# Features:
# 1) Upload Atom payload (.json or .json.gz)
# 2) Structural validation (TEST1, TEST2, TEST3 only)
# 3) Payload summary (participants, span, labels, evidence, model)
# 4) CQ6 window retrieval + timeline plot (ordered y-axis)
# 5) CQ7 per-day duration summary + plot
# 6) JSON-LD projection using a context.jsonld (optional upload or default path)

# ----------------------------
# Packages
# ----------------------------
pk <- c("shiny","jsonlite","DT")
for (p in pk) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(shiny)
library(jsonlite)
library(DT)

# ----------------------------
# Utils
# ----------------------------
options(shiny.maxRequestSize = 100 * 1024^2)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

parse_utc <- function(x) as.numeric(as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))

read_json_any <- function(path) {
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rb") else file(path, "rb")
    on.exit(close(con), add = TRUE)
    jsonlite::fromJSON(con, simplifyVector = FALSE)
}

write_json_any <- function(x, path, gzip = FALSE) {
    txt <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, na = "null")
    writeLines(txt, path, useBytes = TRUE)
    if (isTRUE(gzip)) {
        gz <- gzfile(paste0(path, ".gz"), "wb")
        on.exit(close(gz), add = TRUE)
        writeLines(txt, gz, useBytes = TRUE)
    }
    invisible(path)
}

as_num1 <- function(x) suppressWarnings(as.numeric((x %||% NA_real_)[1]))
as_chr1 <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    if (is.atomic(x) && length(x) == 1) return(as.character(x))
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    x <- as.character(x)
    if (length(x)) x[1] else ""
}

canon <- function(x) {
    if (!is.list(x)) return(x)
    nm <- names(x) %||% character(0)
    if (!length(nm)) return(lapply(x, canon))
    o <- order(nm)
    x <- x[o]
    for (k in names(x)) x[[k]] <- canon(x[[k]])
    x
}

mint_atom_urn <- function(atom_id, prefix = "urn:coel:atom:") {
    paste0(prefix, utils::URLencode(as.character(atom_id), reserved = TRUE))
}

is_list <- function(x) is.list(x)

safe_chr_vec <- function(x) {                                                 # Flatten to character vector
    if (is.null(x) || length(x) == 0) return(character(0))
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    x <- as.character(x)
    x[nzchar(x) & !is.na(x)]
}

github_raw <- function(u) {                                                   # GitHub blob -> raw
    sub("https://github.com/([^/]+)/([^/]+)/blob/([^?]+).*",
        "https://raw.githubusercontent.com/\\1/\\2/\\3", u)
}

to_src <- function(p) {                                                       # Accept path or URL
    if (grepl("^https?://", p)) github_raw(p) else p
}

read_registry_labels <- function(path, col_keys = c("label","name","term","code")) {
    df <- read.csv(to_src(path), stringsAsFactors = FALSE, check.names = FALSE)
    cn <- tolower(names(df))
    pick <- NA_integer_
    for (k in col_keys) { j <- which(grepl(k, cn, fixed = TRUE)); if (length(j)) { pick <- j[1]; break } }
    if (is.na(pick)) stop("Registry file missing a label-like column: ", path)
    labs <- as.character(df[[pick]])
    labs <- labs[nzchar(labs) & !is.na(labs)]
    unique(labs)
}

sig_full <- function(a) {                                                     # Full-content signature (canonical JSON)
    if (!is_list(a)) return(NA_character_)
    jsonlite::toJSON(canon(a), auto_unbox = TRUE, pretty = FALSE, na = "null")
}

okabe_ito <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")       # Okabe-Ito

save_plot_png <- function(plot_fn, path, w = 12, h = 4, res = 600) {
    png(path, width = w, height = h, units = "in", res = res, pointsize = 12)
    on.exit(dev.off(), add = TRUE)
    plot_fn()
    invisible(path)
}

# ----------------------------
# Atom field getters (safe)
# ----------------------------
get_pid <- function(a) {
    if (!is_list(a) || !is_list(a$Who)) return("")
    as_chr1(a$Who$ParticipantID)
}

get_t0 <- function(a) {
    if (!is_list(a) || !is_list(a$When)) return(NA_real_)
    as_num1(a$When$TimeUTC)
}

get_dur <- function(a) {
    if (!is_list(a) || !is_list(a$When)) return(NA_real_)
    as_num1(a$When$Duration)
}

get_off <- function(a) {
    if (!is_list(a) || !is_list(a$When)) return(0)
    as_num1(a$When$UTCOffset)
}

get_ev <- function(a) {
    if (!is_list(a) || !is_list(a$How)) return("")
    as_chr1(a$How$EvidenceType)
}

get_md <- function(a) {
    if (!is_list(a) || !is_list(a$How)) return("")
    as_chr1(a$How$ClassificationModel)
}

get_atom_id <- function(a) {
    if (!is_list(a) || !is_list(a$Header)) return("")
    as_chr1(a$Header$AtomID)
}

get_label <- function(a) {
    if (!is_list(a) || !is_list(a$What)) return("")
    x <- a$What$Label
    if (is.null(x)) "" else as.character(unlist(x, use.names = FALSE))[1]
}

get_models_vec <- function(a) {                                          # How.ClassificationModel as vector
    if (!is_list(a) || !is_list(a$How)) return(character(0))
    safe_chr_vec(a$How$ClassificationModel)
}

get_labels_vec <- function(a) {                                          # What.Label as vector
    if (!is_list(a) || !is_list(a$What)) return(character(0))
    safe_chr_vec(a$What$Label)
}

# ----------------------------
# Structural validator (TEST1-TEST3 only)
# ----------------------------
validator_structural <- function(atoms, run_TEST1 = TRUE, run_TEST2 = TRUE, run_TEST3 = TRUE) {
    n <- length(atoms)
    
    get_path <- function(x, p) {
        for (k in p) {
            if (!is.list(x) || is.null(x[[k]])) return(NULL)
            x <- x[[k]]
        }
        x
    }
    
    ids <- vapply(atoms, function(a) {
        x <- get_path(a, c("Header","AtomID"))
        if (is.character(x) && length(x)) x[1] else NA_character_
    }, character(1))
    ids[is.na(ids) | !nzchar(ids)] <- paste0("index_", which(is.na(ids) | !nzchar(ids)))
    
    out_tests <- vector("list", n)
    out_detail <- vector("list", n)
    
    for (i in seq_len(n)) {
        a <- atoms[[i]]
        tests <- character(0)
        dets  <- character(0)
        
        if (run_TEST1) {
            miss <- character(0)
            if (is.null(get_path(a, c("Header")))) miss <- c(miss, "Header")
            if (is.null(get_path(a, c("When"))))   miss <- c(miss, "When")
            if (is.null(get_path(a, c("What"))))   miss <- c(miss, "What")
            if (is.null(get_path(a, c("Who"))))    miss <- c(miss, "Who")
            if (length(miss)) {
                tests <- c(tests, "TEST1")
                dets  <- c(dets, paste("TEST1:", paste(miss, collapse = " | ")))
            }
        }
        
        if (run_TEST2) {
            miss <- character(0)
            if (is.null(get_path(a, c("Header","AtomVersion")))) miss <- c(miss, "Header.AtomVersion")
            if (is.null(get_path(a, c("When","TimeUTC"))))       miss <- c(miss, "When.TimeUTC")
            if (is.null(get_path(a, c("When","Duration"))))      miss <- c(miss, "When.Duration")
            if (is.null(get_path(a, c("When","UTCOffset"))))     miss <- c(miss, "When.UTCOffset")
            if (is.null(get_path(a, c("What","Label"))))         miss <- c(miss, "What.Label")
            pid0 <- get_path(a, c("Who","ParticipantID"))
            eid0 <- get_path(a, c("Who","EnvironmentID"))
            if (is.null(pid0) && is.null(eid0)) miss <- c(miss, "Who.ParticipantID|Who.EnvironmentID")
            if (length(miss)) {
                tests <- c(tests, "TEST2")
                dets  <- c(dets, paste("TEST2:", paste(miss, collapse = " | ")))
            }
        }
        
        if (run_TEST3) {
            bad <- character(0)
            hdr <- get_path(a, c("Header"))
            whn <- get_path(a, c("When"))
            wht <- get_path(a, c("What"))
            who <- get_path(a, c("Who"))
            if (!is.null(hdr) && !is.list(hdr)) bad <- c(bad, "Header")
            if (!is.null(whn) && !is.list(whn)) bad <- c(bad, "When")
            if (!is.null(wht) && !is.list(wht)) bad <- c(bad, "What")
            if (!is.null(who) && !is.list(who)) bad <- c(bad, "Who")
            
            av  <- get_path(a, c("Header","AtomVersion"))
            tu  <- get_path(a, c("When","TimeUTC"))
            du  <- get_path(a, c("When","Duration"))
            of  <- get_path(a, c("When","UTCOffset"))
            pid <- get_path(a, c("Who","ParticipantID"))
            eid <- get_path(a, c("Who","EnvironmentID"))
            lb  <- get_path(a, c("What","Label"))
            
            if (!is.null(av)  && !is.character(av)) bad <- c(bad, "Header.AtomVersion")
            if (!is.null(tu)  && !(is.numeric(tu) || is.integer(tu))) bad <- c(bad, "When.TimeUTC")
            if (!is.null(du)  && !(is.numeric(du) || is.integer(du))) bad <- c(bad, "When.Duration")
            if (!is.null(of)  && !(is.numeric(of) || is.integer(of))) bad <- c(bad, "When.UTCOffset")
            if (!is.null(pid) && !is.character(pid)) bad <- c(bad, "Who.ParticipantID")
            if (!is.null(eid) && !is.character(eid)) bad <- c(bad, "Who.EnvironmentID")
            
            if (!is.null(lb)) {
                ok <- is.list(lb) && length(lb) >= 1 && all(vapply(lb, is.character, logical(1)))
                if (!ok) bad <- c(bad, "What.Label")
            }
            
            if (length(bad)) {
                tests <- c(tests, "TEST3")
                dets  <- c(dets, paste("TEST3:", paste(unique(bad), collapse = " | ")))
            }
        }
        
        out_tests[[i]]  <- unique(tests)
        out_detail[[i]] <- unique(dets)
    }
    
    keep <- vapply(out_tests, length, integer(1)) > 0
    vio <- data.frame(
        AtomID = ids[keep],
        TEST   = vapply(out_tests[keep], function(x) paste(x, collapse = " | "), ""),
        detail = vapply(out_detail[keep], function(x) paste(x, collapse = " ; "), ""),
        stringsAsFactors = FALSE
    )
    
    list(violations = vio, total_atoms = n)
}

# Interoperability: labels must exist in a registry (simple)
validator_label_integrity <- function(atoms, registry_map, verbose = FALSE) {
    # registry_map: named character vector or list: names = model strings, values = file path/URL
    
    allow_by_model <- lapply(names(registry_map), function(m) read_registry_labels(registry_map[[m]]))
    names(allow_by_model) <- names(registry_map)
    
    out <- data.frame(
        AtomID = character(0),
        model  = character(0),
        label  = character(0),
        detail = character(0),
        stringsAsFactors = FALSE
    )
    
    for (i in seq_along(atoms)) {
        a <- atoms[[i]]
        id <- get_atom_id(a) %||% paste0("index_", i)
        
        mods <- get_models_vec(a)                                     # PascalCase preserved
        labs <- get_labels_vec(a)
        
        if (!length(mods) || !length(labs)) {
            out <- rbind(out, data.frame(AtomID=id, model=NA_character_, label=NA_character_,
                                         detail="Missing How.ClassificationModel or What.Label", stringsAsFactors=FALSE))
            next
        }
        
        k <- min(length(mods), length(labs))                          # pair by index
        if (length(mods) != length(labs)) {
            out <- rbind(out, data.frame(AtomID=id, model=NA_character_, label=NA_character_,
                                         detail=paste0("Model/Label length mismatch: models=", length(mods), ", labels=", length(labs)),
                                         stringsAsFactors=FALSE))
        }
        
        for (j in seq_len(k)) {
            m <- mods[j]; lab <- labs[j]
            
            if (!nzchar(m) || !nzchar(lab)) next
            
            if (is.null(allow_by_model[[m]])) {
                out <- rbind(out, data.frame(AtomID=id, model=m, label=lab,
                                             detail="No registry provided for this model", stringsAsFactors=FALSE))
                next
            }
            
            if (!(lab %in% allow_by_model[[m]])) {
                out <- rbind(out, data.frame(AtomID=id, model=m, label=lab,
                                             detail="Label not found in registry for model", stringsAsFactors=FALSE))
            }
        }
    }
    
    list(violations = out, models = names(allow_by_model))
}

# Duplicates: full Atom-to-Atom match (canonical JSON signature)
validator_duplicates_full <- function(atoms) {
    sig <- vapply(atoms, sig_full, "")
    ok <- nzchar(sig) & !is.na(sig)
    dup <- rep(FALSE, length(sig))
    dup[ok] <- duplicated(sig[ok]) | duplicated(sig[ok], fromLast = TRUE)
    
    out <- data.frame(
        AtomID = character(0),
        detail = character(0),
        stringsAsFactors = FALSE
    )
    
    if (any(dup)) {
        idx <- which(dup)
        out <- data.frame(
            AtomID = vapply(atoms[idx], get_atom_id, ""),
            detail = "Full content duplicate (canonical JSON match)",
            stringsAsFactors = FALSE
        )
        out$AtomID[!nzchar(out$AtomID)] <- paste0("index_", idx[!nzchar(out$AtomID)])
    }
    
    list(duplicates = out, duplicate_n = sum(dup), total_atoms = length(atoms))
}

# Temporal: gaps and overlaps within each participant stream
# - gap: next_start - prev_end > threshold_sec
# - overlap: next_start < prev_end
validator_temporal_simple <- function(atoms, threshold_sec = 1800) {
    
    pid <- vapply(atoms, get_pid, "")
    t0  <- vapply(atoms, get_t0, NA_real_)
    dur <- vapply(atoms, get_dur, NA_real_)
    t1  <- t0 + dur
    
    ok <- nzchar(pid) & is.finite(t0) & is.finite(dur) & dur >= 0 & is.finite(t1)
    idx_by_pid <- split(which(ok), pid[ok])
    
    out_gap <- data.frame(participant_id=character(0), prev_AtomID=character(0), next_AtomID=character(0),
                          gap_sec=numeric(0), stringsAsFactors=FALSE)
    out_ovl <- data.frame(participant_id=character(0), prev_AtomID=character(0), next_AtomID=character(0),
                          overlap_sec=numeric(0), stringsAsFactors=FALSE)
    
    for (p in names(idx_by_pid)) {
        ix <- idx_by_pid[[p]]
        ord <- ix[order(t0[ix])]
        if (length(ord) < 2) next
        
        prv <- ord[-length(ord)]
        nxt <- ord[-1]
        
        gap <- t0[nxt] - t1[prv]
        hit_gap <- which(gap > threshold_sec)
        if (length(hit_gap)) {
            out_gap <- rbind(out_gap, data.frame(
                participant_id = p,
                prev_AtomID = vapply(atoms[prv[hit_gap]], get_atom_id, ""),
                next_AtomID = vapply(atoms[nxt[hit_gap]], get_atom_id, ""),
                gap_sec = gap[hit_gap],
                stringsAsFactors = FALSE
            ))
        }
        
        ovl <- t1[prv] - t0[nxt]
        hit_ovl <- which(ovl > 0)
        if (length(hit_ovl)) {
            out_ovl <- rbind(out_ovl, data.frame(
                participant_id = p,
                prev_AtomID = vapply(atoms[prv[hit_ovl]], get_atom_id, ""),
                next_AtomID = vapply(atoms[nxt[hit_ovl]], get_atom_id, ""),
                overlap_sec = ovl[hit_ovl],
                stringsAsFactors = FALSE
            ))
        }
    }
    
    list(gaps = out_gap, overlaps = out_ovl, threshold_sec = threshold_sec)
}

# ----------------------------
# CQ6 + CQ7 (minimal, reusing your style)
# ----------------------------
participant_slice <- function(atoms, participant_id) {
    pid  <- vapply(atoms, get_pid, "")
    keep <- which(pid == participant_id)
    if (!length(keep)) return(list(indices = integer(0), atoms = list(), coverage = data.frame()))
    t0  <- vapply(atoms[keep], get_t0, 0)
    dur <- vapply(atoms[keep], get_dur, 0)
    t1  <- t0 + dur
    ord <- order(t0)
    keep <- keep[ord]
    data_cov <- data.frame(
        participant_id = participant_id,
        n_atoms        = length(keep),
        start_utc      = min(t0, na.rm = TRUE),
        end_utc        = max(t1, na.rm = TRUE),
        span_sec       = max(t1, na.rm = TRUE) - min(t0, na.rm = TRUE),
        span_hours     = (max(t1, na.rm = TRUE) - min(t0, na.rm = TRUE)) / 3600,
        span_days      = (max(t1, na.rm = TRUE) - min(t0, na.rm = TRUE)) / 86400,
        stringsAsFactors = FALSE
    )
    list(indices = keep, atoms = atoms[keep], coverage = data_cov)
}

retriever <- function(atoms, participant_id, window_start_utc, window_end_utc, tz = "UTC") {
    pid <- vapply(atoms, get_pid, "")
    t0  <- vapply(atoms, get_t0, 0)
    dur <- vapply(atoms, get_dur, 0)
    t1  <- t0 + dur
    hit <- (pid == participant_id) & (t0 < window_end_utc) & (t1 > window_start_utc)
    idx <- which(hit)
    
    tab <- data.frame(
        index        = idx,
        AtomID       = vapply(atoms[idx], get_atom_id, ""),
        start_utc    = t0[idx],
        end_utc      = t1[idx],
        duration_sec = dur[idx],
        label        = vapply(atoms[idx], get_label, ""),
        evidence     = vapply(atoms[idx], get_ev, ""),
        model        = vapply(atoms[idx], get_md, ""),
        stringsAsFactors = FALSE
    )
    if (nrow(tab)) tab <- tab[order(tab$start_utc), , drop = FALSE]
    
    plot_fn <- if (nrow(tab)) {
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
            
            mode <- if (any(lab_present %in% bout_keys)) "bout" else if (any(lab_present %in% rest_keys)) "rest" else "other"
            lev0 <- if (mode == "bout") bout_bottom_to_top else if (mode == "rest") rest_bottom_to_top else character(0)
            
            lev  <- c(lev0[lev0 %in% lab_present], setdiff(lab_present, lev0))                    # preserve order + append unknowns
            y0   <- match(lab_raw, lev)
            
            set.seed(1)
            y <- y0 + runif(length(y0), -0.12, 0.12)
            
            lev_disp <- lev
            
            op <- par(mar = c(8, 10, 2, 1) + 0.1)  # set once (bigger bottom margin)
            plot(range(c(x0, x1)), c(0.5, length(lev) + 0.5), type = "n",
                 xlab = "", ylab = "", yaxt = "n", xaxt = "n")
            axis(2, at = seq_along(lev), labels = lev_disp, las = 2, cex.axis = 0.8)
            mtext("Label", side = 2, line = 5)
            
            ws <- as.POSIXct(window_start_utc, origin = "1970-01-01", tz = tz)
            we <- as.POSIXct(window_end_utc,   origin = "1970-01-01", tz = tz)
            span <- as.numeric(difftime(we, ws, units = "secs"))
            by  <- if (span <= 36 * 3600) "6 hours" else "1 day"
            fmt <- if (span <= 36 * 3600) "%d %b %Y %H:%M" else "%d %b %Y"
            ticks <- seq(ws, we, by = by)
            if (length(ticks) < 2) ticks <- c(ws, we)
            axis.POSIXct(1, at = ticks, format = fmt, las = 2, cex.axis = 0.85)
            mtext(if (span <= 36 * 3600) "Date and time (UTC)" else "Date (UTC)", side = 1, line = 5)
            
            abline(v = c(ws, we), lty = 2)
            segments(x0, y, x1, y, lwd = 3)
            par(op)
        }
    } else NULL
    
    list(indices = idx, atoms = atoms[idx], table = tab, plot_fn = plot_fn)
}

summariser <- function(atoms, participant_id, per_day = TRUE, day_cut_hour = 3) {
    pid  <- vapply(atoms, get_pid, "")
    keep <- which(pid == participant_id)
    if (!length(keep)) return(list(summary = data.frame(), plot_fn = NULL))
    
    lb  <- vapply(atoms[keep], get_label, "")
    dur <- vapply(atoms[keep], get_dur, 0)
    t0  <- vapply(atoms[keep], get_t0, 0)
    off <- vapply(atoms[keep], get_off, 0)
    
    cut_sec <- as.numeric(day_cut_hour) * 3600
    day <- as.Date(as.POSIXct(t0 + off - cut_sec, origin = "1970-01-01", tz = "UTC"))
    
    df <- data.frame(label = lb, duration_sec = dur, day = day, stringsAsFactors = FALSE)
    df <- df[nzchar(df$label) & is.finite(df$duration_sec) & df$duration_sec >= 0, , drop = FALSE]
    
    agg <- aggregate(duration_sec ~ day + label, df, sum)
    agg$duration_hrs <- agg$duration_sec / 3600
    agg <- agg[order(agg$day, -agg$duration_sec), , drop = FALSE]
    
    plot_fn <- if (nrow(agg)) {
        a <- agg
        function() {
            mat <- xtabs(duration_hrs ~ label + day, a)
            
            labs0 <- rownames(mat)
            
            # Desired TOP -> BOTTOM orders
            order_bout_top_to_bottom <- c("Run","FastWalk","SlowWalk","Active","Sedentary","Sleep","NonWear")
            order_rest_top_to_bottom <- c("Wake","Rest","NonWear")
            
            # Convert to BOTTOM -> TOP (because barplot stacks bottom first)
            order_bout_bottom_to_top <- rev(order_bout_top_to_bottom)
            order_rest_bottom_to_top <- rev(order_rest_top_to_bottom)
            
            # Decide which stream ordering to apply (if labels look like bout, use bout order; else rest order)
            is_bout <- any(labs0 %in% order_bout_top_to_bottom)
            is_rest <- any(labs0 %in% order_rest_top_to_bottom)
            
            ord_bottom_to_top <- if (is_bout) {
                order_bout_bottom_to_top
            } else if (is_rest) {
                order_rest_bottom_to_top
            } else {
                character(0)
            }
            
            # Apply ordering: known labels first (in specified order), then any unknown labels appended
            labs_ord <- c(ord_bottom_to_top[ord_bottom_to_top %in% labs0], setdiff(labs0, ord_bottom_to_top))
            
            mat <- mat[labs_ord, , drop = FALSE]
            
            d <- as.Date(colnames(mat))
            o <- order(d)
            mat <- mat[, o, drop = FALSE]
            colnames(mat) <- format(d[o], "%d %b %Y")
            
            labs <- rownames(mat)
            cols <- setNames(rep(okabe_ito, length.out = length(labs)), labs)
            
            op <- par(mar = c(10, 5, 3, 14) + 0.1, xpd = TRUE)  # was c(10,4,3,10)
            max_y <- max(colSums(mat), na.rm = TRUE)
            yt <- pretty(c(0, max_y * 1.05))
            
            barplot(mat, beside = FALSE, col = cols[labs], border = NA, las = 2, cex.names = 0.8,
                    ylab = "Duration (hours)", ylim = c(0, max(yt)), yaxt = "n",
                    main = "Per-day summary (stacked)")
            
            axis(2, at = yt, las = 1)
            mtext("Date", side = 1, line = 5)
            
            legend_labs <- rev(labs)          # top -> bottom
            legend_cols <- rev(cols[labs])
            legend("topright", inset = c(-0.15, 0), bty = "n", cex = 1.0,
                   fill = legend_cols, legend = legend_labs)
            par(op)
        }
    } else NULL
    
    list(summary = agg, plot_fn = plot_fn)
}

# ----------------------------
# Summary for payload
# ----------------------------
payload_summary <- function(atoms) {
    n <- length(atoms)
    pid <- vapply(atoms, get_pid, "")
    t0  <- vapply(atoms, get_t0, NA_real_)
    dur <- vapply(atoms, get_dur, NA_real_)
    t1  <- t0 + dur
    lb  <- vapply(atoms, get_label, "")
    ev  <- vapply(atoms, get_ev, "")
    md_all <- unlist(lapply(atoms, get_models_vec), use.names = FALSE)
    
    pid_u <- sort(unique(pid[nzchar(pid)]))
    sum0 <- data.frame(
        metric = c("n_atoms","n_participants","start_utc_min","end_utc_max","span_hours"),
        value  = c(
            n,
            length(pid_u),
            ifelse(all(!is.finite(t0)), NA, min(t0, na.rm = TRUE)),
            ifelse(all(!is.finite(t1)), NA, max(t1, na.rm = TRUE)),
            ifelse(all(!is.finite(t0)) || all(!is.finite(t1)), NA, (max(t1, na.rm = TRUE) - min(t0, na.rm = TRUE)) / 3600)
        ),
        stringsAsFactors = FALSE
    )
    
    top_lab <- sort(table(lb[nzchar(lb)]), decreasing = TRUE)
    top_ev  <- sort(table(ev[nzchar(ev)]), decreasing = TRUE)
    top_md <- sort(table(md_all[nzchar(md_all)]), decreasing = TRUE)
    
    list(
        overview = sum0,
        participants = data.frame(participant_id = pid_u, stringsAsFactors = FALSE),
        top_labels = head(data.frame(label = names(top_lab), n = as.integer(top_lab), stringsAsFactors = FALSE), 30),
        top_evidence = head(data.frame(evidence = names(top_ev), n = as.integer(top_ev), stringsAsFactors = FALSE), 30),
        top_models = head(data.frame(model = names(top_md), n = as.integer(top_md), stringsAsFactors = FALSE), 30)
    )
}

# ----------------------------
# JSON-LD projection
# ----------------------------
payload_to_jsonld <- function(payload_atoms, context_obj) {
    atoms <- payload_atoms
    atoms <- lapply(atoms, function(a) {
        if (!is_list(a) || !is_list(a$Header)) return(a)                               # skip invalid atoms
        aid <- a$Header$AtomID %||% NA_character_
        if (is.list(aid)) aid <- unlist(aid, use.names = FALSE)
        aid <- as.character(aid)[1]
        if (!is.na(aid) && nzchar(aid)) a[["@id"]] <- mint_atom_urn(aid)
        a
    })
    list(`@context` = context_obj, `@graph` = atoms)
}

# ----------------------------
# UI helpers
# ----------------------------
placeholder_box <- function(title = NULL, text, type = c("info","success")) {
    type <- match.arg(type)
    div(
        class = paste("coel-placeholder", paste0("coel-", type)),
        if (!is.null(title)) tags$div(class = "coel-placeholder-title", title),
        tags$div(class = "coel-placeholder-text", text)
    )
}

panel_block <- function(title, ..., help = NULL) {
    div(
        class = "coel-panel-block",
        tags$div(class = "coel-panel-title", title),
        if (!is.null(help)) tags$div(class = "coel-panel-help", help),
        ...
    )
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            body { background-color: #f6f8fb; }
            .container-fluid { max-width: 1700px; }
            .coel-app-title { margin-bottom: 16px; }
            .coel-app-subtitle {
                color: #5f6b7a; margin-top: -6px; margin-bottom: 18px;
            }
            .well, .sidebarPanel {
                background: #ffffff;
                border: 1px solid #e2e8f0;
                border-radius: 14px;
                padding: 18px 18px 14px 18px;
                box-shadow: 0 2px 10px rgba(15, 23, 42, 0.05);
            }
            .coel-panel-block {
                background: #fbfcfe;
                border: 1px solid #e5eaf1;
                border-radius: 12px;
                padding: 14px 14px 10px 14px;
                margin-bottom: 14px;
            }
            .coel-panel-title {
                font-weight: 600;
                font-size: 15px;
                margin-bottom: 8px;
                color: #1f2937;
            }
            .coel-panel-help {
                font-size: 12px;
                color: #5f6b7a;
                margin-bottom: 10px;
                line-height: 1.45;
            }
            .coel-placeholder {
                border-radius: 12px;
                padding: 16px 18px;
                margin: 10px 0 14px 0;
                border: 1px solid #dbe4ee;
                background: #f8fafc;
            }
            .coel-info {
                background: #f8fafc;
                border-color: #dbe4ee;
            }
            .coel-success {
                background: #f0fdf4;
                border-color: #bbf7d0;
            }
            .coel-placeholder-title {
                font-weight: 600;
                margin-bottom: 4px;
                color: #1f2937;
            }
            .coel-placeholder-text {
                color: #4b5563;
                line-height: 1.45;
            }
            .nav-tabs { margin-bottom: 16px; }
            .tab-content {
                background: #ffffff;
                border: 1px solid #e2e8f0;
                border-radius: 14px;
                padding: 18px;
                box-shadow: 0 2px 10px rgba(15, 23, 42, 0.05);
            }
            .form-group { margin-bottom: 10px; }
            .btn { border-radius: 10px; }
            .btn-default, .btn-primary {
                padding: 8px 12px;
            }
            .shiny-download-link {
                display: inline-block;
                margin-right: 8px;
                margin-top: 6px;
                margin-bottom: 0px;
            }
            .dataTables_wrapper { margin-top: 6px; margin-bottom: 18px; }
            h4 { margin-top: 6px; margin-bottom: 10px; }
            hr { margin-top: 14px; margin-bottom: 14px; }
            
            .coel-app-subtitle {
                color: #5f6b7a; margin-top: -2px; margin-bottom: 8px;
            }
            .coel-app-meta {
                color: #4b5563;
                font-size: 13px;
                margin-bottom: 4px;
            }
            .coel-app-links {
                font-size: 13px;
                margin-bottom: 14px;
            }
            .coel-app-links a, .coel-app-meta a {
                text-decoration: none;
            }
            .coel-panel-block .form-group:last-child {
                margin-bottom: 4px;
            }
            .coel-tight-note {
                font-size: 12px;
                color: #5f6b7a;
                line-height: 1.45;
                margin-top: -4px;
                margin-bottom: 8px;
            }
            .coel-tight-gap {
                margin-top: 6px;
            }
        "))
    ),
    
    div(class = "coel-app-title",
        tags$h2("COEL Web Application"),
        div(
            class = "coel-app-subtitle",
            "Validation, integrity review, temporal scoping, summary inspection, and JSON-LD projection for COEL Behavioural Atoms."
        ),
        div(
            class = "coel-app-meta",
            HTML('Author and Maintainer: Millen J. Theophilus (<a href="https://github.com/miltheo" target="_blank">github: miltheo</a>)')
        ),
        div(
            class = "coel-app-links",
            HTML(paste0(
                '<a href="https://github.com/miltheo/coel" target="_blank">GitHub repository</a>',
                ' &nbsp;|&nbsp; ',
                '<a href="https://w3id.org/coel/atom/2.0/specification.pdf" target="_blank">COEL Behavioural Atom v2.0 specification</a>',
                ' &nbsp;|&nbsp; ',
                '<a href="#" target="_blank">Zenodo release DOI (placeholder)</a>'
            ))
        )
    ),
    
    sidebarLayout(
        sidebarPanel(
            panel_block(
                "Payload",
                fileInput("payload", "Upload Atom payload (.json or .json.gz)", accept = c(".json",".gz"))
            ),
            
            panel_block(
                "Structural validation",
                actionButton("run_val", "Run structural validator (TEST1-TEST3)"),
                help = tagList(
                    tags$b("TEST1 (required elements): "), "Each Atom MUST include Header, When, What, and Who.",
                    tags$br(),
                    tags$b("TEST2 (required sub-elements): "), "Each Atom MUST include Header.AtomVersion; When.TimeUTC, When.Duration, When.UTCOffset; What.Label; and Who.ParticipantID or Who.EnvironmentID.",
                    tags$br(),
                    tags$b("TEST3 (field type checks): "), "Each required element MUST be an object and each required sub-element MUST have the expected type."
                )
            ),
            
            panel_block(
                "Interoperability validation",
                uiOutput("reg_model_ui"),
                actionButton("run_label", "Run label integrity"),
                help = "Checks each What.Label against the registry for its corresponding How.ClassificationModel, paired by index."
            ),
            
            panel_block(
                "Duplicate validation",
                actionButton("run_dup", "Find duplicates (full Atom match)"),
                help = "Detects duplicates using a canonical JSON signature of each Atom."
            ),
            
            panel_block(
                "Temporal validation",
                numericInput("gap_thr", "Gap threshold (minutes)", value = 30, min = 1, step = 1),
                actionButton("run_temp", "Scan gaps and overlaps"),
                help = "Flags gaps where next start minus previous end exceeds threshold, and overlaps where next start occurs before previous end."
            ),
            
            panel_block(
                "Temporal scoping",
                uiOutput("pid_ui"),
                uiOutput("w_start_ui"),
                div(class = "coel-tight-note", "Enter UTC time as YYYY-MM-DD HH:MM:SS. Default is the participant start time."),
                fluidRow(
                    column(6, numericInput("w_dur_val", "Window duration", value = 24, min = 0, step = 1)),
                    column(6, selectInput("w_dur_unit", "Unit", choices = c("Hours"="hours","Days"="days"), selected = "hours"))
                ),
                div(class = "coel-tight-note", "Tip: set duration to 0 to scope the full participant span."),
                div(class = "coel-tight-gap", actionButton("run_cq6", "Run temporal scoping")),
                downloadButton("dl_ts_atoms", "Download retrieved Atoms (JSON)"),
                downloadButton("dl_ts_table", "Download table (CSV)"),
                downloadButton("dl_ts_plot",  "Download plot (PNG)")
            ),
            
            panel_block(
                "Per-day summary scoping",
                numericInput("day_cut", "Day cut hour (local)", value = 3, min = 0, max = 23, step = 1),
                div(
                    class = "coel-tight-note",
                    "Defines the local day boundary used to assign Atoms to dates. For example, 3 means each day runs from 03:00 to 02:59 local time using TimeUTC + UTCOffset."
                ),
                div(class = "coel-tight-gap", actionButton("run_cq7", "Run per-day summary scoping")),
                downloadButton("dl_pd_table", "Download table (CSV)"),
                downloadButton("dl_pd_plot",  "Download plot (PNG)")
            ),
            
            panel_block(
                "JSON-LD projection",
                fileInput("context", "Optional context.jsonld", accept = c(".json",".jsonld")),
                div(class = "coel-tight-gap", actionButton("make_jsonld", "Create JSON-LD")),
                downloadButton("dl_jsonld", "Download JSON-LD")
            ),
            
            panel_block(
                "Downloads",
                downloadButton("dl_violations", "Download violations (CSV)"),
                tags$br(),
                downloadButton("dl_summary", "Download payload overview (CSV)")
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Overview",
                         uiOutput("overview_ui")
                ),
                tabPanel("Validation",
                         uiOutput("validation_ui")
                ),
                tabPanel("Integrity",
                         uiOutput("integrity_ui")
                ),
                tabPanel("Temporal scoping",
                         uiOutput("cq6_ui")
                ),
                tabPanel("Per-day summary scoping",
                         uiOutput("cq7_ui")
                )
            )
        )
    )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
    
    atoms_r <- reactiveVal(NULL)
    summary_r <- reactiveVal(NULL)
    violations_r <- reactiveVal(data.frame())
    cq6_r <- reactiveVal(NULL)
    cq7_r <- reactiveVal(NULL)
    jsonld_r <- reactiveVal(NULL)
    label_vio_r <- reactiveVal(data.frame())
    dup_r       <- reactiveVal(data.frame())
    gap_r       <- reactiveVal(data.frame())
    ovl_r       <- reactiveVal(data.frame())
    models_r <- reactiveVal(character(0))
    
    observeEvent(input$payload, {
        req(input$payload$datapath)
        withProgress(message = "Loading payload", value = 0, {
            incProgress(0.2, detail = "Reading file")
            atoms <- read_json_any(input$payload$datapath)
            incProgress(0.6, detail = "Parsing JSON")
            if (!is.list(atoms)) stop("Payload must be a JSON array (list of Atoms).")
            atoms_r(atoms)
            md_all <- unique(unlist(lapply(atoms, get_models_vec), use.names = FALSE))
            md_all <- md_all[nzchar(md_all)]
            models_r(sort(md_all))
            incProgress(0.9, detail = "Computing overview")
            summary_r(payload_summary(atoms))
            violations_r(data.frame())
            cq6_r(NULL)
            cq7_r(NULL)
            jsonld_r(NULL)
            label_vio_r(data.frame())
            dup_r(data.frame())
            gap_r(data.frame())
            ovl_r(data.frame())
            incProgress(1, detail = "Done")
        })
    })
    
    output$reg_model_ui <- renderUI({
        mods <- models_r()
        if (!length(mods)) return(tags$small("No classification models detected yet."))
        
        tagList(
            tags$small("Provide one registry (CSV path or GitHub URL) for each detected model:"),
            lapply(mods, function(m) {
                textInput(paste0("reg__", m), label = paste0(m, " registry"), value = "")
            })
        )
    })
    
    output$pid_ui <- renderUI({
        atoms <- atoms_r()
        if (is.null(atoms)) {
            return(selectInput("pid", "Participant", choices = character(0), selected = NULL))
        }
        pid_u <- sort(unique(vapply(atoms, get_pid, "")))
        pid_u <- pid_u[nzchar(pid_u)]
        selectInput("pid", "Participant", choices = pid_u, selected = pid_u[1] %||% "")
    })
    
    output$w_start_ui <- renderUI({
        atoms <- atoms_r()
        if (is.null(atoms) || !nzchar(input$pid %||% "")) {
            return(textInput("w_start_utc", "Window start UTC", value = "", placeholder = "YYYY-MM-DD HH:MM:SS"))
        }
        
        ps <- participant_slice(atoms, input$pid)
        if (!nrow(ps$coverage)) {
            return(textInput("w_start_utc", "Window start UTC", value = "", placeholder = "YYYY-MM-DD HH:MM:SS"))
        }
        
        start_txt <- format(
            as.POSIXct(ps$coverage$start_utc[1], origin = "1970-01-01", tz = "UTC"),
            "%Y-%m-%d %H:%M:%S"
        )
        
        textInput(
            "w_start_utc",
            "Window start UTC",
            value = start_txt,
            placeholder = "YYYY-MM-DD HH:MM:SS"
        )
    })
    
    # Overview tables
    output$overview_ui <- renderUI({
        s <- summary_r()
        if (is.null(s)) {
            return(placeholder_box(
                title = "No payload loaded",
                text = "Upload a COEL Atom payload to view the payload overview, participants, labels, evidence types, and classification models.",
                type = "info"
            ))
        }
        
        tagList(
            h4("Payload overview"),
            DTOutput("ov_tbl"),
            h4("Participants"),
            DTOutput("pid_tbl"),
            h4("Top labels"),
            DTOutput("lab_tbl"),
            h4("Top evidence"),
            DTOutput("ev_tbl"),
            h4("Top models"),
            DTOutput("md_tbl")
        )
    })
    
    output$ov_tbl <- renderDT({
        s <- summary_r(); req(s)
        datatable(s$overview, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
    })
    output$pid_tbl <- renderDT({
        s <- summary_r(); req(s)
        datatable(s$participants, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
    })
    output$lab_tbl <- renderDT({
        s <- summary_r(); req(s)
        datatable(s$top_labels, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
    })
    output$ev_tbl <- renderDT({
        s <- summary_r(); req(s)
        datatable(s$top_evidence, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
    })
    output$md_tbl <- renderDT({
        s <- summary_r(); req(s)
        datatable(s$top_models, options = list(pageLength = 10, dom = "tip"), rownames = FALSE)
    })
    
    # Validation
    observeEvent(input$run_val, {
        atoms <- atoms_r()
        req(atoms)
        withProgress(message = "Validating structure", value = 0, {
            incProgress(0.2, detail = "Running TEST1-TEST3")
            val <- validator_structural(atoms, run_TEST1 = TRUE, run_TEST2 = TRUE, run_TEST3 = TRUE)
            incProgress(0.9, detail = "Preparing results")
            violations_r(val$violations)
            incProgress(1, detail = "Done")
        })
    })
    
    output$validation_ui <- renderUI({
        if (is.null(atoms_r())) {
            return(placeholder_box(
                title = "Structural validation not yet run",
                text = "Upload a payload and run the structural validator to view any TEST1 to TEST3 findings.",
                type = "info"
            ))
        }
        
        if (isTRUE(input$run_val < 1)) {
            return(placeholder_box(
                title = "Structural validation not yet run",
                text = "Run the structural validator to display structural findings for the uploaded payload.",
                type = "info"
            ))
        }
        
        if (!nrow(violations_r())) {
            return(placeholder_box(
                title = "Routine quality assurance passed",
                text = "No structural violations were detected. The uploaded Atom payload clears routine structural quality assurance under TEST1 to TEST3.",
                type = "success"
            ))
        }
        
        tagList(
            h4("Structural violations (TEST1-TEST3)"),
            DTOutput("vio_tbl")
        )
    })
    
    output$vio_tbl <- renderDT({
        datatable(violations_r(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    })
    
    # Integrity

    observeEvent(input$run_label, {
         atoms <- atoms_r()
         mods <- models_r()
         req(atoms, length(mods) > 0)
         
         reg_vals <- vapply(mods, function(m) input[[paste0("reg__", m)]] %||% "", character(1))
         reg_vals <- reg_vals[nzchar(reg_vals)]
         
         if (!length(reg_vals)) stop("Please provide at least one registry path/URL.")
         
         withProgress(message = "Running label integrity", value = 0, {
             incProgress(0.2, detail = "Loading registries")
             res <- validator_label_integrity(atoms, registry_map = reg_vals)
             incProgress(0.9, detail = "Preparing results")
             label_vio_r(res$violations)
             incProgress(1, detail = "Done")
         })
     })

    observeEvent(input$run_dup, {
         atoms <- atoms_r()
         req(atoms)
         withProgress(message = "Finding duplicates", value = 0, {
             incProgress(0.3, detail = "Computing signatures")
             res <- validator_duplicates_full(atoms)
             incProgress(0.9, detail = "Preparing results")
             dup_r(res$duplicates)
             incProgress(1, detail = "Done")
         })
     })

    observeEvent(input$run_temp, {
         atoms <- atoms_r()
         req(atoms)
         thr <- as.numeric(input$gap_thr) * 60
         withProgress(message = "Scanning temporal consistency", value = 0, {
             incProgress(0.3, detail = "Grouping by participant")
             res <- validator_temporal_simple(atoms, threshold_sec = thr)
             incProgress(0.9, detail = "Preparing results")
             gap_r(res$gaps)
             ovl_r(res$overlaps)
             incProgress(1, detail = "Done")
         })
     })
    
    output$integrity_ui <- renderUI({
        if (is.null(atoms_r())) {
            return(placeholder_box(
                title = "Integrity checks not yet run",
                text = "Upload a payload and run one or more integrity checks to view label, duplicate, or temporal findings.",
                type = "info"
            ))
        }
        
        ran_any <- isTRUE(input$run_label >= 1) || isTRUE(input$run_dup >= 1) || isTRUE(input$run_temp >= 1)
        if (!ran_any) {
            return(placeholder_box(
                title = "Integrity checks not yet run",
                text = "Run label integrity, duplicate detection, or temporal validation to display integrity findings.",
                type = "info"
            ))
        }
        
        label_clear <- isTRUE(input$run_label >= 1) && !nrow(label_vio_r())
        dup_clear   <- isTRUE(input$run_dup   >= 1) && !nrow(dup_r())
        temp_clear  <- isTRUE(input$run_temp  >= 1) && !nrow(gap_r()) && !nrow(ovl_r())
        
        any_rows <- nrow(label_vio_r()) || nrow(dup_r()) || nrow(gap_r()) || nrow(ovl_r())
        
        tagList(
            if (!any_rows) placeholder_box(
                title = "Routine quality assurance passed",
                text = "No integrity issues were detected in the checks that were run. The uploaded Atom payload clears routine integrity quality assurance for those checks.",
                type = "success"
            ),
            
            if (isTRUE(input$run_label >= 1)) {
                if (nrow(label_vio_r())) {
                    tagList(h4("Label integrity"), DTOutput("lab_vio_tbl"))
                } else {
                    placeholder_box("Label integrity", "No label integrity issues detected.", "success")
                }
            },
            
            if (isTRUE(input$run_dup >= 1)) {
                if (nrow(dup_r())) {
                    tagList(h4("Duplicates"), DTOutput("dup_tbl"))
                } else {
                    placeholder_box("Duplicates", "No full-content duplicate Atoms detected.", "success")
                }
            },
            
            if (isTRUE(input$run_temp >= 1)) {
                if (nrow(gap_r())) {
                    tagList(h4("Temporal gaps"), DTOutput("gap_tbl"))
                } else {
                    placeholder_box("Temporal gaps", "No temporal gaps detected above the selected threshold.", "success")
                }
            },
            
            if (isTRUE(input$run_temp >= 1)) {
                if (nrow(ovl_r())) {
                    tagList(h4("Temporal overlaps"), DTOutput("ovl_tbl"))
                } else {
                    placeholder_box("Temporal overlaps", "No temporal overlaps detected.", "success")
                }
            }
        )
    })
    
    output$lab_vio_tbl <- renderDT({
        datatable(label_vio_r(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    })
    output$dup_tbl <- renderDT({
        datatable(dup_r(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    })
    output$gap_tbl <- renderDT({
        datatable(gap_r(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    })
    output$ovl_tbl <- renderDT({
        datatable(ovl_r(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    })
    
    # CQ6
    observeEvent(input$run_cq6, {
        atoms <- atoms_r()
        req(atoms, input$pid, input$w_start_utc, input$w_dur_val, input$w_dur_unit)
        withProgress(message = "Running temporal scoping", value = 0, {
            incProgress(0.2, detail = "Computing window")
            ws <- parse_utc(trimws(input$w_start_utc))
            dv <- suppressWarnings(as.numeric(input$w_dur_val))
            unit <- input$w_dur_unit %||% "hours"
            
            if (!is.finite(ws)) {
                showNotification("Temporal scoping: invalid window start. Use YYYY-MM-DD HH:MM:SS in UTC.", type = "error")
                return(NULL)
            }
            
            ps <- participant_slice(atoms, input$pid)
            if (!nrow(ps$coverage)) {
                showNotification("Temporal scoping: no Atoms found for this participant.", type = "error")
                return(NULL)
            }
            
            start_all <- ps$coverage$start_utc[1]
            end_all   <- ps$coverage$end_utc[1]
            
            mult <- if (identical(unit, "days")) 86400 else 3600
            span_sec <- end_all - ws
            if (!is.finite(span_sec) || span_sec <= 0) span_sec <- end_all - start_all
            
            # duration <= 0 means "full span"
            if (!is.finite(dv) || dv <= 0) {
                we <- end_all
            } else {
                we <- ws + dv * mult
                if (!is.finite(we) || we > end_all) we <- end_all
            }
            incProgress(0.6, detail = "Retrieving Atoms")
            cq6_r(retriever(atoms, input$pid, ws, we, tz = "UTC"))
            incProgress(1, detail = "Done")
        })
    })
    
    output$cq6_ui <- renderUI({
        cq6 <- cq6_r()
        if (is.null(atoms_r())) {
            return(placeholder_box(
                title = "Temporal scoping not yet run",
                text = "Upload a payload, choose a participant and window, then run temporal scoping to display the retrieved Atoms and timeline plot.",
                type = "info"
            ))
        }
        
        if (is.null(cq6)) {
            return(placeholder_box(
                title = "Temporal scoping not yet run",
                text = "Set the participant and time window, then run temporal scoping to view the scoped Atom table and plot.",
                type = "info"
            ))
        }
        
        if (!nrow(cq6$table)) {
            return(placeholder_box(
                title = "No Atoms retrieved",
                text = "The selected time window did not retrieve any Atoms for this participant.",
                type = "info"
            ))
        }
        
        tagList(
            h4("Temporal scoping table"),
            DTOutput("cq6_tbl"),
            h4("Temporal scoping plot"),
            plotOutput("temporal_scoping_plot", height = 350)
        )
    })
    
    output$cq6_tbl <- renderDT({
        cq6 <- cq6_r(); req(cq6)
        datatable(
            cq6$table,
            options = list(
                pageLength = 100,
                lengthMenu = list(c(25, 50, 100, 250, -1), c("25", "50", "100", "250", "All")),
                scrollX = TRUE
            ),
            rownames = FALSE
        )
    })
    
    output$temporal_scoping_plot <- renderPlot({
        cq6 <- cq6_r(); req(cq6, cq6$plot_fn)
        cq6$plot_fn()
    })
    
    output$dl_ts_atoms <- downloadHandler(
        filename = function() paste0("temporal_scoping_atoms_", input$pid, ".json"),
        content = function(file) {
            ts <- cq6_r(); req(ts)
            write_json_any(ts$atoms, file, gzip = FALSE)
        }
    )
    
    output$dl_ts_table <- downloadHandler(
        filename = function() paste0("temporal_scoping_table_", input$pid, ".csv"),
        content = function(file) {
            ts <- cq6_r(); req(ts)
            write.csv(ts$table, file, row.names = FALSE)
        }
    )
    
    output$dl_ts_plot <- downloadHandler(
        filename = function() paste0("temporal_scoping_plot_", input$pid, ".png"),
        content = function(file) {
            ts <- cq6_r(); req(ts, ts$plot_fn)
            save_plot_png(ts$plot_fn, file, w = 12, h = 4, res = 600)
        }
    )
    
    # CQ7
    observeEvent(input$run_cq7, {
        atoms <- atoms_r()
        req(atoms, input$pid)
        withProgress(message = "Running per-day summary scoping", value = 0, {
            incProgress(0.3, detail = "Aggregating by day")
            cq7_r(summariser(atoms, input$pid, per_day = TRUE, day_cut_hour = input$day_cut))
            incProgress(1, detail = "Done")
        })
    })
    
    output$cq7_ui <- renderUI({
        cq7 <- cq7_r()
        if (is.null(atoms_r())) {
            return(placeholder_box(
                title = "Per-day summary scoping not yet run",
                text = "Upload a payload and run per-day summary scoping to display daily duration summaries and the stacked plot.",
                type = "info"
            ))
        }
        
        if (is.null(cq7)) {
            return(placeholder_box(
                title = "Per-day summary scoping not yet run",
                text = "Run per-day summary scoping to generate the daily summary table and stacked plot.",
                type = "info"
            ))
        }
        
        if (!nrow(cq7$summary)) {
            return(placeholder_box(
                title = "No daily summary available",
                text = "No Atoms were available to generate a per-day summary for the selected participant.",
                type = "info"
            ))
        }
        
        tagList(
            h4("Per-day summary scoping table"),
            DTOutput("cq7_tbl"),
            h4("Per-day summary scoping plot"),
            plotOutput("summary_scoping_plot", height = 500)
        )
    })
    
    output$cq7_tbl <- renderDT({
        cq7 <- cq7_r(); req(cq7)
        datatable(cq7$summary, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
    })
    
    output$summary_scoping_plot <- renderPlot({
        cq7 <- cq7_r(); req(cq7, cq7$plot_fn)
        cq7$plot_fn()
    })
    
    output$dl_pd_table <- downloadHandler(
        filename = function() paste0("perday_summary_table_", input$pid, ".csv"),
        content = function(file) {
            pd <- cq7_r(); req(pd)
            write.csv(pd$summary, file, row.names = FALSE)
        }
    )
    
    output$dl_pd_plot <- downloadHandler(
        filename = function() paste0("perday_summary_plot_", input$pid, ".png"),
        content = function(file) {
            pd <- cq7_r(); req(pd, pd$plot_fn)
            save_plot_png(pd$plot_fn, file, w = 10, h = 8, res = 600)
        }
    )
    
    # JSON-LD
    observeEvent(input$make_jsonld, {
        atoms <- atoms_r()
        req(atoms)
        withProgress(message = "Creating JSON-LD", value = 0, {
            incProgress(0.2, detail = "Loading context")
            if (!is.null(input$context$datapath) && file.exists(input$context$datapath)) {
                ctx_doc <- read_json_any(input$context$datapath)
                ctx <- ctx_doc[["@context"]] %||% ctx_doc
            } else {
                default_ctx_path <- "https://raw.githubusercontent.com/miltheo/coel/main/utilities/jsonld/context.jsonld"
                ctx_doc <- read_json_any(to_src(default_ctx_path))
                ctx <- ctx_doc[["@context"]] %||% ctx_doc
            }
            incProgress(0.7, detail = "Projecting payload")
            jsonld_r(payload_to_jsonld(atoms, ctx))
            incProgress(1, detail = "Done")
        })
    })
    
    output$dl_jsonld <- downloadHandler(
        filename = function() paste0("coel_atoms_payload.jsonld"),
        content = function(file) {
            req(jsonld_r())
            write_json_any(jsonld_r(), file, gzip = FALSE)
        }
    )
    
    output$dl_violations <- downloadHandler(
        filename = function() "validator_structural_violations.csv",
        content = function(file) {
            write.csv(violations_r(), file, row.names = FALSE)
        }
    )
    
    output$dl_summary <- downloadHandler(
        filename = function() "payload_overview.csv",
        content = function(file) {
            s <- summary_r()
            req(s)
            write.csv(s$overview, file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)