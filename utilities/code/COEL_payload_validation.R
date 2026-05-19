# COEL Atom payload validation pipeline
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require(c("jsonlite", "charlatan"))

# ============================================================
# CONFIG (edit once)
# ============================================================

repo_root <- coel_repo_root()
stream    <- "rest_activity"                                                         # behavioural_bout | rest_activity

payload_json <- file.path(repo_root, "utilities", "data", "atoms_payload", stream, "coel_atoms_payload.json")  # baseline payload
out_root     <- file.path(repo_root, "utilities", "data", "research_paper_results", "validation", stream)   # outputs

registry_behaviour <- file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.csv") # registry
registry_rest      <- file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.csv")        # registry
registry_coel      <- file.path(repo_root, "models", "coel", "2.0", "coel-model-v2.0.csv")                                           # registry

pii_forename_csv <- "https://github.com/sigpwned/popular-names-by-country-dataset/blob/main/common-forenames-by-country.csv"          # pii
pii_surname_csv  <- "https://github.com/sigpwned/popular-names-by-country-dataset/blob/main/common-surnames-by-country.csv"           # pii

gap_threshold_sec <- 3600                                                             # CQ5 gap threshold
pct_min <- 10                                                                          # percent range min
pct_max <- 30                                                                          # percent range max

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)                          # mkdir

# ============================================================
# SEED REGISTRY (documented + reproducible)
# - One seed per CQ per stream per run_id
# - You can bump run_id when you want a new reproducible variant
# ============================================================

run_id <- 1                                                                           # bump to 2,3,... when desired

seed_from_key <- function(key) {                                                      # deterministic int seed from string
    x <- utf8ToInt(key)                                                                 # codepoints
    s <- sum(x * seq_along(x))                                                          # weighted sum
    as.integer((s %% 2147483646L) + 1L)                                                 # clamp to RNG range
}

seed_table <- function(stream, run_id) {                                              # seeds for documentation
    cqs <- c("CQ1","CQ2","CQ3","CQ4","CQ5")                                              # cqs
    data.frame(
        stream = stream,
        run_id = run_id,
        cq = cqs,
        seed = vapply(cqs, function(cq) seed_from_key(paste(stream, run_id, cq, sep = "|")), integer(1)),
        stringsAsFactors = FALSE
    )
}

seeds <- seed_table(stream, run_id)                                                   # build seeds
write.csv(seeds, file.path(out_root, "seed_registry.csv"), row.names = FALSE)         # write

# ============================================================
# IO HELPERS
# ============================================================

read_json_any <- function(path) {                                                     # read json or gz
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rb") else file(path, "rb")
    on.exit(close(con), add = TRUE)
    txt <- readLines(con, warn = FALSE, encoding = "UTF-8")
    jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
}

write_json_pretty <- function(x, path) {                                              # write pretty json
    txt <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, na = "null")
    writeLines(txt, path, useBytes = TRUE)
    invisible(path)
}

# ============================================================
# INJECTOR (cleaned RNG: separate draws for k and assignment)
# ============================================================

set_by_path <- function(x, p, f) {                                                    # set nested path by transformer
    if (length(p) == 1) {                                                               # leaf
        e <- !is.null(x[[p[1]]]); o <- x[[p[1]]]
        if (e) x[[p[1]]] <- f(o)
        return(list(x = x, e = e, o = o, n = if (e) x[[p[1]]] else NULL))
    }
    if (is.null(x[[p[1]]])) return(list(x = x, e = FALSE, o = NULL, n = NULL))
    r <- set_by_path(x[[p[1]]], p[-1], f)
    x[[p[1]]] <- r$x
    list(x = x, e = r$e, o = r$o, n = r$n)
}

flip_type <- function(v) {                                                            # deliberate invalid type
    if (is.numeric(v)) return(as.character(v))
    if (is.character(v)) return(as.integer(v))
    if (is.logical(v)) return(as.character(v))
    if (is.list(v)) return(10L)
    as.character(v)
}

rm_path <- function(x, p) {                                                           # remove nested key
    if (length(p) == 1) { e <- !is.null(x[[p[1]]]); x[[p[1]]] <- NULL; return(list(x = x, e = e)) }
    if (is.null(x[[p[1]]])) return(list(x = x, e = FALSE))
    r <- rm_path(x[[p[1]]], p[-1])
    x[[p[1]]] <- r$x
    list(x = x, e = r$e)
}

injector <- function(atoms,
                     pct_min, pct_max,
                     enable_remove = TRUE,
                     enable_type_change = TRUE,
                     enable_string_corrupt = TRUE,
                     enable_pii_inject = TRUE,
                     enable_duplicate = TRUE,
                     enable_gap = TRUE,
                     gap_threshold_sec = 3600,
                     gap_jitter_sec = 600,
                     remove_pool = c("When","What","Who"),
                     type_change_pool = c("When","What","Who"),
                     string_pool = c("What.Label","What.LabelIRI","How.ClassificationModel","How.ClassificationModelIRI"),
                     pii_names = c("John Smith","Jane Doe"),
                     seed_main = 1,
                     seed_k = NULL) {
    
    t0 <- Sys.time()                                                                     # timer
    n  <- length(atoms)                                                                  # atoms n
    if (!n) stop("injector: atoms is empty")                                             # guard
    
    if (is.null(seed_k)) seed_k <- seed_main + 10007L                                    # seed for k draw
    set.seed(seed_k)                                                                     # seed k
    kmin <- max(0L, floor(n * (pct_min / 100)))                                          # k min
    kmax <- max(0L, floor(n * (pct_max / 100)))                                          # k max
    kvec <- if (kmax >= kmin) seq.int(kmin, kmax) else kmin                              # vector
    k <- if (length(kvec)) sample(kvec, 1) else 0L                                       # choose k
    
    set.seed(seed_main)                                                                  # seed main for selections
    types <- character(0)                                                                # enabled types
    if (enable_remove) types <- c(types, "remove")
    if (enable_type_change) types <- c(types, "type_change")
    if (enable_string_corrupt) types <- c(types, "string_corrupt")
    if (enable_pii_inject) types <- c(types, "pii_inject")
    if (enable_duplicate) types <- c(types, "duplicate")
    if (enable_gap) types <- c(types, "gap")
    
    idx_all <- if (length(types) && k) sample.int(n, min(k, n)) else integer(0)          # affected atoms
    asn <- if (length(idx_all)) sample(rep(types, length.out = length(idx_all))) else character(0)
    
    idx_rm  <- idx_all[asn == "remove"]
    idx_ty  <- idx_all[asn == "type_change"]
    idx_sc  <- idx_all[asn == "string_corrupt"]
    idx_pii <- idx_all[asn == "pii_inject"]
    idx_dup <- idx_all[asn == "duplicate"]
    idx_gap <- idx_all[asn == "gap"]
    
    log <- data.frame(AtomID = character(0), path = character(0), action = character(0), existed = logical(0),
                      from = character(0), to = character(0), stringsAsFactors = FALSE)
    
    if (length(idx_rm)) {                                                                # remove fields
        path <- sample(remove_pool, length(idx_rm), replace = TRUE)
        parts <- strsplit(gsub("\\$", ".", path), "\\.", fixed = FALSE)
        for (j in seq_along(idx_rm)) {
            i <- idx_rm[j]
            r <- rm_path(atoms[[i]], parts[[j]])
            atoms[[i]] <- r$x
            log <- rbind(log, data.frame(AtomID = atoms[[i]]$Header$AtomID %||% NA_character_,
                                         path = path[j], action = "Field removed", existed = r$e,
                                         from = NA_character_, to = NA_character_, stringsAsFactors = FALSE))
        }
    }
    
    if (length(idx_ty)) {                                                                # type change
        path <- sample(type_change_pool, length(idx_ty), replace = TRUE)
        parts <- strsplit(gsub("\\$", ".", path), "\\.", fixed = FALSE)
        for (j in seq_along(idx_ty)) {
            i <- idx_ty[j]
            r <- set_by_path(atoms[[i]], parts[[j]], flip_type)
            atoms[[i]] <- r$x
            log <- rbind(log, data.frame(AtomID = atoms[[i]]$Header$AtomID %||% NA_character_,
                                         path = path[j], action = "Type change", existed = r$e,
                                         from = class(r$o)[1] %||% NA_character_,
                                         to = class(r$n)[1] %||% NA_character_,
                                         stringsAsFactors = FALSE))
        }
    }
    
    if (length(idx_sc)) {                                                                # string corrupt
        tok <- c("//","!!","??")
        path <- sample(string_pool, length(idx_sc), replace = TRUE)
        parts <- strsplit(gsub("\\$", ".", path), "\\.", fixed = FALSE)
        for (j in seq_along(idx_sc)) {
            i <- idx_sc[j]
            f <- function(v) {
                ins_mid <- function(s) {
                    s <- as.character(s)[1]
                    if (!nzchar(s)) return(s)
                    pos <- max(1, floor(nchar(s) / 2))
                    paste0(substr(s, 1, pos), sample(tok, 1), substr(s, pos + 1, nchar(s)))
                }
                if (is.list(v) && length(v)) { m <- sample.int(length(v), 1); v[[m]] <- ins_mid(v[[m]]); return(v) }
                if (is.character(v) && length(v)) { m <- sample.int(length(v), 1); v[m] <- ins_mid(v[m]); return(v) }
                v
            }
            r <- set_by_path(atoms[[i]], parts[[j]], f)
            atoms[[i]] <- r$x
            log <- rbind(log, data.frame(AtomID = atoms[[i]]$Header$AtomID %||% NA_character_,
                                         path = path[j], action = "String corrupt", existed = r$e,
                                         from = if (r$e) paste(unlist(r$o), collapse = " | ") else NA_character_,
                                         to   = if (r$e) paste(unlist(r$n), collapse = " | ") else NA_character_,
                                         stringsAsFactors = FALSE))
        }
    }
    
    if (length(idx_pii)) {                                                               # pii inject
        get_str_paths <- function(x, pre = "") {
            nm <- names(x)
            if (!is.list(x) || is.null(nm)) return(character(0))
            out <- character(0)
            for (k in nm) {
                v <- x[[k]]
                p <- if (nzchar(pre)) paste0(pre, ".", k) else k
                if (is.character(v) && length(v)) out <- c(out, p)
                if (is.list(v) && length(v) && all(vapply(v, function(z) is.character(z) && length(z), logical(1)))) out <- c(out, p)
                if (is.list(v) && !(length(v) && all(vapply(v, function(z) is.character(z) && length(z), logical(1))))) out <- c(out, get_str_paths(v, p))
            }
            out
        }
        for (j in seq_along(idx_pii)) {
            i <- idx_pii[j]
            cand <- get_str_paths(atoms[[i]])
            path <- if (length(cand)) sample(cand, 1) else NA_character_
            if (is.na(path)) {
                log <- rbind(log, data.frame(AtomID = atoms[[i]]$Header$AtomID %||% NA_character_,
                                             path = NA_character_, action = "PII inject", existed = FALSE,
                                             from = NA_character_, to = NA_character_, stringsAsFactors = FALSE))
            } else {
                parts <- strsplit(gsub("\\$", ".", path), "\\.", fixed = FALSE)[[1]]
                nmv <- sample(pii_names, 1)
                f <- function(v) {
                    if (is.list(v)) return(lapply(v, function(z) if (is.character(z) && length(z)) nmv else z))
                    if (is.character(v) && length(v)) return(rep(nmv, length(v)))
                    v
                }
                r <- set_by_path(atoms[[i]], parts, f)
                atoms[[i]] <- r$x
                log <- rbind(log, data.frame(AtomID = atoms[[i]]$Header$AtomID %||% NA_character_,
                                             path = path, action = "PII inject", existed = r$e,
                                             from = if (r$e) paste(unlist(r$o), collapse = " | ") else NA_character_,
                                             to   = if (r$e) paste(unlist(r$n), collapse = " | ") else NA_character_,
                                             stringsAsFactors = FALSE))
            }
        }
    }
    
    if (enable_gap && length(idx_gap)) {                                                  # temporal gaps
        get_num <- function(x) suppressWarnings(as.numeric((x %||% NA_real_)[1]))
        idx_other <- setdiff(idx_all, idx_gap)
        for (j in seq_along(idx_gap)) {
            jitter <- if (gap_jitter_sec > 0) sample.int(gap_jitter_sec, 1) else 0
            gap1 <- gap_threshold_sec + jitter
            pid <- vapply(atoms, function(a) (a$Who$ParticipantID %||% NA_character_)[1], "")
            t <- vapply(atoms, function(a) get_num(a$When$TimeUTC), numeric(1))
            d <- vapply(atoms, function(a) get_num(a$When$Duration), numeric(1))
            ok <- !is.na(t) & !is.na(d) & d >= 0 & nzchar(pid)
            idx_by_pid <- split(which(ok), pid[ok])
            idx_by_pid <- idx_by_pid[vapply(idx_by_pid, length, integer(1)) >= 2]
            if (!length(idx_by_pid)) next
            pid_pick <- sample(names(idx_by_pid), 1)
            ord <- idx_by_pid[[pid_pick]][order(t[idx_by_pid[[pid_pick]]])]
            if (length(ord) < 2) next
            cand_prv <- integer(0); cand_nxt <- integer(0)
            for (u in seq_len(length(ord) - 1)) {
                prv <- ord[u]
                if (prv %in% idx_other) next
                prev_end <- t[prv] + d[prv]
                if (!is.finite(prev_end)) next
                v <- u + 1
                while (v <= length(ord)) {
                    nxt <- ord[v]
                    if (nxt %in% idx_other) { v <- v + 1; next }
                    gap_now <- t[nxt] - prev_end
                    if (is.finite(gap_now) && gap_now > gap1) { cand_prv <- c(cand_prv, prv); cand_nxt <- c(cand_nxt, nxt); break }
                    v <- v + 1
                }
            }
            if (!length(cand_prv)) next
            pick <- sample.int(length(cand_prv), 1)
            prv <- cand_prv[pick]; nxt <- cand_nxt[pick]
            pos_prv <- match(prv, ord); pos_nxt <- match(nxt, ord)
            if (is.na(pos_prv) || is.na(pos_nxt) || pos_nxt <= pos_prv + 0) next
            rm_idx <- ord[seq.int(pos_prv + 1, pos_nxt - 1)]
            if (!length(rm_idx)) next
            gap0 <- t[nxt] - (t[prv] + d[prv])
            prv_id <- (atoms[[prv]]$Header$AtomID %||% NA_character_)[1]
            nxt_id <- (atoms[[nxt]]$Header$AtomID %||% NA_character_)[1]
            atoms <- atoms[-sort(rm_idx, decreasing = TRUE)]
            log <- rbind(log, data.frame(AtomID = prv_id, path = "When.TimeUTC", action = "Temporal gap", existed = TRUE,
                                         from = as.character(round(gap0, 3)), to = as.character(round(gap1, 3)), stringsAsFactors = FALSE))
            log <- rbind(log, data.frame(AtomID = nxt_id, path = "When.TimeUTC", action = "Temporal gap", existed = TRUE,
                                         from = as.character(round(gap0, 3)), to = as.character(round(gap1, 3)), stringsAsFactors = FALSE))
        }
    }
    
    if (enable_duplicate && length(idx_dup)) {                                             # duplicates
        dupe <- atoms[idx_dup]
        atoms <- c(atoms, dupe)
        
        dup_ids <- vapply(dupe, function(a) a$Header$AtomID %||% NA_character_, character(1))
        log <- rbind(log, data.frame(AtomID = dup_ids, path = NA_character_, action = "Atom duplicated", existed = TRUE,
                                     from = NA_character_, to = NA_character_, stringsAsFactors = FALSE))
        
        # also record originals as truth (same AtomIDs, because duplicates are exact copies)
        log <- rbind(log, data.frame(AtomID = dup_ids, path = NA_character_, action = "Atom duplicated (pair)", existed = TRUE,
                                     from = NA_character_, to = NA_character_, stringsAsFactors = FALSE))
    }
    
    summary <- data.frame(
        error_type = c("Field removed","Type change","String corrupt","PII inject","Temporal gap","Atom duplicated","Total unique"),
        atoms_affected = c(length(idx_rm), length(idx_ty), length(idx_sc), length(idx_pii), length(idx_gap), length(idx_dup), length(unique(idx_all))),
        k_drawn = k,
        pct_min = pct_min,
        pct_max = pct_max,
        seed_main = seed_main,
        seed_k = seed_k,
        stringsAsFactors = FALSE
    )
    
    dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
    list(atoms = atoms, changelog = log, summary = summary, seconds = dt)
}

# ============================================================
# VALIDATOR (logic preserved, refactored IO, no baseline recursion)
# ============================================================

validator <- function(atoms,
                      registry_behaviour,
                      registry_rest,
                      registry_coel,
                      gap_threshold_sec = 3600,
                      pii_forename_csvs = character(0),
                      pii_surname_csvs  = character(0),
                      run_TEST1 = TRUE,
                      run_TEST2 = TRUE,
                      run_TEST3 = TRUE,
                      run_TEST4 = TRUE,
                      run_TEST5 = TRUE,
                      run_TEST6 = TRUE,
                      run_TEST7 = TRUE) {
    
    get_path <- function(x, p) {                                                          # safe getter
        for (k in p) { if (!is.list(x) || is.null(x[[k]])) return(NULL); x <- x[[k]] }
        x
    }
    
    github_raw <- function(u) sub("https://github.com/([^/]+)/([^/]+)/blob/([^?]+).*", "https://raw.githubusercontent.com/\\1/\\2/\\3", u)
    to_src <- function(p) if (grepl("^https?://", p)) github_raw(p) else p
    
    norm_col <- function(x) tolower(gsub("[^a-z]", "", x))
    get_col <- function(df, target) { cn <- norm_col(names(df)); j <- which(cn == norm_col(target)); if (length(j)) j[1] else NA_integer_ }
    
    read_name_cols <- function(path, col_targets) {
        df <- read.csv(to_src(path), stringsAsFactors = FALSE, check.names = FALSE)
        out <- character(0)
        for (ct in col_targets) { j <- get_col(df, ct); if (!is.na(j)) out <- c(out, as.character(df[[j]])) }
        out <- tolower(trimws(out))
        unique(out[nzchar(out) & !is.na(out)])
    }
    
    load_name_set <- function(paths, col_targets) {
        if (!length(paths)) return(character(0))
        unique(unlist(lapply(paths, read_name_cols, col_targets = col_targets), use.names = FALSE))
    }
    
    str_flat <- function(x) {
        if (is.character(x) && length(x)) return(x)
        if (!is.list(x) || !length(x)) return(character(0))
        unlist(lapply(x, str_flat), use.names = FALSE)
    }
    
    tokenise <- function(strings) {
        txt <- paste(strings, collapse = " ")
        w <- unlist(regmatches(txt, gregexpr("\\p{L}+", txt, perl = TRUE)), use.names = FALSE)
        tolower(w)
    }
    
    pii_hit2 <- function(strings, forenames, surnames, win = 6) {
        nm_all <- unique(c(forenames, surnames))
        s0 <- tolower(trimws(strings))
        if (length(s0)) { m <- match(s0, nm_all); if (any(!is.na(m))) return(paste0("exact:", s0[which(!is.na(m))[1]])) }
        w <- tokenise(strings); L <- length(w)
        if (L < 2) return(NA_character_)
        for (i in seq_len(L - 1)) {
            a <- w[i]; b <- w[i + 1]
            if ((a %in% forenames && b %in% surnames) || (a %in% surnames && b %in% forenames)) return(paste(a, b))
        }
        for (i in seq_len(L - 1)) {
            if (!(w[i] %in% nm_all)) next
            j <- seq.int(i + 1, min(L, i + win))
            k <- j[w[j] %in% nm_all][1]
            if (!is.na(k)) return(paste(w[i], w[k]))
        }
        NA_character_
    }
    
    pick_col_sub <- function(df, keys) {
        cn <- tolower(names(df))
        for (k in keys) { j <- which(grepl(k, cn, fixed = TRUE)); if (length(j)) return(j[1]) }
        1
    }
    
    read_set <- function(path, keys) {
        df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
        col <- pick_col_sub(df, keys)
        x <- tolower(trimws(as.character(df[[col]])))
        unique(x[nzchar(x) & !is.na(x)])
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
    
    sig_full <- function(a) jsonlite::toJSON(canon(a), auto_unbox = TRUE, pretty = FALSE, na = "null")
    
    n <- length(atoms)
    ids <- vapply(atoms, function(a) { x <- get_path(a, c("Header","AtomID")); if (is.character(x) && length(x)) x[1] else NA_character_ }, character(1))
    ids[is.na(ids) | !nzchar(ids)] <- paste0("index_", which(is.na(ids) | !nzchar(ids)))
    
    allow_label <- character(0)
    allow_code  <- character(0)
    if (run_TEST4) {
        allow_b <- read_set(registry_behaviour, c("label","name","term","code"))
        allow_r <- read_set(registry_rest,      c("label","name","term","code"))
        allow_c <- read_set(registry_coel,      c("coel_code","code"))
        allow_label <- unique(c(allow_b, allow_r))
        allow_code  <- unique(allow_c)
    }
    
    forenames <- character(0)
    surnames  <- character(0)
    if (run_TEST5) {
        name_cols <- c("Localized Name","Romanized Name","Romantized Name")
        forenames <- load_name_set(pii_forename_csvs, name_cols)
        surnames  <- load_name_set(pii_surname_csvs,  name_cols)
    }
    
    dup_sig <- rep(FALSE, n)
    if (run_TEST6) {
        sig <- vapply(atoms, sig_full, "")
        dup_sig <- duplicated(sig) | duplicated(sig, fromLast = TRUE)
    }
    
    gap_ids <- character(0)
    if (run_TEST7) {
        pid <- vapply(atoms, function(a) { x <- get_path(a, c("Who","ParticipantID")); if (is.character(x) && length(x)) x[1] else NA_character_ }, character(1))
        t_utc <- vapply(atoms, function(a) { x <- get_path(a, c("When","TimeUTC")); suppressWarnings(as.numeric(if (length(x)) x[1] else NA_real_)) }, numeric(1))
        dur <- vapply(atoms, function(a) { x <- get_path(a, c("When","Duration")); suppressWarnings(as.numeric(if (length(x)) x[1] else NA_real_)) }, numeric(1))
        ok <- !is.na(t_utc) & !is.na(dur) & nzchar(pid)
        idx_by_pid <- split(which(ok), pid[ok])
        gap_ids <- unique(unlist(lapply(idx_by_pid, function(ix) {
            ord <- ix[order(t_utc[ix])]
            if (length(ord) < 2) return(character(0))
            prv <- ord[-length(ord)]
            nxt <- ord[-1]
            gap <- t_utc[nxt] - (t_utc[prv] + dur[prv])
            hit <- which(gap > gap_threshold_sec)
            if (!length(hit)) return(character(0))
            ids[c(prv[hit], nxt[hit])]
        }), use.names = FALSE))
    }
    
    out_tests  <- vector("list", n)
    out_detail <- vector("list", n)
    
    for (i in seq_len(n)) {
        a <- atoms[[i]]
        id <- ids[i]
        tests <- character(0)
        dets  <- character(0)
        
        if (run_TEST1) {
            miss <- character(0)
            if (is.null(get_path(a, c("Header")))) miss <- c(miss, "Header")
            if (is.null(get_path(a, c("When"))))   miss <- c(miss, "When")
            if (is.null(get_path(a, c("What"))))   miss <- c(miss, "What")
            if (is.null(get_path(a, c("Who"))))    miss <- c(miss, "Who")
            if (length(miss)) { tests <- c(tests, "TEST1"); dets <- c(dets, paste("TEST1:", paste(miss, collapse = " | "))) }
        }
        
        if (run_TEST2) {
            miss <- character(0)
            if (is.null(get_path(a, c("Header","AtomVersion")))) miss <- c(miss, "Header.AtomVersion")
            if (is.null(get_path(a, c("When","TimeUTC"))))       miss <- c(miss, "When.TimeUTC")
            if (is.null(get_path(a, c("When","Duration"))))      miss <- c(miss, "When.Duration")
            if (is.null(get_path(a, c("When","UTCOffset"))))     miss <- c(miss, "When.UTCOffset")
            if (is.null(get_path(a, c("What","Label"))))         miss <- c(miss, "What.Label")
            if (is.null(get_path(a, c("Who","ParticipantID"))))  miss <- c(miss, "Who.ParticipantID")
            if (length(miss)) { tests <- c(tests, "TEST2"); dets <- c(dets, paste("TEST2:", paste(miss, collapse = " | "))) }
        }
        
        if (run_TEST3) {
            bad <- character(0)
            hdr <- get_path(a, c("Header")); whn <- get_path(a, c("When")); wht <- get_path(a, c("What")); who <- get_path(a, c("Who"))
            if (!is.null(hdr) && !is.list(hdr)) bad <- c(bad, "Header")
            if (!is.null(whn) && !is.list(whn)) bad <- c(bad, "When")
            if (!is.null(wht) && !is.list(wht)) bad <- c(bad, "What")
            if (!is.null(who) && !is.list(who)) bad <- c(bad, "Who")
            av <- get_path(a, c("Header","AtomVersion"))
            du <- get_path(a, c("When","Duration"))
            tu <- get_path(a, c("When","TimeUTC"))
            of <- get_path(a, c("When","UTCOffset"))
            pid <- get_path(a, c("Who","ParticipantID"))
            lb <- get_path(a, c("What","Label"))
            if (!is.null(av)  && !is.character(av)) bad <- c(bad, "Header.AtomVersion")
            if (!is.null(du)  && !(is.numeric(du) || is.integer(du))) bad <- c(bad, "When.Duration")
            if (!is.null(tu)  && !(is.numeric(tu) || is.integer(tu))) bad <- c(bad, "When.TimeUTC")
            if (!is.null(of)  && !(is.numeric(of) || is.integer(of))) bad <- c(bad, "When.UTCOffset")
            if (!is.null(pid) && !is.character(pid)) bad <- c(bad, "Who.ParticipantID")
            if (!is.null(lb)) {
                ok_lb <- (is.list(lb) && all(vapply(lb, is.character, logical(1)))) || is.character(lb)
                if (!ok_lb) bad <- c(bad, "What.Label")
            }
            if (length(bad)) { tests <- c(tests, "TEST3"); dets <- c(dets, paste("TEST3:", paste(unique(bad), collapse = " | "))) }
        }
        
        if (run_TEST4) {
            lb <- get_path(a, c("What","Label"))
            lab <- if (is.null(lb)) character(0) else tolower(trimws(unlist(lb)))
            lab <- lab[nzchar(lab) & !is.na(lab)]
            if (length(lab)) {
                bad <- lab[!(lab %in% allow_label) & !(lab %in% allow_code)]
                if (length(bad)) { tests <- c(tests, "TEST4"); dets <- c(dets, paste("TEST4:", paste(bad, collapse = " | "))) }
            }
        }
        
        if (run_TEST5) {
            if (length(forenames) && length(surnames)) {
                hit <- pii_hit2(str_flat(a), forenames, surnames)
                if (!is.na(hit)) { tests <- c(tests, "TEST5"); dets <- c(dets, paste("TEST5:", hit)) }
            }
        }
        
        if (run_TEST6 && dup_sig[i]) { tests <- c(tests, "TEST6"); dets <- c(dets, "TEST6: content_duplicate_full") }
        
        if (run_TEST7 && (id %in% gap_ids)) { tests <- c(tests, "TEST7"); dets <- c(dets, paste0("TEST7: gap>", gap_threshold_sec, "s")) }
        
        out_tests[[i]] <- unique(tests)
        out_detail[[i]] <- unique(dets)
    }
    
    keep <- vapply(out_tests, length, integer(1)) > 0
    vio <- data.frame(
        AtomID = ids[keep],
        TEST = vapply(out_tests[keep], function(x) paste(x, collapse = " | "), ""),
        detail = vapply(out_detail[keep], function(x) paste(x, collapse = " ; "), ""),
        stringsAsFactors = FALSE
    )
    
    list(violations = vio, total_atoms = n)
}

# ============================================================
# EVALUATOR
# ============================================================

evaluator_any <- function(changelog, violations, total_atoms) {                          # overall
    T <- unique(changelog$AtomID[!is.na(changelog$AtomID) & nzchar(changelog$AtomID)])
    P <- unique(violations$AtomID[!is.na(violations$AtomID) & nzchar(violations$AtomID)])
    TP <- length(intersect(T, P))
    FP <- length(setdiff(P, T))
    FN <- length(setdiff(T, P))
    TN <- total_atoms - TP - FP - FN
    acc <- (TP + TN) / (TP + FP + FN + TN)
    prec <- if ((TP + FP) == 0) NA_real_ else TP / (TP + FP)
    sens <- if ((TP + FN) == 0) NA_real_ else TP / (TP + FN)
    spec <- if ((TN + FP) == 0) NA_real_ else TN / (TN + FP)
    f1 <- if (is.na(prec) || is.na(sens) || (prec + sens) == 0) NA_real_ else 2 * prec * sens / (prec + sens)
    data.frame(TP = TP, FP = FP, FN = FN, TN = TN, Accuracy = acc, Precision = prec, Sensitivity = sens, Specificity = spec, F1 = f1,
               stringsAsFactors = FALSE)
}

# ============================================================
# PII NAME POOL (fixed size, reproducible per stream + run_id)
# ============================================================

pii_name_pool <- function(seed, n = 80) {                                                # generate names
    loc_all <- charlatan_locales()$Name                                                     # locales
    ok <- vapply(loc_all, function(z) !inherits(try(ch_name(n = 1, locale = z), silent = TRUE), "try-error"), logical(1))
    loc <- loc_all[ok]
    set.seed(seed)
    out <- unique(unlist(lapply(sample(loc, min(10, length(loc))), function(z) ch_name(10, locale = z)), use.names = FALSE))
    out <- out[nzchar(out)]
    if (length(out) < n) out <- rep(out, length.out = n)
    out[seq_len(n)]
}

pii_names <- pii_name_pool(seed_from_key(paste(stream, run_id, "PII_POOL", sep = "|")), n = 80)  # pool
write.csv(data.frame(pii_names = pii_names, stringsAsFactors = FALSE), file.path(out_root, "pii_name_pool.csv"), row.names = FALSE)

# ============================================================
# CQ DEFINITIONS
# - You can adjust pools per CQ here
# ============================================================

mand_pool <- c("Header.AtomVersion","When","When.TimeUTC","When.UTCOffset","When.Duration","What","What.Label","Who")  # mandatory-like
string_pool <- c("What.Label","What.LabelIRI","How.ClassificationModel","How.ClassificationModelIRI")                 # strings

cq_plan <- data.frame(
    cq = c("CQ1","CQ2","CQ3","CQ4","CQ5"),
    enable_remove = c(TRUE,  FALSE, FALSE, FALSE, FALSE),
    enable_type_change = c(FALSE, TRUE,  FALSE, FALSE, FALSE),
    enable_string_corrupt = c(FALSE, FALSE, TRUE,  FALSE, FALSE),
    enable_pii_inject = c(FALSE, FALSE, FALSE, FALSE, FALSE),   # not used in CQ1-5
    enable_duplicate = c(FALSE, FALSE, FALSE, TRUE,  FALSE),    # CQ4 duplicates
    enable_gap = c(FALSE, FALSE, FALSE, FALSE, TRUE),           # CQ5 gaps
    stringsAsFactors = FALSE
)

# ============================================================
# RUN
# ============================================================

atoms_base <- read_json_any(payload_json)                                                 # load baseline payload
stopifnot(is.list(atoms_base) && length(atoms_base) > 0)                                  # guard

# ---- baseline sanity check: selected tests should pass with 0 violations ----
baseline_v <- validator(atoms_base,
                        registry_behaviour = registry_behaviour,
                        registry_rest = registry_rest,
                        registry_coel = registry_coel,
                        gap_threshold_sec = gap_threshold_sec,
                        pii_forename_csvs = pii_forename_csv,
                        pii_surname_csvs = pii_surname_csv,
                        run_TEST1 = TRUE, run_TEST2 = TRUE, run_TEST3 = TRUE, run_TEST4 = TRUE, run_TEST5 = FALSE, run_TEST6 = TRUE, run_TEST7 = FALSE)

write.csv(baseline_v$violations, file.path(out_root, "baseline_violations.csv"), row.names = FALSE)  # baseline vio
write.csv(data.frame(total_atoms = baseline_v$total_atoms, violation_atoms = nrow(baseline_v$violations)),
          file.path(out_root, "baseline_summary.csv"), row.names = FALSE)

# ---- per CQ: seed, inject, validate, score ----
all_metrics <- vector("list", nrow(cq_plan))                                             # store metrics
all_seeded_summary <- vector("list", nrow(cq_plan))                                      # store seeding summary

for (i in seq_len(nrow(cq_plan))) {
    
    cq <- cq_plan$cq[i]                                                                     # cq label
    seed_main <- seeds$seed[seeds$cq == cq][1]                                              # main seed
    seed_k <- seed_from_key(paste(stream, run_id, cq, "K", sep = "|"))                      # k seed
    
    cq_dir <- file.path(out_root, cq)                                                       # cq output dir
    dir.create(cq_dir, recursive = TRUE, showWarnings = FALSE)
    
    inj <- injector(atoms = atoms_base,
                    pct_min = pct_min, pct_max = pct_max,
                    enable_remove = cq_plan$enable_remove[i],
                    enable_type_change = cq_plan$enable_type_change[i],
                    enable_string_corrupt = cq_plan$enable_string_corrupt[i],
                    enable_pii_inject = cq_plan$enable_pii_inject[i],
                    enable_duplicate = cq_plan$enable_duplicate[i],
                    enable_gap = cq_plan$enable_gap[i],
                    gap_threshold_sec = gap_threshold_sec,
                    gap_jitter_sec = 0,
                    remove_pool = mand_pool,
                    type_change_pool = mand_pool,
                    string_pool = string_pool,
                    pii_names = pii_names,
                    seed_main = seed_main,
                    seed_k = seed_k)
    
    write_json_pretty(inj$atoms, file.path(cq_dir, paste0("payload_seeded_", cq, ".json")))  # seeded payload
    write.csv(inj$changelog, file.path(cq_dir, paste0("changelog_", cq, ".csv")), row.names = FALSE) # changelog
    write.csv(inj$summary, file.path(cq_dir, paste0("seed_summary_", cq, ".csv")), row.names = FALSE) # seed summary
    
    run1 <- list(run_TEST1 = FALSE, run_TEST2 = FALSE, run_TEST3 = FALSE, run_TEST4 = FALSE, run_TEST5 = FALSE, run_TEST6 = FALSE, run_TEST7 = FALSE) # init
    if (cq == "CQ1") { run1$run_TEST1 <- TRUE; run1$run_TEST2 <- TRUE }                    # missing fields hits TEST1/2
    if (cq == "CQ2") { run1$run_TEST3 <- TRUE }                                            # type change hits TEST3
    if (cq == "CQ3") { run1$run_TEST4 <- TRUE }                                            # string corrupt hits TEST4
    if (cq == "CQ4") { run1$run_TEST6 <- TRUE }                                            # duplicates hits TEST6
    if (cq == "CQ5") { run1$run_TEST7 <- TRUE }                                            # gaps hits TEST7
    
    val <- validator(inj$atoms,
                     registry_behaviour = registry_behaviour,
                     registry_rest = registry_rest,
                     registry_coel = registry_coel,
                     gap_threshold_sec = gap_threshold_sec,
                     pii_forename_csvs = pii_forename_csv,
                     pii_surname_csvs = pii_surname_csv,
                     run_TEST1 = run1$run_TEST1, run_TEST2 = run1$run_TEST2, run_TEST3 = run1$run_TEST3,
                     run_TEST4 = run1$run_TEST4, run_TEST5 = run1$run_TEST5, run_TEST6 = run1$run_TEST6, run_TEST7 = run1$run_TEST7)
    
    write.csv(val$violations, file.path(cq_dir, paste0("violations_", cq, ".csv")), row.names = FALSE)  # violations
    
    ids_seeded <- vapply(inj$atoms, function(a) (a$Header$AtomID %||% NA_character_)[1], "")            # ids
    chg <- inj$changelog                                                                            # changelog
    chg <- chg[chg$existed == TRUE & chg$AtomID %in% ids_seeded, , drop = FALSE]                     # keep valid ids
    
    met <- evaluator_any(chg, val$violations, length(inj$atoms))                                     # metrics
    met$cq <- cq                                                                                    # add
    met$stream <- stream                                                                            # add
    met$run_id <- run_id                                                                            # add
    met$seed_main <- seed_main                                                                       # add
    met$seed_k <- seed_k                                                                             # add
    met$k_drawn <- inj$summary$k_drawn[1]                                                            # add
    met$pct_min <- pct_min                                                                           # add
    met$pct_max <- pct_max                                                                           # add
    
    write.csv(met, file.path(cq_dir, paste0("metrics_any_", cq, ".csv")), row.names = FALSE)         # metrics file
    
    all_metrics[[i]] <- met                                                                          # collect
    all_seeded_summary[[i]] <- cbind(data.frame(cq = cq, stream = stream, run_id = run_id, stringsAsFactors = FALSE), inj$summary) # collect
}

metrics_all <- do.call(rbind, all_metrics)                                                          # bind
seeded_all  <- do.call(rbind, all_seeded_summary)                                                   # bind

write.csv(metrics_all, file.path(out_root, "metrics_any_all_cq.csv"), row.names = FALSE)             # all metrics
write.csv(seeded_all,  file.path(out_root, "seed_summary_all_cq.csv"), row.names = FALSE)            # all seed summaries

cat("Done\n")                                                                                        # message
cat("Outputs:", out_root, "\n")                                                                       # path
