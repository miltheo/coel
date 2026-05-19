# COEL Atom payload summary tables
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require("jsonlite")

# ----------------------------
# 0) Helpers
# ----------------------------
file_size_mb <- function(path) {
    if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) return(NA_real_)
    as.numeric(file.info(path)$size) / (1024^2)
}

pick_col <- function(df, candidates) {
    nm <- names(df)
    hit <- candidates[candidates %in% nm]
    if (length(hit) == 0) stop("Missing expected column. Tried: ", paste(candidates, collapse = ", "), call. = FALSE)
    hit[[1]]
}

# safe nested getter (base R)
get_in <- function(x, path, default = NULL) {
    cur <- x
    for (p in path) {
        if (!is.list(cur) || is.null(cur[[p]])) return(default)
        cur <- cur[[p]]
    }
    cur
}

read_atoms_json <- function(path) {
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rb") else file(path, "rb")
    on.exit(close(con), add = TRUE)
    txt <- readLines(con, warn = FALSE, encoding = "UTF-8")
    x <- jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
    if (is.list(x) && !is.null(x$Atoms)) return(x$Atoms)
    x
}


# ----------------------------
# 1) Minimal flattening for pre-CQ summaries
# ----------------------------
atoms_to_df_min <- function(atoms, stream_name) {
    n <- length(atoms)
    out <- vector("list", n)
    
    for (i in seq_len(n)) {
        a <- atoms[[i]]
        
        lbl <- get_in(a, c("What", "Label"), default = character(0))
        iri <- get_in(a, c("What", "LabelIRI"), default = character(0))
        
        native_label <- if (length(lbl) >= 1) as.character(lbl[[1]]) else NA_character_
        coel_code    <- if (length(lbl) >= 2) as.character(lbl[[2]]) else NA_character_
        native_iri   <- if (length(iri) >= 1) as.character(iri[[1]]) else NA_character_
        coel_iri     <- if (length(iri) >= 2) as.character(iri[[2]]) else NA_character_
        
        time_utc    <- suppressWarnings(as.numeric(get_in(a, c("When", "TimeUTC"), default = NA)))
        duration_s  <- suppressWarnings(as.numeric(get_in(a, c("When", "Duration"), default = NA)))
        end_utc     <- if (!is.na(time_utc) && !is.na(duration_s)) time_utc + duration_s else NA_real_
        
        out[[i]] <- data.frame(
            stream = stream_name,
            atom_id = as.character(get_in(a, c("Header", "AtomID"), default = NA_character_)),
            participant_id = as.character(get_in(a, c("Who", "ParticipantID"), default = NA_character_)),
            time_utc = time_utc,
            duration_s = duration_s,
            end_utc = end_utc,
            native_label = native_label,
            coel_code = coel_code,
            native_iri = native_iri,
            coel_iri = coel_iri,
            stringsAsFactors = FALSE
        )
    }
    
    do.call(rbind, out)
}

# ----------------------------
# 2) 4.2.3 Stream summary table
# ----------------------------
stream_summary <- function(df, json_path = NA_character_, gz_path = NA_character_) {
    participants <- df$participant_id[!is.na(df$participant_id) & nzchar(df$participant_id)]
    native_labels <- df$native_label[!is.na(df$native_label) & nzchar(df$native_label)]
    coel_codes <- df$coel_code[!is.na(df$coel_code) & nzchar(df$coel_code)]
    
    data.frame(
        stream = unique(df$stream)[1],
        atoms_n = nrow(df),
        participants_n = length(unique(participants)),
        unique_native_labels_n = length(unique(native_labels)),
        unique_coel_codes_n = length(unique(coel_codes)),
        median_duration_s = if (all(is.na(df$duration_s))) NA_real_ else stats::median(df$duration_s, na.rm = TRUE),
        start_time_utc = if (all(is.na(df$time_utc))) NA_real_ else min(df$time_utc, na.rm = TRUE),
        end_time_utc = if (all(is.na(df$end_utc))) NA_real_ else max(df$end_utc, na.rm = TRUE),
        total_duration_h = sum(df$duration_s, na.rm = TRUE) / 3600,
        json_size_mb = file_size_mb(json_path),
        gz_size_mb = file_size_mb(gz_path),
        stringsAsFactors = FALSE
    )
}

# ----------------------------
# 3) 4.3 Mapping coverage
# ----------------------------

load_coel_registry <- function(path) {
    r <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) 
    code_col <- pick_col(r, c("code","Code","coel_code","COELCode","coelCode","id","ID")) 
    iri_col <- pick_col(r, c("iri","IRI","labelIRI","LabelIRI","termIRI","TermIRI")) 
    reg <- unique(data.frame( coel_code = as.character(r[[code_col]]), 
                              coel_iri = as.character(r[[iri_col]]), 
                              stringsAsFactors = FALSE )) 
    reg[!is.na(reg$coel_code) & nzchar(reg$coel_code), , drop = FALSE] 
}

load_mapping_native_to_coel <- function(path) 
{ 
    m <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) 
    native_col <- pick_col(m, c("label","Label","source_label","SourceLabel",
                                "model_label","ModelLabel","native_label",
                                "NativeLabel")) 
    coel_col <- pick_col(m, c("coel_code","COELCode","coel","COEL","mapped_code",
                              "MappedCOELCode")) 
    mp <- unique(data.frame( 
        native_label = as.character(m[[native_col]]), 
        coel_code = as.character(m[[coel_col]]), 
        stringsAsFactors = FALSE )) 
    
    mp[!is.na(mp$native_label) & nzchar(mp$native_label), , drop = FALSE] 
}

# helper: non-empty string
is_nonempty <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

# Compute in-payload mapping coverage:
# For each native label (Label[1]), check whether any instance has a mapped COEL code (Label[2]).
mapping_coverage_in_payload <- function(df) {
    # unique native labels present
    nat <- df$native_label[!is.na(df$native_label) & nzchar(df$native_label)]
    if (length(nat) == 0L) {
        return(data.frame(
            stream = unique(df$stream)[1],
            unique_native_labels_n = 0L,
            mapped_native_labels_n = 0L,
            unmapped_native_labels_n = 0L,
            pct_native_mapped = NA_real_,
            unique_coel_codes_n = 0L,
            stringsAsFactors = FALSE
        ))
    }
    
    # For each native label, is there any non-empty coel_code in the payload?
    # Split rows by native label, then evaluate.
    rows_by_label <- split(df, df$native_label)
    
    mapped_flag <- vapply(rows_by_label, function(dfi) {
        any(!is.na(dfi$coel_code) & nzchar(trimws(dfi$coel_code)))
    }, logical(1))
    
    u_native <- names(rows_by_label)
    mapped_labels <- u_native[mapped_flag]
    unmapped_labels <- u_native[!mapped_flag]
    
    # COEL codes observed in payload.
    u_coel <- unique(df$coel_code[!is.na(df$coel_code) & nzchar(df$coel_code)])
    
    data.frame(
        stream = unique(df$stream)[1],
        unique_native_labels_n = length(u_native),
        mapped_native_labels_n = length(mapped_labels),
        unmapped_native_labels_n = length(unmapped_labels),
        pct_native_mapped = 100 * length(mapped_labels) / length(u_native),
        unique_coel_codes_n = length(u_coel),
        top_unmapped_native = paste(head(sort(unmapped_labels), 10), collapse = "; "),
        stringsAsFactors = FALSE
    )
}

# ============================================================
# RUN
# ============================================================

repo_root <- coel_repo_root()

# Stream-specific combined payloads.
bout_json <- file.path(repo_root, "utilities", "data", "atoms_payload", "behavioural_bout", "coel_atoms_payload.json")
bout_gz   <- file.path(repo_root, "utilities", "data", "atoms_payload", "behavioural_bout", "coel_atoms_payload.json.gz")

rest_json <- file.path(repo_root, "utilities", "data", "atoms_payload", "rest_activity", "coel_atoms_payload.json")
rest_gz   <- file.path(repo_root, "utilities", "data", "atoms_payload", "rest_activity", "coel_atoms_payload.json.gz")

# Registry + mapping resources
coel_registry_path <- file.path(repo_root, "models", "coel", "2.0", "coel-model-v2.0.csv")
map_bout_path <- file.path(repo_root, "mapping", "behavioural-bout-model-v1.0-to-coel-model-v2.0.csv")
map_rest_path <- file.path(repo_root, "mapping", "rest-activity-model-v1.0-to-coel-model-v2.0.csv")

# Output folder
out_dir <- file.path(repo_root, "utilities", "data", "research_paper_results", "tables")
ensure_dir(out_dir)

# Read payloads
atoms_bout <- read_atoms_json(bout_json)
atoms_rest <- read_atoms_json(rest_json)

# Flatten
df_bout <- atoms_to_df_min(atoms_bout, "Behavioural Bout stream")
df_rest <- atoms_to_df_min(atoms_rest, "Rest-activity stream")

# Load registry + mappings
coel_reg <- load_coel_registry(coel_registry_path)
map_bout <- load_mapping_native_to_coel(map_bout_path)
map_rest <- load_mapping_native_to_coel(map_rest_path)

# 4.2.3 table: stream summary
tbl_stream_summary <- rbind(
    stream_summary(df_bout, bout_json, bout_gz),
    stream_summary(df_rest, rest_json, rest_gz)
)


# 2) Mapping coverage for each stream, including combined (in-payload)
tbl_mapping_coverage_all <- rbind(
    mapping_coverage_in_payload(df_bout),
    mapping_coverage_in_payload(df_rest)
)


# 3) Merge coverage (%) into the stream summary to form Table 5
cov_pct <- tbl_mapping_coverage_all[, c("stream", "pct_native_mapped")]
tbl_table5 <- merge(tbl_stream_summary, cov_pct, by = "stream", all.x = TRUE, sort = FALSE)

# Keep original stream order
ord <- match(tbl_stream_summary$stream, tbl_table5$stream)
tbl_table5 <- tbl_table5[ord, , drop = FALSE]

# Drop time-range columns if you do not want them in Table 5
tbl_table5$start_time_utc <- NULL
tbl_table5$end_time_utc <- NULL

# Rename and order columns to match your manuscript Table 5
tbl_table5 <- tbl_table5[, c(
    "stream",
    "atoms_n",
    "participants_n",
    "unique_native_labels_n",
    "unique_coel_codes_n",
    "pct_native_mapped",
    "median_duration_s",
    "total_duration_h",
    "json_size_mb",
    "gz_size_mb"
)]

names(tbl_table5) <- c(
    "Atom Stream",
    "Atoms (n)",
    "Participants (n)",
    "Event labels (n)",
    "COEL codes (n)",
    "Mapping Coverage (%)",
    "Median dur (s)",
    "Total dur (h)",
    "JSON (MB)",
    "Gzip (MB)"
)

# Optional rounding for presentation (keep raw if you prefer)
tbl_table5[["Mapping Coverage (%)"]] <- round(tbl_table5[["Mapping Coverage (%)"]], 1)
tbl_table5[["Total dur (h)"]] <- round(tbl_table5[["Total dur (h)"]], 1)
tbl_table5[["JSON (MB)"]] <- round(tbl_table5[["JSON (MB)"]], 2)
tbl_table5[["Gzip (MB)"]] <- round(tbl_table5[["Gzip (MB)"]], 2)

# 4) Write outputs
write.csv(tbl_table5, file.path(out_dir, "Table_atom_stream_and_payload_summary.csv"), row.names = FALSE)

# Table 6: in-payload mapping diagnostics (now including combined)
tbl_table6 <- tbl_mapping_coverage_all

names(tbl_table6) <- c(
    "Atom Stream",
    "Event labels (n)",
    "Mapped labels (n)",
    "Unmapped labels (n)",
    "Mapping Coverage (%)",
    "COEL codes (n)",
    "Unmapped labels (examples)"
)
tbl_table6[["Mapping Coverage (%)"]] <- round(tbl_table6[["Mapping Coverage (%)"]], 1)

write.csv(tbl_table6, file.path(out_dir, "Table_mapping_coverage_in_payload.csv"), row.names = FALSE)

# "Coverage is calculated over unique event stream labels relative to the curated mapping to COEL codes."

unmapped_labels <- function(df) {
    rows_by <- split(df, df$native_label)
    mapped_flag <- vapply(rows_by, function(dfi) any(!is.na(dfi$coel_code) & nzchar(trimws(dfi$coel_code))), logical(1))
    sort(names(rows_by)[!mapped_flag])
}

u_bout <- unmapped_labels(df_bout)
u_rest <- unmapped_labels(df_rest)

u_bout
u_rest

# check overlap of native labels across streams (explains 9 vs 10)
intersect(unique(df_bout$native_label), unique(df_rest$native_label))
