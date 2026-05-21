# COEL Atom payload to JSON-LD projection
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require("jsonlite")

# ---- paths (edit once) ----
repo_root  <- coel_repo_root()
context_fp <- file.path(repo_root, "utilities", "jsonld", "context.jsonld")
stopifnot(file.exists(context_fp))

atoms_root <- file.path(repo_root, "utilities", "data", "atoms_participants")               # per-participant atoms
out_root   <- file.path(repo_root, "utilities", "data", "atoms_payload")                    # combined payload outputs

rest_in    <- file.path(atoms_root, "rest_activity")                                        # input folder (rest)
bout_in    <- file.path(atoms_root, "behavioural_bout")                                     # input folder (bout)

rest_out   <- file.path(out_root, "rest_activity", "coel_atoms_payload.json")               # payload json (rest)
bout_out   <- file.path(out_root, "behavioural_bout", "coel_atoms_payload.json")            # payload json (bout)

rest_outld <- file.path(out_root, "rest_activity", "coel_atoms_payload.jsonld")             # payload jsonld (rest)
bout_outld <- file.path(out_root, "behavioural_bout", "coel_atoms_payload.jsonld")          # payload jsonld (bout)

# ---- combine per-participant atoms to one payload json ----
read_atoms_file <- function(path) {                                                         # read one per-participant atoms file
    x <- read_json_any(path)                                                                  # parse
    if (!is.list(x)) stop("Not a JSON array: ", path)                                         # check
    x
}

check_duplicate_atom_ids <- function(atoms) {                                               # stop if AtomID duplicates exist
    ids <- vapply(atoms, function(a) {                                                        # extract AtomID
        if (!is.list(a) || is.null(a$Header) || is.null(a$Header$AtomID)) return(NA_character_) # missing
        as.character(a$Header$AtomID)                                                           # AtomID
    }, character(1))
    dup <- unique(ids[duplicated(ids) & !is.na(ids) & nzchar(ids)])                           # duplicates
    if (length(dup)) stop("Duplicate AtomID detected (example): ", dup[1])                    # stop
    invisible(TRUE)                                                                           # ok
}

combine_atoms_to_payload <- function(in_dir, out_json_path, pattern = "\\.json$", gzip = TRUE) {
    files <- list.files(in_dir, pattern = pattern, full.names = TRUE)                         # list
    if (!length(files)) stop("No input JSON files in: ", in_dir)                              # check
    atoms_list <- lapply(files, read_atoms_file)                                              # read all
    atoms_all  <- do.call(c, atoms_list)                                                      # flatten
    check_duplicate_atom_ids(atoms_all)                                                       # validate
    write_json_any(atoms_all, out_json_path, gzip = gzip)                                     # write
    invisible(out_json_path)                                                                  # return path
}

# ---- JSON payload to JSON-LD projection ----
mint_atom_urn <- function(atom_id, prefix = "urn:coel:atom:") {                             # mint stable @id from AtomID
    paste0(prefix, utils::URLencode(as.character(atom_id), reserved = TRUE))                  # encode
}

ensure_labeliri_set <- function(a) {                                                        # ensure What.LabelIRI is a list (set container)
    if (is.null(a$What) || is.null(a$What$LabelIRI)) return(a)                                # skip if absent
    v <- a$What$LabelIRI                                                                      # current
    if (is.character(v)) a$What$LabelIRI <- as.list(v)                                        # scalar -> list
    if (is.list(v) && !is.null(names(v))) a$What$LabelIRI <- unname(v)                        # named -> unnamed
    a
}

payload_to_jsonld <- function(payload_atoms, context_path) {                                # build JSON-LD document
    ctx_doc <- read_json_any(context_path)                                                    # read context.jsonld
    ctx     <- ctx_doc[["@context"]] %||% ctx_doc                                             # accept either style
    atoms   <- payload_atoms                                                                  # copy
    atoms   <- lapply(atoms, function(a) {                                                    # mutate each Atom
        aid <- a$Header$AtomID %||% NA_character_                                               # AtomID
        if (!is.na(aid) && nzchar(aid)) a[["@id"]] <- mint_atom_urn(aid)                        # add @id
        a <- ensure_labeliri_set(a)                                                             # normalise LabelIRI
        a
    })
    list(`@context` = ctx, `@graph` = atoms)                                                  # JSON-LD doc
}

project_payload_file_to_jsonld <- function(payload_json_path, context_path, out_jsonld_path, gzip = TRUE) {
    atoms <- read_json_any(payload_json_path)                                                 # read payload array
    if (!is.list(atoms)) stop("Payload is not a JSON array: ", payload_json_path)             # check
    doc <- payload_to_jsonld(atoms, context_path)                                             # make jsonld
    write_json_any(doc, out_jsonld_path, gzip = gzip)                                         # write
    invisible(out_jsonld_path)                                                                # return path
}


# ============================================================
# RUN
# ============================================================

# 1) Build payloads (skip if you already have them and do not want to overwrite)
combine_atoms_to_payload(rest_in, rest_out, pattern = "\\.json$", gzip = TRUE)              # rest payload
combine_atoms_to_payload(bout_in, bout_out, pattern = "\\.json$", gzip = TRUE)              # bout payload

# 2) Project payloads to JSON-LD using context.jsonld
project_payload_file_to_jsonld(rest_out, context_fp, rest_outld, gzip = TRUE)              # rest jsonld
project_payload_file_to_jsonld(bout_out, context_fp, bout_outld, gzip = TRUE)              # bout jsonld

cat("Done\n")
cat("Rest JSON-LD: ", rest_outld, "\n")
cat("Bout JSON-LD: ", bout_outld, "\n")

# doc <- jsonlite::fromJSON(rest_outld, simplifyVector = FALSE)
# atoms <- doc[["@graph"]]
# sum(vapply(atoms, function(a) is.null(a[["@id"]]) || !nzchar(as.character(a[["@id"]])), logical(1)))
#
# i <- which(vapply(atoms, function(a) is.null(a[["@id"]]) || !nzchar(as.character(a[["@id"]])), logical(1)))[1]
# atoms[[i]]$Header$AtomID
