# Project a canonical compressed COEL Atom payload to derived JSON-LD.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)
coel_require("jsonlite")

mint_urn <- function(kind, value) paste0("urn:coel:", kind, ":", utils::URLencode(as.character(value), reserved = TRUE))

payload_to_jsonld <- function(atoms, context_path) {
    context_document <- read_json_any(context_path)
    context <- context_document[["@context"]] %||% context_document
    graph <- lapply(atoms, function(a) {
        aid <- atom_id(a); pid <- atom_pid(a)
        evidence_id <- as.character(get_in(a, c("How", "EvidenceID"), NA_character_))[1]
        start <- atom_start(a); duration <- atom_duration(a)
        if (!is.na(aid) && nzchar(aid)) a[["@id"]] <- mint_urn("atom", aid)
        if (!is.na(pid) && nzchar(pid)) a$Who[["@id"]] <- mint_urn("participant", pid)
        if (!is.na(evidence_id) && nzchar(evidence_id)) a$How[["@id"]] <- mint_urn("evidence", evidence_id)
        if (is.finite(start) && is.finite(duration)) a$When$EndTimeISO <- utc_iso(start + duration)
        if (!is.null(a$What$LabelIRI)) a$What$LabelIRI <- unname(as.list(unlist(a$What$LabelIRI, use.names = FALSE)))
        a
    })
    list(`@context` = context, `@graph` = graph)
}

project_payload_file_to_jsonld <- function(payload_path, context_path, output_path) {
    atoms <- read_json_any(payload_path)
    if (!is.list(atoms) || !length(atoms)) stop("Payload is not a non-empty JSON array: ", payload_path, call. = FALSE)
    ids <- vapply(atoms, atom_id, character(1))
    duplicates <- unique(ids[!is.na(ids) & duplicated(ids)])
    if (length(duplicates)) stop("Duplicate AtomID in canonical payload: ", duplicates[1], call. = FALSE)
    write_json_any(payload_to_jsonld(atoms, context_path), output_path, pretty = FALSE)
}
