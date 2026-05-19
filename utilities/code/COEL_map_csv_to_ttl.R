# COEL mapping CSV to Turtle
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require(c("readr", "glue", "dplyr", "jsonlite", "stringr", "tools"))

# Mapping CSV -> TTL (SKOS triples)
build_mapping_ttl <- function(map_csv,
                              src_base,                      # e.g. https://w3id.org/coel/models/activinsights/behavioural_bout/1.0
                              coel_base,                     # e.g. https://w3id.org/coel/models/coel/2.0
                              out_ttl_path,
                              mapping_iri = NULL,
                              mapping_label = "COEL mapping",
                              default_pred = "skos:broadMatch") {
    
    m <- readr::read_csv(map_csv, col_types = cols(.default = col_character()))
    stopifnot(all(c("label","coel_code") %in% names(m)))
    
    if (!("mapping_type" %in% names(m))) m$mapping_type <- default_pred
    
    norm_pred <- function(x) {
        x <- trimws(x)
        x[x == "skos:broader"] <- "skos:broadMatch"
        x[x == "skos:related"] <- "skos:relatedMatch"
        x[!nzchar(x) | is.na(x)] <- default_pred
        x
    }
    m$mapping_type <- norm_pred(m$mapping_type)
    
    src_iri  <- paste0(src_base,  "#", m$label)
    coel_iri <- paste0(coel_base, "#", m$coel_code)
    
    keep <- nzchar(m$label) & nzchar(m$coel_code) & !is.na(m$label) & !is.na(m$coel_code)
    m <- m[keep, , drop = FALSE]
    src_iri  <- src_iri[keep]
    coel_iri <- coel_iri[keep]
    
    if (is.null(mapping_iri)) mapping_iri <- paste0(src_base, "/mapping-to-coel")
    
    ttl <- c(
        "@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
        "@prefix owl:  <http://www.w3.org/2002/07/owl#> .",
        "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
        "@prefix skos: <http://www.w3.org/2004/02/skos/core#> .",
        "",
        paste0("<", mapping_iri, "> a owl:Ontology ;"),
        paste0("  rdfs:label \"", mapping_label, "\" ."),
        ""
    )
    
    for (i in seq_len(nrow(m))) {
        ttl <- c(ttl,
                 paste0("<", src_iri[i], "> ", m$mapping_type[i], " <", coel_iri[i], "> .")
        )
    }
    
    writeLines(ttl, out_ttl_path, useBytes = TRUE)
    invisible(out_ttl_path)
}

repo_root <- coel_repo_root()

build_mapping_ttl(
    map_csv      = file.path(repo_root, "mapping", "behavioural-bout-model-v1.0-to-coel-model-v2.0.csv"),
    src_base     = "https://w3id.org/coel/models/activinsights/behavioural_bout/1.0",
    coel_base    = "https://w3id.org/coel/models/coel/2.0",
    out_ttl_path = file.path(repo_root, "mapping", "behavioural-bout-model-v1.0-to-coel-model-v2.0.ttl"),
    mapping_label = "Behavioural Bout Model v1.0 to COEL Model v2.0 mapping"
)

build_mapping_ttl(
    map_csv      = file.path(repo_root, "mapping", "rest-activity-model-v1.0-to-coel-model-v2.0.csv"),
    src_base     = "https://w3id.org/coel/models/activinsights/rest_activity/1.0",
    coel_base    = "https://w3id.org/coel/models/coel/2.0",
    out_ttl_path = file.path(repo_root, "mapping", "rest-activity-model-v1.0-to-coel-model-v2.0.ttl"),
    mapping_label = "Rest Activity Model v1.0 to COEL Model v2.0 mapping"
)
