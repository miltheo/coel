# Convert COEL mapping tables to SKOS Turtle statements.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require("readr")

escape_turtle_string <- function(x) {
    x <- gsub("\\\\", "\\\\\\\\", x, fixed = TRUE)
    x <- gsub("\"", "\\\\\"", x, fixed = TRUE)
    x <- gsub("\r", "\\\\r", x, fixed = TRUE)
    gsub("\n", "\\\\n", x, fixed = TRUE)
}

build_mapping_ttl <- function(map_csv,
                              src_base,
                              coel_base,
                              out_ttl_path,
                              mapping_iri = NULL,
                              mapping_label = "COEL mapping",
                              default_pred = "skos:broadMatch") {
    mapping <- readr::read_csv(
        map_csv,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE
    )
    required <- c("label", "coel_code")
    missing_columns <- setdiff(required, names(mapping))
    if (length(missing_columns)) {
        stop("Missing mapping column(s): ", paste(missing_columns, collapse = ", "))
    }

    if (!"mapping_type" %in% names(mapping)) mapping$mapping_type <- default_pred
    mapping$mapping_type <- trimws(mapping$mapping_type)
    mapping$mapping_type[mapping$mapping_type == "skos:broader"] <- "skos:broadMatch"
    mapping$mapping_type[mapping$mapping_type == "skos:related"] <- "skos:relatedMatch"
    missing_type <- is.na(mapping$mapping_type) | !nzchar(mapping$mapping_type)
    mapping$mapping_type[missing_type] <- default_pred

    allowed_types <- paste0(
        "skos:",
        c("exactMatch", "closeMatch", "broadMatch", "narrowMatch", "relatedMatch")
    )
    invalid_types <- setdiff(unique(mapping$mapping_type), allowed_types)
    if (length(invalid_types)) {
        stop("Unsupported mapping_type value(s): ", paste(invalid_types, collapse = ", "))
    }

    keep <- !is.na(mapping$label) & nzchar(mapping$label) &
        !is.na(mapping$coel_code) & nzchar(mapping$coel_code)
    mapping <- mapping[keep, , drop = FALSE]
    if (is.null(mapping_iri)) mapping_iri <- paste0(src_base, "/mapping-to-coel")

    ttl <- c(
        "@prefix owl:  <http://www.w3.org/2002/07/owl#> .",
        "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
        "@prefix skos: <http://www.w3.org/2004/02/skos/core#> .",
        "",
        paste0("<", mapping_iri, "> a owl:Ontology ;"),
        paste0("  rdfs:label \"", escape_turtle_string(mapping_label), "\" ."),
        ""
    )
    for (i in seq_len(nrow(mapping))) {
        ttl <- c(
            ttl,
            paste0(
                "<", src_base, "#", mapping$label[i], "> ",
                mapping$mapping_type[i],
                " <", coel_base, "#", mapping$coel_code[i], "> ."
            )
        )
    }

    dir.create(dirname(out_ttl_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(ttl, out_ttl_path, useBytes = TRUE)
    invisible(out_ttl_path)
}

build_all_mapping_ttl <- function(repo_root = coel_repo_root()) {
    build_mapping_ttl(
        file.path(repo_root, "mapping", "behavioural-bout-model-v1.0-to-coel-model-v2.0.csv"),
        "https://w3id.org/coel/models/activinsights/behavioural_bout/1.0",
        "https://w3id.org/coel/models/coel/2.0",
        file.path(repo_root, "mapping", "behavioural-bout-model-v1.0-to-coel-model-v2.0.ttl"),
        mapping_label = "Behavioural Bout Model v1.0 to COEL Model v2.0 mapping"
    )
    build_mapping_ttl(
        file.path(repo_root, "mapping", "rest-activity-model-v1.0-to-coel-model-v2.0.csv"),
        "https://w3id.org/coel/models/activinsights/rest_activity/1.0",
        "https://w3id.org/coel/models/coel/2.0",
        file.path(repo_root, "mapping", "rest-activity-model-v1.0-to-coel-model-v2.0.ttl"),
        mapping_label = "Rest Activity Model v1.0 to COEL Model v2.0 mapping"
    )
    invisible(TRUE)
}

if (sys.nframe() == 0L) build_all_mapping_ttl()
