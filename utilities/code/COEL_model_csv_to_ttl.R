# Convert canonical COEL model tables to OWL Turtle.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require("readr")

if (!exists("make_iri_fragment", mode = "function")) {
    make_iri_fragment <- function(x) {
        fragment <- gsub("[^A-Za-z0-9]+", "_", trimws(x))
        gsub("^_+|_+$", "", fragment)
    }
}

escape_turtle_literal <- function(x) {
    x <- gsub("\\\\", "\\\\\\\\", x, fixed = TRUE)
    x <- gsub("\"", "\\\\\"", x, fixed = TRUE)
    x <- gsub("\r", "\\\\r", x, fixed = TRUE)
    gsub("\n", "\\\\n", x, fixed = TRUE)
}

read_ontology_terms <- function(csv_path, base_iri) {
    terms <- readr::read_csv(
        csv_path,
        col_types = readr::cols(
            label = readr::col_character(),
            parent_label = readr::col_character(),
            description = readr::col_character(),
            is_output = readr::col_logical(),
            iri = readr::col_character()
        ),
        show_col_types = FALSE
    )
    required <- c("label", "parent_label", "description", "is_output")
    missing_columns <- setdiff(required, names(terms))
    if (length(missing_columns)) {
        stop("Missing model column(s): ", paste(missing_columns, collapse = ", "))
    }

    terms$iri_fragment <- make_iri_fragment(terms$label)
    duplicate_fragments <- duplicated(terms$iri_fragment) |
        duplicated(terms$iri_fragment, fromLast = TRUE)
    if (any(duplicate_fragments)) {
        stop(
            "Duplicate IRI fragments derived from labels: ",
            paste(unique(terms$label[duplicate_fragments]), collapse = ", ")
        )
    }
    if (!"iri" %in% names(terms)) terms$iri <- NA_character_
    missing_iri <- is.na(terms$iri) | !nzchar(terms$iri)
    terms$iri[missing_iri] <- paste0(base_iri, "#", terms$iri_fragment[missing_iri])
    terms
}

build_model_ontology <- function(csv_path,
                                 base_iri,
                                 ontology_iri = base_iri,
                                 out_ttl_path = "model_ontology.ttl",
                                 model_name = NULL,
                                 prefix = NULL,
                                 update_csv = TRUE) {
    terms <- read_ontology_terms(csv_path, base_iri)
    if (is.null(model_name)) model_name <- tools::file_path_sans_ext(basename(csv_path))
    if (isTRUE(update_csv)) {
        readr::write_csv(
            terms[c("label", "parent_label", "description", "is_output", "iri")],
            csv_path,
            na = "NA"
        )
    }

    fragment_by_label <- setNames(terms$iri_fragment, terms$label)
    if (is.null(prefix) || !nzchar(prefix)) {
        prefix_line <- paste0("@prefix : <", base_iri, "#> .")
        namespace <- ":"
    } else {
        prefix_line <- paste0("@prefix ", prefix, ": <", base_iri, "#> .")
        namespace <- paste0(prefix, ":")
    }
    output_property <- paste0(namespace, "isDirectOutputLabel")

    ttl <- c(
        prefix_line,
        "@prefix owl:  <http://www.w3.org/2002/07/owl#> .",
        "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
        "@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .",
        "",
        paste0("<", ontology_iri, "> a owl:Ontology ;"),
        paste0("    rdfs:label \"", escape_turtle_literal(model_name), "\" ."),
        "",
        paste0(output_property, " a owl:AnnotationProperty ."),
        ""
    )

    for (i in seq_len(nrow(terms))) {
        statements <- c(
            paste0("    rdfs:label \"", escape_turtle_literal(terms$label[i]), "\""),
            if (!is.na(terms$description[i]) && nzchar(terms$description[i])) {
                paste0("    rdfs:comment \"", escape_turtle_literal(terms$description[i]), "\"")
            },
            if (!is.na(terms$is_output[i])) {
                paste0(output_property, " \"", tolower(as.character(terms$is_output[i])), "\"^^xsd:boolean")
            }
        )
        parent_label <- terms$parent_label[i]
        if (!is.na(parent_label) && nzchar(parent_label)) {
            parent_fragment <- unname(fragment_by_label[parent_label])
            if (is.na(parent_fragment)) {
                stop("Parent label '", parent_label, "' is not defined in ", csv_path)
            }
            statements <- c(statements, paste0("    rdfs:subClassOf ", namespace, parent_fragment))
        }

        class_line <- paste0(namespace, terms$iri_fragment[i], " a owl:Class ;")
        statement_lines <- paste0(
            statements,
            c(rep(" ;", length(statements) - 1L), " .")
        )
        ttl <- c(ttl, class_line, statement_lines, "")
    }

    dir.create(dirname(out_ttl_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(ttl, out_ttl_path, useBytes = TRUE)
    invisible(out_ttl_path)
}

build_all_model_ontologies <- function(repo_root = coel_repo_root()) {
    build_model_ontology(
        file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.csv"),
        "https://w3id.org/coel/models/activinsights/behavioural_bout/1.0",
        out_ttl_path = file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.ttl"),
        model_name = "Behavioural Bout Model v1.0"
    )
    build_model_ontology(
        file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.csv"),
        "https://w3id.org/coel/models/activinsights/rest_activity/1.0",
        out_ttl_path = file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.ttl"),
        model_name = "Rest Activity Model v1.0"
    )
    invisible(TRUE)
}

if (sys.nframe() == 0L) build_all_model_ontologies()
