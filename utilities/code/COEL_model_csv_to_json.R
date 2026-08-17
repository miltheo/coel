# Convert canonical COEL model tables to JSON.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require(c("readr", "jsonlite"))

make_iri_fragment <- function(x) {
    fragment <- gsub("[^A-Za-z0-9]+", "_", trimws(x))
    gsub("^_+|_+$", "", fragment)
}

read_model_terms <- function(csv_path, base_iri) {
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

build_model_json <- function(csv_path,
                             base_iri,
                             model_name,
                             model_id,
                             version = "1.0.0",
                             out_json_path = NULL,
                             update_csv = TRUE) {
    terms <- read_model_terms(csv_path, base_iri)
    output_columns <- c("label", "parent_label", "description", "is_output", "iri")
    terms_out <- terms[output_columns]
    if (isTRUE(update_csv)) readr::write_csv(terms_out, csv_path, na = "NA")

    model_json <- list(
        modelName = model_name,
        modelId = model_id,
        version = version,
        baseIRI = base_iri,
        labelKey = "label",
        parentKey = "parent_label",
        isOutputKey = "is_output",
        terms = terms_out
    )
    if (is.null(out_json_path)) {
        stem <- tools::file_path_sans_ext(basename(csv_path))
        out_json_path <- file.path(dirname(csv_path), paste0(stem, ".json"))
    }
    dir.create(dirname(out_json_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(model_json, out_json_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
    invisible(out_json_path)
}

build_all_model_json <- function(repo_root = coel_repo_root()) {
    build_model_json(
        file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.csv"),
        "https://w3id.org/coel/models/activinsights/behavioural_bout/1.0",
        "Behavioural Bout Model v1.0",
        "behavioural-bout-model-v1.0",
        "1.0",
        file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.json")
    )
    build_model_json(
        file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.csv"),
        "https://w3id.org/coel/models/activinsights/rest_activity/1.0",
        "Rest Activity Model v1.0",
        "rest-activity-model-v1.0",
        "1.0",
        file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.json")
    )
    invisible(TRUE)
}

if (sys.nframe() == 0L) build_all_model_json()
