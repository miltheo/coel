# COEL model CSV to JSON
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require(c("readr", "dplyr", "jsonlite", "stringr", "tools"))

## Helper: mint safe IRI fragments from labels ---------------------------------
make_fragment <- function(x) {
  x %>%
    stringr::str_trim() %>%
    # normalise spaces and punctuation to underscores
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    # remove leading / trailing underscores
    stringr::str_replace_all("^_+|_+$", "")
}

## Generic function: CSV -> JSON model -----------------------------------------
build_model_json <- function(csv_path,
                             base_iri,
                             model_name,
                             model_id,
                             version = "1.0.0",
                             out_json_path = NULL) {

  terms <- readr::read_csv(
    csv_path,
    col_types = cols(
      label        = col_character(),
      parent_label = col_character(),
      description   = col_character(),
      is_output    = col_logical()
    )
  )

  # Preserve existing stable IRIs when the canonical CSV already has an iri column.
  terms <- terms %>%
    dplyr::mutate(
      iri_fragment = make_fragment(label),
      iri = if ("iri" %in% names(terms)) {
        dplyr::if_else(is.na(.data$iri) | .data$iri == "", paste0(base_iri, "#", iri_fragment), .data$iri)
      } else {
        paste0(base_iri, "#", iri_fragment)
      }
    )

  # Check for collisions
  if (any(duplicated(terms$iri_fragment))) {
    dup <- terms$label[duplicated(terms$iri_fragment) |
                         duplicated(terms$iri_fragment, fromLast = TRUE)]
    stop("Duplicate IRI fragments derived from labels: ",
         paste(unique(dup), collapse = ", "))
  }

  # Write updated CSV with iri column (canonical + iri)
  terms_for_csv <- terms %>%
    dplyr::select(label, parent_label, description, is_output, iri)
  readr::write_csv(terms_for_csv, csv_path)

  # Wrap into a canonical JSON structure
  model_json <- list(
    modelName    = model_name,
    modelId      = model_id,
    version      = version,
    baseIRI      = base_iri,
    labelKey     = "label",
    parentKey    = "parent_label",
    isOutputKey  = "is_output",
    terms        = terms %>%
      dplyr::select(label, parent_label, description, is_output, iri)
  )

  if (is.null(out_json_path)) {
    stem <- tools::file_path_sans_ext(basename(csv_path))
    out_json_path <- file.path(dirname(csv_path), paste0(stem, ".json"))
  }

  jsonlite::write_json(
    model_json,
    out_json_path,
    pretty     = TRUE,
    auto_unbox = TRUE
  )

  invisible(out_json_path)
}


repo_root <- coel_repo_root()

build_model_json(
  csv_path      = file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.csv"),
  base_iri      = "https://w3id.org/coel/models/activinsights/behavioural_bout/1.0",
  model_name    = "Behavioural Bout Model v1.0",
  model_id      = "behavioural-bout-model-v1.0",
  version       = "1.0",
  out_json_path = file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.json")
)

build_model_json(
  csv_path      = file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.csv"),
  base_iri      = "https://w3id.org/coel/models/activinsights/rest_activity/1.0",
  model_name    = "Rest Activity Model v1.0",
  model_id      = "rest-activity-model-v1.0",
  version       = "1.0",
  out_json_path = file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.json")
)
