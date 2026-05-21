# COEL model CSV to Turtle
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require(c("readr", "glue", "dplyr", "jsonlite", "stringr", "tools"))

# Helper: mint safe IRI fragments from labels ----------------------------------
make_fragment <- function(x) {
  x %>%
    str_trim() %>%
    # replace any non-alphanumeric run with underscore
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    # remove leading/trailing underscores
    str_replace_all("^_+|_+$", "")
}

# Generic CSV -> TTL ontology --------------------------------------------------
build_model_ontology <- function(csv_path,
                                 base_iri,
                                 ontology_iri = base_iri,
                                 out_ttl_path = "model_ontology.ttl",
                                 model_name = NULL,
                                 prefix = NULL) {

  # 1. Read canonical CSV ------------------------------------------------
  terms <- readr::read_csv(
    csv_path,
    col_types = cols(
      label        = col_character(),
      parent_label = col_character(),
      description   = col_character(),
      is_output    = col_logical()
    )
  )

  if (is.null(model_name)) {
    model_name <- tools::file_path_sans_ext(basename(csv_path))
  }

  # 2. Preserve existing stable IRIs, minting only if the column is absent.
  terms <- terms %>%
    mutate(
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

  # 2b. Write IRIs back into the CSV (overwrite / update) ----------------
  # This keeps the original columns and adds/updates the 'iri' column.
  terms_for_csv <- terms %>%
    select(label, parent_label, description, is_output, iri)

  readr::write_csv(terms_for_csv, csv_path)

  # Map label -> fragment for parent lookup
  frag_by_label <- setNames(terms$iri_fragment, terms$label)

  # Prefix handling ------------------------------------------------------
  if (is.null(prefix)) {
    prefix_line    <- glue("@prefix : <{base_iri}#> .")
    ns_prefix      <- ":"                      # used as ":Fragment"
    ann_prop_qname <- ":isDirectOutputLabel"
  } else {
    prefix_line    <- glue("@prefix {prefix}: <{base_iri}#> .")
    ns_prefix      <- paste0(prefix, ":")      # e.g. "geneabout:"
    ann_prop_qname <- paste0(prefix, ":isDirectOutputLabel")
  }

  # 3. TTL header --------------------------------------------------------
  ttl <- c(
    prefix_line,
    "@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
    "@prefix owl:  <http://www.w3.org/2002/07/owl#> .",
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
    "@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .",
    "",
    glue("<{ontology_iri}> rdf:type owl:Ontology ."),
    glue("<{ontology_iri}> rdfs:label \"{model_name}\" ."),
    "",
    "# Annotation property to flag direct model outputs",
    glue("{ann_prop_qname} a owl:AnnotationProperty ."),
    ""
  )

  esc <- function(x) stringr::str_replace_all(x, "\"", "\\\\\"")

  # 4. One class per label ----------------------------------------------
  for (i in seq_len(nrow(terms))) {
    row <- terms[i, ]

    # QName for this class, e.g. geneabout:Behaviour or :Behaviour
    class_qname <- glue("{ns_prefix}{row$iri_fragment}")
    lab <- esc(row$label)

    lines <- c(
      glue("{class_qname} a owl:Class ;"),
      glue("    rdfs:label \"{lab}\" ;")
    )

    # optional description as rdfs:comment
    if (!is.na(row$description) && nzchar(row$description)) {
      lines <- c(lines, glue("    rdfs:comment \"{esc(row$description)}\" ;"))
    }

    # optional is_output flag
    if (!is.na(row$is_output)) {
      bool_str <- if (isTRUE(row$is_output)) "true" else "false"
      lines <- c(
        lines,
        glue("    {ann_prop_qname} \"{bool_str}\"^^xsd:boolean ;")
      )
    }

    # parent relationship
    if (!is.na(row$parent_label)) {
      parent_frag   <- frag_by_label[[row$parent_label]]
      if (is.na(parent_frag)) {
        stop("Parent label '", row$parent_label,
             "' not found among labels in ", csv_path)
      }
      parent_qname <- glue("{ns_prefix}{parent_frag}")
      lines <- c(
        lines,
        glue("    rdfs:subClassOf {parent_qname} .")
      )
    } else {
      # no parent_label: root class
      lines[length(lines)] <- sub(" ;$", " .", lines[length(lines)])
    }

    ttl <- c(ttl, lines, "")
  }

  writeLines(ttl, out_ttl_path)
  invisible(out_ttl_path)
}

repo_root <- coel_repo_root()

build_model_ontology(
  csv_path     = file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.csv"),
  base_iri     = "https://w3id.org/coel/models/activinsights/behavioural_bout/1.0",
  ontology_iri = "https://w3id.org/coel/models/activinsights/behavioural_bout/1.0",
  out_ttl_path = file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.ttl"),
  model_name   = "Behavioural Bout Model v1.0",
  prefix       = ""
)

build_model_ontology(
  csv_path     = file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.csv"),
  base_iri     = "https://w3id.org/coel/models/activinsights/rest_activity/1.0",
  ontology_iri = "https://w3id.org/coel/models/activinsights/rest_activity/1.0",
  out_ttl_path = file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.ttl"),
  model_name   = "Rest Activity Model v1.0",
  prefix       = ""
)
