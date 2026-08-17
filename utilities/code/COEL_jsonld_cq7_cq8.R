# CQ7/CQ8 JSON-LD, SKOS mapping and SPARQL analysis.

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)
coel_require(c("jsonlite", "rdflib", "ggplot2"))

semantic_atom_rows <- function(jsonld_path, participant_id, window_seconds) {
    graph <- read_json_any(jsonld_path)[["@graph"]]
    pid <- vapply(graph, atom_pid, character(1)); start <- vapply(graph, atom_start, numeric(1))
    duration <- vapply(graph, atom_duration, numeric(1)); keep_pid <- which(pid == participant_id)
    if (!length(keep_pid)) stop("Participant absent from JSON-LD input", call. = FALSE)
    window_start <- min(start[keep_pid], na.rm = TRUE); window_end <- window_start + window_seconds
    keep <- which(pid == participant_id & start < window_end & start + duration > window_start)
    data.frame(atom_iri = vapply(graph[keep], function(a) as.character(a[["@id"]]), character(1)),
        start = start[keep], duration = duration[keep],
        source_iri = vapply(graph[keep], function(a) {
            iris <- as.character(unlist(get_in(a, c("What", "LabelIRI"), character(0)), use.names = FALSE))
            iris <- iris[!grepl("/models/coel/", iris, fixed = TRUE)]
            if (length(iris)) iris[1] else NA_character_
        }, character(1)), stringsAsFactors = FALSE)
}

mapping_triples <- function(mapping_path, source_registry_path, coel_registry_path) {
    mapping <- read.csv(mapping_path, stringsAsFactors = FALSE, check.names = FALSE)
    source <- read.csv(source_registry_path, stringsAsFactors = FALSE, check.names = FALSE)
    coel <- read.csv(coel_registry_path, stringsAsFactors = FALSE, check.names = FALSE)
    names(mapping) <- tolower(names(mapping)); names(source) <- tolower(names(source)); names(coel) <- tolower(names(coel))
    triples <- merge(mapping, source[, c("label", "iri")], by = "label", all.x = TRUE)
    names(triples)[names(triples) == "iri"] <- "source_iri"
    coel_lookup <- setNames(coel$iri, coel$label)
    triples$coel_iri <- unname(coel_lookup[as.character(triples$coel_code)])
    valid_types <- c("skos:exactMatch", "skos:closeMatch", "skos:broadMatch", "skos:relatedMatch")
    triples <- triples[triples$mapping_type %in% valid_types & !is.na(triples$source_iri) & !is.na(triples$coel_iri), ]
    triples[, c("source_iri", "coel_iri", "mapping_type")]
}

cq7_query <- function() paste(
    "PREFIX dcterms: <http://purl.org/dc/terms/>",
    "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>",
    "SELECT ?src (SUM(?d) AS ?sumdur)",
    "WHERE { { SELECT ?src (xsd:integer(?dur) AS ?d)",
    "WHERE { ?atom dcterms:extent ?dur ; dcterms:subject ?src . } } }",
    "GROUP BY ?src", sep = "\n")

cq8_query <- function() paste(
    "PREFIX dcterms: <http://purl.org/dc/terms/>",
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>",
    "SELECT ?coel (SUM(?d) AS ?sumdur)",
    "WHERE { { SELECT ?coel (xsd:integer(?dur) AS ?d) WHERE {",
    "?atom dcterms:extent ?dur ; dcterms:subject ?src .",
    "{ ?src skos:exactMatch ?coel } UNION { ?src skos:closeMatch ?coel } UNION",
    "{ ?src skos:broadMatch ?coel } UNION { ?src skos:relatedMatch ?coel } } } }",
    "GROUP BY ?coel", sep = "\n")

run_semantic_stream <- function(stream, config) {
    rows <- semantic_atom_rows(stream$jsonld, config$participant_id, config$window_seconds)
    triples <- mapping_triples(stream$mapping, stream$registry, config$coel_registry)
    observed_iris <- setNames(unique(rows$source_iri), tolower(unique(rows$source_iri)))
    replacement <- unname(observed_iris[tolower(triples$source_iri)])
    triples$source_iri[!is.na(replacement)] <- replacement[!is.na(replacement)]
    graph <- rdflib::rdf()
    for (i in seq_len(nrow(rows))) {
        graph <- rdflib::rdf_add(graph, rows$atom_iri[i], "http://purl.org/dc/terms/extent",
            as.character(rows$duration[i]), datatype = "http://www.w3.org/2001/XMLSchema#integer")
        graph <- rdflib::rdf_add(graph, rows$atom_iri[i], "http://purl.org/dc/terms/subject", rows$source_iri[i])
    }
    skos <- "http://www.w3.org/2004/02/skos/core#"
    for (i in seq_len(nrow(triples))) {
        predicate <- paste0(skos, sub("^skos:", "", triples$mapping_type[i]))
        graph <- rdflib::rdf_add(graph, triples$source_iri[i], predicate, triples$coel_iri[i])
    }
    q7 <- cq7_query(); q8 <- cq8_query()
    native <- rdflib::rdf_query(graph, q7); mapped <- rdflib::rdf_query(graph, q8)
    native$duration_s <- as.numeric(as.character(native$sumdur)); mapped$duration_s <- as.numeric(as.character(mapped$sumdur))
    source <- read.csv(stream$registry, stringsAsFactors = FALSE)
    native$label <- source$label[match(tolower(native$src), tolower(source$iri))]
    native$label[is.na(native$label) | !nzchar(native$label)] <- sub("^.*#", "", native$src[is.na(native$label) | !nzchar(native$label)])
    coel <- read.csv(config$coel_registry, stringsAsFactors = FALSE); mapped$label <- coel$Name[match(mapped$coel, coel$iri)]
    native$duration_h <- native$duration_s / 3600; mapped$duration_h <- mapped$duration_s / 3600
    native <- native[order(native$duration_s, decreasing = TRUE), ]; mapped <- mapped[order(mapped$duration_s, decreasing = TRUE), ]
    out <- file.path(config$results_root, "CQ8", stream$key); ensure_dir(out)
    writeLines(q7, file.path(out, "cq7_semantic.rq")); writeLines(q8, file.path(out, "cq8_mapped.rq"))
    write_csv(native, file.path(out, "CQ7_semantic_full.csv")); write_csv(mapped, file.path(out, "CQ8_mapped_full.csv"))
    rdflib::rdf_serialize(graph, file.path(out, "kg.ttl"), format = "turtle")
    plot_one <- function(data, title, path) {
        plot_data <- head(data, 12)
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = stats::reorder(label, duration_h), y = duration_h)) +
            ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::theme_bw() +
            ggplot2::labs(x = NULL, y = "Duration (hours)", title = title)
        ggplot2::ggsave(path, p, width = 8, height = 5, dpi = 300)
    }
    plot_one(native, paste0(stream$display, ": CQ7 native labels"), file.path(out, "CQ7_semantic.png"))
    plot_one(mapped, paste0(stream$display, ": CQ8 COEL roll-up"), file.path(out, "CQ8_mapped.png"))
    list(native = native, mapped = mapped)
}

run_semantic_cqs <- function(config) lapply(config$streams, run_semantic_stream, config = config)
