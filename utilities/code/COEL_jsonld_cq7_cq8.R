# COEL JSON-LD knowledge graph and mapping roll-up
# Author: Millen J. Theophilus
# GitHub: https://github.com/miltheo
# Licence: see repository LICENSE

helper_path <- file.path("utilities", "code", "coel_utilities.R")
if (!file.exists(helper_path)) helper_path <- "coel_utilities.R"
source(helper_path)

coel_require(c("jsonlite", "rdflib", "ggplot2", "grid", "gridExtra"))

# ----------------------------
# Inputs (edit)
# ----------------------------
stream_to_run <- "rest_activity"                                                        # rest_activity | behavioural_bout
participant_id <- "]%@v55o:ae!l01jSGV.("                                                # demo participant
repo_root <- coel_repo_root()
out_dir <- file.path(repo_root, "utilities", "data", "research_paper_results", "CQ8")

atoms_rest_jsonld_path <- file.path(repo_root, "utilities", "data", "atoms_payload", "rest_activity", "coel_atoms_payload.jsonld")
atoms_bout_jsonld_path <- file.path(repo_root, "utilities", "data", "atoms_payload", "behavioural_bout", "coel_atoms_payload.jsonld")

map_rest_path <- file.path(repo_root, "mapping", "rest-activity-model-v1.0-to-coel-model-v2.0.csv")
map_bout_path <- file.path(repo_root, "mapping", "behavioural-bout-model-v1.0-to-coel-model-v2.0.csv")

src_rest_registry_path <- file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.csv")
src_bout_registry_path <- file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.csv")
coel_registry_path     <- file.path(repo_root, "models", "coel", "2.0", "coel-model-v2.0.csv")

coel_ns <- "https://w3id.org/coel/models/coel/2.0"                                      # namespace root

# ----------------------------
# Utils
# ----------------------------
ensure_dir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE)           # mkdir
nz <- function(x) !is.na(x) & nzchar(x)                                                 # non-empty
stop_cols <- function(df, msg) stop(msg, " Columns: ", paste(names(df), collapse = ", "))# stop

detect_col <- function(nm, candidates) {                                                # choose 1st matching
    hit <- nm[nm %in% candidates]
    if (!length(hit)) NA_character_ else hit[1]
}

# okabe_ito <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#000000")
# okabe_map <- function(labs) setNames(rep(okabe_ito, length.out = length(labs)), labs)

pal_semantic <- c(
  NonWear   = "#000000",
  Sleep     = "#0072B2",
  Asleep    = "#0072B2",
  Rest      = "#E69F00",
  Sedentary = "#E69F00",
  Active    = "#009E73",
  SlowWalk  = "#56B4E9",
  FastWalk  = "#56B4E9",
  Walking   = "#56B4E9",
  Run       = "#D55E00"
)

order_native <- function(d) {
  if (stream_to_run == "behavioural_bout") {
    ord <- c("NonWear","Sleep","Sedentary","Active","SlowWalk","FastWalk","Run")
  } else {
    ord <- c("NonWear","Rest","Active")
  }
  d$label <- as.character(d$label)
  core <- ord[ord %in% d$label]
  extra <- setdiff(unique(d$label), ord)
  lev <- c(core, extra)
  d$label <- factor(d$label, levels = lev)
  d[order(d$label), , drop = FALSE]
}

order_rollup <- function(d) {
  if (stream_to_run == "behavioural_bout") {
    ord <- c("Asleep","Sedentary","Active","Walking","Running","NonWear")
  } else {
    ord <- c("Asleep","Active")
  }
  d$label <- as.character(d$label)
  core <- ord[ord %in% d$label]
  extra <- setdiff(unique(d$label), ord)
  lev <- c(core, extra)
  d$label <- factor(d$label, levels = lev)
  d[order(d$label), , drop = FALSE]
}

# ----------------------------
# Atom parsing (minimal + robust)
# ----------------------------
atoms_to_df <- function(atoms_jsonld_path, stream) {                                      # atoms -> df
    doc <- jsonlite::fromJSON(atoms_jsonld_path, simplifyVector = FALSE)                    # read jsonld
    atoms <- doc[["@graph"]]                                                                # atoms
    if (!length(atoms)) return(data.frame())                                                # empty

    pid <- vapply(atoms, function(a) {                                                      # participant ids
        if (is.null(a$Who$ParticipantID)) return(NA_character_)
        as.character(a$Who$ParticipantID[[1]])
    }, character(1))

    keep <- which(pid == participant_id)                                                    # keep participant atoms
    if (!length(keep)) return(data.frame())                                                 # empty

    atom_iri <- vapply(atoms[keep], function(a) {                                           # Atom subject IRI
        x <- a[["@id"]]
        if (is.null(x)) NA_character_ else as.character(x)
    }, character(1))

    t0  <- vapply(atoms[keep], function(a) as.numeric(a$When$TimeUTC[[1]]), numeric(1))      # start utc
    dur <- vapply(atoms[keep], function(a) as.numeric(a$When$Duration[[1]]), numeric(1))     # duration

    iris <- lapply(atoms[keep], function(a) {                                               # label iris
      z <- as.character(unlist(a$What$LabelIRI, use.names = FALSE))
      z[nz(z)]
    })

    # ---- 48h window from first Atom (speeds up behavioural_bout) ----
    t1 <- t0 + dur                                                                            # end times
    w0 <- min(t0, na.rm = TRUE)                                                               # window start
    w1 <- w0 + 48 * 3600                                                                      # window end (48h)

    k <- which(t0 < w1 & t1 > w0)                                                             # overlap filter
    if (!length(k)) return(data.frame())

    atom_iri <- atom_iri[k]
    t0 <- t0[k]
    dur <- dur[k]
    iris <- iris[k]
    keep <- keep[k]

    data.frame(stream = stream, atom_i = seq_along(t0), atom_iri = atom_iri,
               t0 = t0, dur_s = dur, label_iris = I(iris),
               stringsAsFactors = FALSE)
}


# ----------------------------
# Registry readers (auto-detect)
# ----------------------------
read_source_label_to_iri <- function(path) {                                            # label->iri
    r <- read.csv(path, stringsAsFactors = FALSE)
    nm <- names(r)
    lab <- detect_col(nm, c("label"))
    iri <- detect_col(nm, c("iri"))
    if (is.na(lab) || is.na(iri)) stop_cols(r, "Source registry must contain label+iri-like columns.")
    keep <- nz(r[[lab]]) & nz(r[[iri]])
    setNames(as.character(r[[iri]][keep]), as.character(r[[lab]][keep]))
}

read_coel_code_to_iri <- function(path) {                                                  # code->iri
    r <- read.csv(path, stringsAsFactors = FALSE)
    keep <- nz(r[["label"]]) & nz(r[["iri"]])
    setNames(as.character(r[["iri"]][keep]), as.character(r[["label"]][keep]))
}

read_coel_iri_to_label <- function(path) {                                                 # iri -> human label
    r <- read.csv(path, stringsAsFactors = FALSE)
    keep <- nz(r[["iri"]]) & nz(r[["Name"]])
    setNames(as.character(r[["Name"]][keep]), as.character(r[["iri"]][keep]))
}

norm_map_type <- function(x) {                                                          # normalise predicate names
    x <- trimws(as.character(x))
    x[x == "skos:broader"] <- "skos:broadMatch"
    x[x == "skos:related"] <- "skos:relatedMatch"
    x
}

read_source_iri_to_label <- function(path) {                                              # iri -> label
    r <- read.csv(path, stringsAsFactors = FALSE)
    keep <- nz(r[["iri"]]) & nz(r[["label"]])
    setNames(as.character(r[["label"]][keep]), as.character(r[["iri"]][keep]))
}

resolve_mapping_triples <- function(map_path, src_reg_path, coel_reg_path,
                                    allowed = c("skos:exactMatch","skos:closeMatch","skos:broadMatch")) {
    m <- read.csv(map_path, stringsAsFactors = FALSE)
    nm <- names(m)
    lab_col  <- detect_col(nm, c("label"))
    code_col <- detect_col(nm, c("coel_code"))
    type_col <- detect_col(nm, c("mapping_type"))
    if (is.na(lab_col) || is.na(code_col)) stop_cols(m, "Mapping CSV must contain source label + COEL code columns.")
    map_type <- if (is.na(type_col)) rep("skos:closeMatch", nrow(m)) else m[[type_col]]
    map_type <- norm_map_type(map_type)

    src_map  <- read_source_label_to_iri(src_reg_path)
    coel_map <- read_coel_code_to_iri(coel_reg_path)

    src_iri <- unname(src_map[as.character(m[[lab_col]])])
    coel_iri <- unname(coel_map[as.character(m[[code_col]])])

    keep <- nz(src_iri) & nz(coel_iri) & map_type %in% allowed
    data.frame(source_iri = src_iri[keep], coel_iri = coel_iri[keep], mapping_type = map_type[keep],
               stringsAsFactors = FALSE)
}

clean_cq7 <- function(res_df) {
  if (is.null(res_df) || !nrow(res_df)) return(data.frame())
  if ("sumdur" %in% names(res_df)) names(res_df)[names(res_df) == "sumdur"] <- "duration_s"
  res_df$duration_s <- as.numeric(as.character(res_df$duration_s))
  res_df <- res_df[is.finite(res_df$duration_s) & res_df$duration_s > 0 & nz(res_df$src), , drop = FALSE]
  res_df
}

attach_source_labels <- function(res_df, iri_to_label) {                                  # add label + hours
    if (!nrow(res_df)) return(data.frame())
    d <- res_df
    d$duration_h <- d$duration_s / 3600
    d$label <- unname(iri_to_label[as.character(d$src)])
    d$label[is.na(d$label)] <- as.character(d$src)[is.na(d$label)]
    d[order(d$duration_h, decreasing = TRUE), , drop = FALSE]
}

coverage_text <- function(atoms_df) {
  if (!nrow(atoms_df)) return("")
  span_s <- (max(atoms_df$t0 + atoms_df$dur_s, na.rm = TRUE) - min(atoms_df$t0, na.rm = TRUE))
  paste0("Participant ", participant_id, ", first 48 h window; observed span ~", round(span_s / 3600, 1), " h")
}

plot_bar_console <- function(d, title, subtitle = NULL, top_n = 12, fill = NULL) {
    if (!nrow(d)) return(invisible(NULL))
    dd <- head(d, top_n)
    dd$label <- factor(dd$label, levels = dd$label)
    p <- ggplot(dd, aes(x = label, y = duration_h)) +
        geom_col(aes(fill = label), show.legend = FALSE) +
        coord_flip() + theme_bw() +
        labs(x = NULL, y = "Duration (hours)", title = title, subtitle = subtitle) +
        geom_text(aes(label = sprintf("%.1f", duration_h)), hjust = -0.05, size = 3) +
        expand_limits(y = max(dd$duration_h) * 1.10)
    if (!is.null(fill)) p <- p + scale_fill_manual(values = fill)
    p
}


# ----------------------------
# RDF build (minimal CQ8 graph)
# ----------------------------
NS <- list(
    rdf     = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    prov    = "http://www.w3.org/ns/prov#",
    dcterms = "http://purl.org/dc/terms/",
    skos    = "http://www.w3.org/2004/02/skos/core#",
    xsd     = "http://www.w3.org/2001/XMLSchema#"
)

pred_iri <- function(p) paste0(NS$skos, sub("^skos:", "", p))

add_atoms_to_rdf <- function(g, atoms_df) {
    if (!nrow(atoms_df)) return(g)
    for (i in seq_len(nrow(atoms_df))) {
        s <- atoms_df$atom_iri[i]
        rdf_add(g, s, paste0(NS$rdf, "type"), paste0(NS$prov, "Entity"))
        rdf_add(g, s, paste0(NS$dcterms, "extent"), as.character(atoms_df$dur_s[i]), datatype = paste0(NS$xsd, "integer"))
        rdf_add(g, s, paste0(NS$dcterms, "temporal"), as.character(atoms_df$t0[i]), datatype = paste0(NS$xsd, "integer"))
        srcs <- atoms_df$label_iris[[i]]
        srcs <- srcs[!startsWith(srcs, coel_ns)]
        if (length(srcs)) rdf_add(g, s, paste0(NS$dcterms, "subject"), srcs[1])
    }
    g
}

add_mapping_to_rdf <- function(g, triples_df) {
    if (is.null(triples_df) || !nrow(triples_df)) return(g)
    for (i in seq_len(nrow(triples_df))) rdf_add(g, triples_df$source_iri[i], pred_iri(triples_df$mapping_type[i]), triples_df$coel_iri[i])
    g
}


clean_cq8 <- function(res_df) {
    if (is.null(res_df) || !nrow(res_df)) return(data.frame())
    if ("sumdur" %in% names(res_df)) names(res_df)[names(res_df) == "sumdur"] <- "duration_s"
    if ("duration_s" %in% names(res_df)) res_df$duration_s <- as.numeric(as.character(res_df$duration_s))
    res_df <- res_df[is.finite(res_df$duration_s) & res_df$duration_s > 0 & nz(res_df$coel), , drop = FALSE]
    res_df
}

attach_labels <- function(res_df, coel_iri_to_label) {                                   # add label + hours
    if (!nrow(res_df)) return(data.frame())
    d <- res_df
    d$duration_h <- d$duration_s / 3600
    d$coel_label <- unname(coel_iri_to_label[as.character(d$coel)])
    d$coel_label[is.na(d$coel_label)] <- as.character(d$coel)[is.na(d$coel_label)]
    d[order(d$duration_h, decreasing = TRUE), , drop = FALSE]
}

plot_bar_mapped <- function(d, title, out_png, top_n = 12) {                             # mapped roll-up figure
    if (!nrow(d)) return(invisible(NULL))
    dd <- head(d, top_n)
    p <- ggplot(dd, aes(x = reorder(coel_label, duration_h), y = duration_h)) +
        geom_col() + coord_flip() + theme_bw() +
        labs(x = NULL, y = "Duration (hours)", title = title)
    ggsave(out_png, p, width = 8, height = 5, dpi = 600)
}

write_text <- function(x, path) writeLines(x, path, useBytes = TRUE)

# ----------------------------
# SPARQL (Redland-stable)
# ----------------------------

cq7_query_semantic <- function() {
  '
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX xsd:     <http://www.w3.org/2001/XMLSchema#>

SELECT ?src (SUM(?d) AS ?sumdur)
WHERE {
  {
    SELECT ?src (xsd:integer(?dur) AS ?d)
    WHERE {
      ?atom dcterms:extent ?dur .
      ?atom dcterms:subject ?src .
    }
  }
}
GROUP BY ?src
'
}

cq8_query_mapped <- function(preds = c("skos:exactMatch","skos:closeMatch","skos:broadMatch"), coel_ns = coel_ns) {
  preds <- unique(sub("^skos:", "", preds))
  blocks <- paste(sprintf("{ ?src skos:%s ?coel . }", preds), collapse = "\n  UNION\n  ")
  sprintf('
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
PREFIX xsd:     <http://www.w3.org/2001/XMLSchema#>

SELECT ?coel (SUM(?d) AS ?sumdur)
WHERE {
  {
    SELECT ?coel (xsd:integer(?dur) AS ?d)
    WHERE {
      ?atom dcterms:extent ?dur .
      ?atom dcterms:subject ?src .
      FILTER(!STRSTARTS(STR(?src), "%s")) .
      %s
    }
  }
}
GROUP BY ?coel
', coel_ns, blocks)
}


# ----------------------------
# Select per-stream inputs
# ----------------------------
if (!stream_to_run %in% c("rest_activity","behavioural_bout")) stop("stream_to_run must be rest_activity or behavioural_bout")

if (stream_to_run == "rest_activity") {
    stream_name <- "rest_activity"
    atoms_path  <- atoms_rest_jsonld_path
    map_path    <- map_rest_path
    src_reg     <- src_rest_registry_path
}

if (stream_to_run == "behavioural_bout") {
    stream_name <- "behavioural_bout"
    atoms_path  <- atoms_bout_jsonld_path
    map_path    <- map_bout_path
    src_reg     <- src_bout_registry_path
}

# ============================================================
# RUN (single stream)
# ============================================================

ensure_dir(out_dir)
stream_dir <- file.path(out_dir, stream_name)
ensure_dir(stream_dir)

coel_iri_to_label <- read_coel_iri_to_label(coel_registry_path)                           # iri->label

atoms_df <- atoms_to_df(atoms_path, stream_name)
if (!nrow(atoms_df)) stop("No atoms found for participant in payload: ", stream_name)

stopifnot(nrow(atoms_df) > 0, all(nz(atoms_df$atom_iri)))


map_triples <- resolve_mapping_triples(map_path, src_reg, coel_registry_path)
if (!nrow(map_triples)) stop("No mapping triples resolved. Check mapping CSV + registries for: ", stream_name)

g <- rdf()
g <- add_atoms_to_rdf(g, atoms_df)
g <- add_mapping_to_rdf(g, map_triples)

# ---- CQ7 semantic (native source labels) ----
src_iri_to_label <- read_source_iri_to_label(src_reg)                                     # iri -> label (stream-specific)

q_cq7 <- cq7_query_semantic()                                                             # query
write_text(q_cq7, file.path(stream_dir, "cq7_semantic.rq"))                               # save query for appendix

res_cq7 <- clean_cq7(rdf_query(g, q_cq7))                                                 # run + clean
cq7_l <- attach_source_labels(res_cq7, src_iri_to_label)                                  # add labels

write.csv(cq7_l, file.path(stream_dir, "CQ7_semantic_full.csv"), row.names = FALSE)       # save table
write.csv(head(cq7_l, 12), file.path(stream_dir, "CQ7_semantic_top.csv"), row.names = FALSE)


rdf_serialize(g, file.path(stream_dir, "kg.ttl"), format = "turtle")                      # KG artifact

q_mapped <- cq8_query_mapped(c("skos:exactMatch","skos:closeMatch","skos:broadMatch"), coel_ns)

write_text(q_mapped, file.path(stream_dir, "cq8_mapped.rq"))

res_map <- clean_cq8(rdf_query(g, q_mapped))

res_raw <- rdf_query(g, q_mapped)
print(names(res_raw))
print(head(res_raw, 3))

map_l <- attach_labels(res_map, coel_iri_to_label)

# Plot -------------------------------------
subtxt <- coverage_text(atoms_df)

cq7_plot_df <- order_native(cq7_l)
p_cq7 <- plot_bar_console(
  cq7_plot_df,
  paste0(stream_name, ": CQ7 native labels (SPARQL)"),
  subtitle = subtxt,
  top_n = 12,
  fill = pal_semantic
)
print(p_cq7)

cq8_plot_df <- order_rollup(transform(map_l, label = coel_label, duration_h = duration_h))
p_cq8 <- plot_bar_console(
  cq8_plot_df,
  paste0(stream_name, ": CQ8 roll-up to COEL (SKOS + SPARQL)"),
  subtitle = subtxt,
  top_n = 12,
  fill = pal_semantic
)
print(p_cq8)

#--------------------------------------------

write.csv(map_l, file.path(stream_dir, "CQ8_mapped_full.csv"), row.names = FALSE)
# write.csv(dir_l, file.path(stream_dir, "CQ8_direct_full.csv"), row.names = FALSE)
write.csv(head(map_l, 12), file.path(stream_dir, "CQ8_top_mapped.csv"), row.names = FALSE)

plot_bar_mapped(map_l, paste0(stream_name, ": CQ8 roll-up via SKOS + SPARQL"),
                file.path(stream_dir, "CQ8_mapped_bar.png"), top_n = 12)

png(file.path(stream_dir, "CQ7_CQ8_native_vs_rollup.png"), width = 10, height = 8, units = "in", res = 600, pointsize = 12)
gridExtra::grid.arrange(p_cq7, p_cq8, ncol = 1)
dev.off()

cat("Done. Stream artifacts written to:\n", stream_dir, "\n")
cat("Now restart R, switch stream_to_run, and re-run for the other stream.\n")
