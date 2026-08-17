#!/usr/bin/env Rscript
# One-command reproduction of all manuscript analyses from the two canonical payloads.

script_args <- commandArgs(trailingOnly = FALSE)
script_flag <- grep("^--file=", script_args, value = TRUE)
script_path <- if (length(script_flag)) sub("^--file=", "", script_flag[1]) else file.path("utilities", "code", "reproduce.R")
code_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))

source(file.path(code_dir, "coel_utilities.R"))
coel_use_local_library()
coel_set_utf8_locale()
coel_require(c("jsonlite", "rdflib", "ggplot2"))
source(file.path(code_dir, "analysis_config.R"))
source(file.path(code_dir, "COEL_Atoms_to_jsonld.R"))
source(file.path(code_dir, "COEL_payload_validation.R"))
source(file.path(code_dir, "COEL_payload_data_summary.R"))
source(file.path(code_dir, "COEL_temporal_retreiver_summariser_CQ6_CQ7.R"))
source(file.path(code_dir, "COEL_jsonld_cq7_cq8.R"))

config <- coel_analysis_config()
ensure_dir(config$results_root); ensure_dir(config$intermediate_root)

message("[1/6] Verifying canonical compressed payloads")
inputs <- vapply(config$streams, function(stream) stream$payload, character(1))
missing <- inputs[!file.exists(inputs)]; if (length(missing)) stop("Missing canonical input(s): ", paste(missing, collapse = ", "))
relative_inputs <- sub(paste0(normalizePath(config$repo_root, winslash = "/"), "/"), "",
    normalizePath(inputs, winslash = "/"), fixed = TRUE)
input_manifest <- data.frame(stream = names(inputs), file = relative_inputs, bytes_gzip = file.info(inputs)$size,
    bytes_json = vapply(inputs, gzip_uncompressed_size, numeric(1)), md5 = unname(tools::md5sum(inputs)), stringsAsFactors = FALSE)
write_csv(input_manifest, file.path(config$results_root, "input_manifest.csv"))

message("[2/6] Deriving JSON-LD from compressed JSON")
force_jsonld <- identical(tolower(Sys.getenv("COEL_REBUILD_JSONLD", "false")), "true")
for (stream in config$streams) {
    current <- file.exists(stream$jsonld) && file.info(stream$jsonld)$size > 0 &&
        file.info(stream$jsonld)$mtime >= max(file.info(stream$payload)$mtime, file.info(config$context)$mtime)
    if (force_jsonld || !current) project_payload_file_to_jsonld(stream$payload, config$context, stream$jsonld)
    else message("      Reusing current derived JSON-LD: ", stream$key)
}

message("[3/6] Reproducing payload and mapping summary tables")
summaries <- run_payload_summaries(config)

message("[4/6] Reproducing CQ1-CQ5 validation experiments")
metrics <- run_validation_cqs(config)

message("[5/6] Reproducing CQ6-CQ8 retrieval, summaries and semantic roll-up")
temporal <- run_temporal_cqs(config)
semantic <- run_semantic_cqs(config)

message("[6/6] Verifying regenerated measurements")
comparison <- merge(metrics[, c("stream", "cq", "F1")], config$expected_f1, by = c("stream", "cq"), all.x = TRUE)
comparison$difference <- comparison$F1 - comparison$expected_f1
if (any(abs(comparison$difference) >= 5e-4)) stop("Regenerated validation metrics differ from the configured expected values.")

session <- capture.output(sessionInfo())
writeLines(session, file.path(config$results_root, "sessionInfo.txt"))
message("Reproduction complete: ", config$results_root)
