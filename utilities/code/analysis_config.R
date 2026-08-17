# Fixed configuration used to reproduce the manuscript analyses.

coel_analysis_config <- function(repo_root = coel_repo_root()) {
    build_root <- file.path(repo_root, "utilities", "build")
    payload_root <- file.path(repo_root, "utilities", "data", "atoms_payload")
    list(
        repo_root = repo_root,
        build_root = build_root,
        results_root = file.path(build_root, "research_paper_results"),
        intermediate_root = file.path(build_root, "intermediate"),
        reference_root = file.path(repo_root, "utilities", "data", "research_paper_results"),
        context = file.path(repo_root, "utilities", "jsonld", "context.jsonld"),
        participant_id = "]%@v55o:ae!l01jSGV.(",
        window_seconds = 48 * 60 * 60,
        gap_threshold_seconds = 30 * 60,
        seed = 1L,
        sample_percent_min = 10L,
        sample_percent_max = 30L,
        write_seeded_payloads = identical(tolower(Sys.getenv("COEL_WRITE_SEEDED_PAYLOADS", "false")), "true"),
        streams = list(
            rest_activity = list(key = "rest_activity", display = "Rest-activity stream",
                payload = file.path(payload_root, "rest_activity", "coel_atoms_payload.json.gz"),
                jsonld = file.path(build_root, "intermediate", "rest_activity", "coel_atoms_payload.jsonld.gz"),
                registry = file.path(repo_root, "models", "activinsights", "rest_activity", "1.0", "rest-activity-model-v1.0.csv"),
                mapping = file.path(repo_root, "mapping", "rest-activity-model-v1.0-to-coel-model-v2.0.csv")),
            behavioural_bout = list(key = "behavioural_bout", display = "Behavioural Bout stream",
                payload = file.path(payload_root, "behavioural_bout", "coel_atoms_payload.json.gz"),
                jsonld = file.path(build_root, "intermediate", "behavioural_bout", "coel_atoms_payload.jsonld.gz"),
                registry = file.path(repo_root, "models", "activinsights", "behavioural_bout", "1.0", "behavioural-bout-model-v1.0.csv"),
                mapping = file.path(repo_root, "mapping", "behavioural-bout-model-v1.0-to-coel-model-v2.0.csv"))
        ),
        coel_registry = file.path(repo_root, "models", "coel", "2.0", "coel-model-v2.0.csv"),
        expected_f1 = data.frame(stream = rep(c("rest_activity", "behavioural_bout"), each = 5),
            cq = rep(paste0("CQ", 1:5), 2),
            expected_f1 = c(1, 1, 0.424242, 1, 1, 1, 1, 0.308506, 1, 1), stringsAsFactors = FALSE)
    )
}
