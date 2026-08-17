# Analysis code

Run `reproduce.R` to reproduce the manuscript analyses. Its modules are:

- `analysis_config.R`: fixed inputs, participant, window, threshold, seeds and expected validation metrics;
- `COEL_Atoms_to_jsonld.R`: compressed JSON to derived JSON-LD projection;
- `COEL_payload_validation.R`: CQ1-CQ5 injection, validation and scoring;
- `COEL_payload_data_summary.R`: payload and mapping summary tables;
- `COEL_temporal_retreiver_summariser_CQ6_CQ7.R`: CQ6 interval-overlap retrieval and CQ7 duration summaries;
- `COEL_jsonld_cq7_cq8.R`: CQ7 native-label and CQ8 SKOS/SPARQL aggregation;
- `coel_utilities.R`: shared path, compressed JSON and Atom helpers.

The upstream data-conversion utilities are:

- `COEL_bin_to_Atoms.R`: GENEAcore events to classified behavioural bouts;
- `COEL_json_builder.R`: classified events to COEL Atom JSON and compressed payloads;
- `COEL_model_csv_to_json.R`: canonical model CSV to JSON;
- `COEL_model_csv_to_ttl.R`: canonical model CSV to OWL Turtle;
- `COEL_map_csv_to_ttl.R`: mapping CSV to SKOS Turtle.

All paths are resolved from the repository root. The scripts do not require uncompressed copies of either payload and do not write into the canonical input directories.
