# COEL Utility Code

Author: [Millen J. Theophilus](https://github.com/miltheo)

This folder contains reproducible R utilities used with the COEL v2.0 artefacts.

## Scripts

- [COEL_model_csv_to_json.R](COEL_model_csv_to_json.R) converts model registry CSV files into derived JSON serialisations.
- [COEL_model_csv_to_ttl.R](COEL_model_csv_to_ttl.R) converts model registry CSV files into derived Turtle serialisations.
- [COEL_map_csv_to_ttl.R](COEL_map_csv_to_ttl.R) converts mapping CSV files into derived SKOS/Turtle mapping files.
- [COEL_Atoms_to_jsonld.R](COEL_Atoms_to_jsonld.R) combines Atom JSON files and projects payloads to JSON-LD.
- [COEL_payload_validation.R](COEL_payload_validation.R) runs schema validation and synthetic-token error-detection checks over Atom payloads.
- [COEL_jsonld_cq7_cq8.R](COEL_jsonld_cq7_cq8.R) builds RDF graph and mapping-based aggregation outputs.
- [COEL_payload_data_summary.R](COEL_payload_data_summary.R) creates stream and mapping coverage summary tables.
- [COEL_temporal_retreiver_summariser_CQ6_CQ7.R](COEL_temporal_retreiver_summariser_CQ6_CQ7.R) provides temporal retrieval and summary helpers.
- [COEL_json_builder.R](COEL_json_builder.R) builds COEL Behavioural Atom JSON from classified event data.
- [COEL_bin_to_Atoms.R](COEL_bin_to_Atoms.R) contains GENEAcore-dependent preparation helpers for local source data.

## Running

Run scripts from the repository root unless a script-specific workflow states otherwise:

```r
source("utilities/code/COEL_model_csv_to_ttl.R")
```

The scripts do not install packages automatically. Install the listed R package dependencies before running a script.

`COEL_bin_to_Atoms.R` depends on GENEAcore and local source data that are not distributed in this repository.
