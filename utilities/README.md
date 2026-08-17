# Manuscript reproducibility utilities

This directory contains the complete replication package for the associated research paper (under submission). It deliberately keeps manuscript-specific material inside `utilities/`.

## Canonical inputs

Only these two immutable inputs are required:

- `data/atoms_payload/rest_activity/coel_atoms_payload.json.gz`
- `data/atoms_payload/behavioural_bout/coel_atoms_payload.json.gz`

JSON-LD, seeded validation payloads, figures, query artifacts and regenerated tables are derived from those inputs. They are written to the ignored `utilities/build/` directory so a run cannot overwrite the reference results in `utilities/data/research_paper_results/`.

## Reproduce the analysis

From the repository root:

```text
R -e "install.packages('renv'); renv::restore(project = 'utilities')"
Rscript utilities/code/reproduce.R
```

The run performs the following stages:

1. verifies and records the compressed inputs;
2. derives manuscript-compatible JSON-LD;
3. regenerates the payload and mapping tables;
4. reruns CQ1-CQ5 with fixed seeds and a 30-minute gap threshold;
5. reruns the 48-hour CQ6-CQ8 analyses for the fixed manuscript participant;
6. verifies the regenerated validation metrics and records `sessionInfo()`.

Set `COEL_WRITE_SEEDED_PAYLOADS=true` only when the large intermediate seeded payloads are needed for debugging. These files remain ignored by Git.

Set `COEL_REBUILD_JSONLD=true` to force regeneration of an existing derived JSON-LD file. By default, a current build artifact is reused when it is newer than both its compressed payload and the JSON-LD context.

## Outputs

- `data/research_paper_results/` contains the compact reference results included in the archived release.
- `build/research_paper_results/` contains the results of a local run and is ignored by Git.
