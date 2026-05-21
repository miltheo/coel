# Public Release Checklist

Use this checklist before tagging a release, enabling GitHub Pages as the public landing page, and submitting or updating `w3id.org/coel` redirects.

## Repository Hygiene

- [ ] Confirm there are no private data, local-only files, credentials, deployment records, or personal paths in tracked release files.
- [ ] Confirm local interactive workspaces are ignored and excluded from the public COEL v2.0 release unless published separately.
- [ ] Confirm private evaluation payloads, source-derived payload exports, and generated validation outputs are excluded from tracked release files.
- [ ] Confirm all public links in `README.md`, subdirectory READMEs, and `docs/` resolve or intentionally point to planned release targets.
- [ ] Confirm the repository licence and citation guidance are current.

## Artefact Consistency

- [ ] Confirm COEL Model v2.0 terms use stable IRIs from `models/coel/2.0/coel-model-v2.0.csv`.
- [ ] Confirm Behavioural Bout Model v1.0 terms use stable IRIs from `models/activinsights/behavioural_bout/1.0/behavioural-bout-model-v1.0.csv`.
- [ ] Confirm Rest Activity Model v1.0 terms use stable IRIs from `models/activinsights/rest_activity/1.0/rest-activity-model-v1.0.csv`.
- [ ] Check that `Behavioural Bout Model` is used consistently and that no public text drops `al` from `Behavioural`.
- [ ] Check all uses of `behavioural_bout`, `behavioural-bout`, and any legacy `behaviour_bout` slug. Keep stable IRIs unchanged unless the affected files and rows are reviewed first.
- [ ] Confirm model JSON and Turtle files are treated as derived serialisations, not independent canonical sources.

## Validation

- [ ] Confirm the COEL Behavioural Atom v2.0 JSON Schema at `atom/2.0/coel-atom.json` matches the release specification.
- [ ] Validate the synthetic minimal Atom example against the COEL Behavioural Atom v2.0 JSON Schema.
- [ ] Confirm `utilities/jsonld/context.jsonld` expands representative Atom payloads successfully.
- [ ] Confirm mapping files use valid source labels and target COEL codes from the corresponding registry CSVs.
- [ ] Confirm mapping-based aggregation outputs match expected COEL Model v2.0 rollups.

## Derived Artefacts

- [ ] Run the CSV-to-ontology conversion script for model registries and confirm generated Turtle outputs are reproducible.
- [ ] Run the mapping CSV-to-Turtle conversion script and confirm generated mapping Turtle outputs are reproducible.
- [ ] Confirm derived serialisations do not introduce IRIs absent from the canonical CSV registries.
- [ ] Update or remove generated files that are stale relative to their source CSVs.

## Pages and Redirects

- [ ] Enable GitHub Pages from `docs/` on the intended branch and confirm the landing page builds successfully.
- [ ] Confirm the final GitHub Pages URL before using it as a stable w3id landing target.
- [ ] Test all internal `docs/` links from the deployed Pages site.
- [ ] Submit or update the `w3id.org/coel` redirect configuration using temporary `302` redirects for testing.
- [ ] Confirm all registry IRIs resolve to canonical CSV artefacts or have planned redirects documented in `docs/w3id-redirects.md`.
- [ ] Test specific artefact redirects for model registries, mapping CSVs, the extension registry, the JSON-LD context, and the Atom schema.
- [ ] Remove or document temporary compatibility aliases before final release.

## Archiving

- [ ] Prepare a Zenodo release with the final tagged repository state.
- [ ] Confirm Zenodo metadata, creators, licence, keywords, and citation text.
- [ ] Add the Zenodo DOI to repository documentation after it is minted.
