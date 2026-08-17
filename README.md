# COEL

This repository hosts the **COEL v2.0 public artefact bundle** for representing behavioural event data. It includes the COEL Behavioural Atom v2.0 JSON Schema, behaviour classification model registries, mapping files, derived serialisations, optional projection resources, documentation, and supporting utilities.

Start from the live public namespace: [https://w3id.org/coel/](https://w3id.org/coel/).

## Relationship to COEL v1.0

**COEL v1.0** is an OASIS standard. This repository builds on concepts introduced in that standard but contains additional project-specific artefacts, implementation resources, and extensions developed for the COEL v2.0 ecosystem.

This repository is independent of OASIS and is not an OASIS publication.

## Main entry point

- **Root namespace:** [https://w3id.org/coel/](https://w3id.org/coel/)

Detailed documentation, artefact routes, and versioned resources are linked from the namespace pages.

## Repository structure

- [atom/](atom/)
  Versioned Atom schema resources.

- [models/](models/)
  Versioned behaviour classification model registries and derived serialisations.

- [mapping/](mapping/)
  Mapping files linking implementation-specific models to COEL model concepts.

- [utilities/](utilities/)
  Supporting code and JSON-LD projection resources.

- [docs/](docs/)
  GitHub Pages documentation and w3id namespace summary.

## Source policy

Classification model CSV registries are the source files for model terms and stable term IRIs. Derived JSON and Turtle serialisations should be regenerated from the CSV registries using the reusable utilities in `utilities/code/`.

The public release provides schemas, registries, mappings, derived serialisations, documentation, utilities, and a minimal synthetic Atom example. The Semantic Web Journal replication package is contained entirely in `utilities/`: its two compressed canonical payloads are under `utilities/data/atoms_payload/`, compact reference results are under `utilities/data/research_paper_results/`, and the one-command pipeline is `utilities/code/reproduce.R`. Generated validation payloads, derived build files, and local application workspaces remain excluded from the public release.

## Citation

If you use these resources, please cite the relevant versioned repository release. Where appropriate, also cite the OASIS COEL v1.0 standard.

## Licence

See [LICENSE](LICENSE).

## Maintainers

- **Millen J. Theophilus** (`miltheo`)

## Acknowledgements

This work builds on the COEL v1.0 standard and related community contributions. This repository is an independent implementation ecosystem resource and is not an OASIS publication.
