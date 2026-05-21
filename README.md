# COEL

This repository hosts the **COEL v2.0 ecosystem**, a versioned collection of machine-readable artefacts for representing behavioural event data in interoperable, provenance-aware formats. It includes behaviour classification model registries, Atom schema resources, mapping files, optional semantic projection resources, and reusable utility code.

## Relationship to COEL v1.0

**COEL v1.0** is an OASIS standard. This repository builds on concepts introduced in that standard but contains additional project-specific artefacts, implementation resources, and extensions developed for the COEL v2.0 ecosystem.

This repository is independent of OASIS and is not an OASIS publication.

## Main entry points

- **Root namespace:** `https://w3id.org/coel/`
- **Expected GitHub Pages landing page:** `https://miltheo.github.io/coel/`
- **COEL Behavioural Atom v2.0:** `https://w3id.org/coel/atom/2.0/`
- **COEL Model v2.0:** `https://w3id.org/coel/models/coel/2.0/`
- **Mappings:** `https://w3id.org/coel/mapping/`
- **JSON-LD projection resources:** `https://w3id.org/coel/utilities/jsonld/`

Additional persistent identifiers resolve through the repository folder structure and are documented within the corresponding subdirectory README files and `docs/`.

## Repository structure

- `atom/`
  Versioned Atom schema resources.

- `models/`
  Versioned behaviour classification model registries and derived serialisations.

- `mapping/`
  Mapping files linking implementation-specific models to COEL model concepts.

- `utilities/`
  Supporting code and JSON-LD projection resources.

- `docs/`
  GitHub Pages documentation and draft w3id redirect notes.

## Source policy

Classification model CSV registries are the canonical model artefacts. Derived JSON and Turtle serialisations should be regenerated from the CSV registries using the reusable utilities in `utilities/code/`.

The public release provides schemas, registries, mappings, derived serialisations, documentation, utilities, and a minimal synthetic Atom example. Private evaluation payloads, source-derived payload exports, generated validation outputs, and local application workspaces are not included as public release artefacts.

## Citation

If you use these resources, please cite the relevant versioned repository release. Where appropriate, also cite the OASIS COEL v1.0 standard.

## Licence

See `LICENSE`.

## Maintainers

- **Millen J. Theophilus** (`miltheo`)
- **Jia Ying Chua** (`ai-jyc`)

## Acknowledgements

This work builds on the COEL v1.0 standard and related community contributions. This repository is an independent implementation ecosystem resource and is not an OASIS publication.
