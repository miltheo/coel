# COEL

This repository hosts working artefacts for an extended COEL (Classification of Everyday Living) ecosystem, including versioned Atom schema resources, behaviour classification models, and crosswalk mappings. The goal is to support interoperable representation of behavioural event data across research and commercial workflows, with clear versioning and machine readable artefacts.

## Relationship to the COEL v1.0 standard

COEL (Classification of Everyday Living) v1.0 is an OASIS standard. This repository builds on the concepts and structure introduced in that standard and contains additional, project specific artefacts and draft extensions. Nothing in this repository should be interpreted as an official OASIS publication unless explicitly stated and versioned as such.

If you use COEL in academic or commercial work, please cite the official COEL v1.0 standard and associated OASIS materials.

## Repository structure

- `atom/`  
  Versioned Atom resources, including JSON Schema and JSON-LD context files.

- `models/`  
  Versioned behaviour classification models in CSV, JSON, and TTL, plus a `registry.csv` describing available models.

- `mapping/`  
  Crosswalk mappings between external models and COEL model versions.

## Status and scope

This repository is under active development and is intended to support internal review and iterative refinement. File names and folder paths are being standardised to support resolvable identifiers and long term maintenance.

## Licence

See `LICENSE`.

## Maintainers

- Millen J. Theophilus (GitHub: `miltheo`)
- Joss Langford (Activinsights) (GitHub: `<add-handle>`)
- JIa Ying (Activinsights) (GitHub: `<add-handle>`)

Maintainer list and organisational crediting will be finalised after internal review.

## Acknowledgements

This work is informed by the OASIS COEL v1.0 standard and related community contributions. COEL is a trademark and standard maintained by OASIS and its contributors. This repository is not affiliated with or endorsed by OASIS unless explicitly stated.


