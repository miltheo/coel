# COEL

This repository hosts the **COEL v2.0 ecosystem**, a versioned collection of machine-readable artefacts for representing behavioural event data in interoperable, provenance-aware formats. It includes behaviour classification models, Atom schema resources, mapping files, semantic projection resources, and a Shiny web application.

This repository is being prepared as a published companion resource to a **future COEL v2.0 manuscript**. It should therefore be treated as a versioned research and implementation resource.

## Relationship to COEL v1.0

**COEL v1.0** is an official OASIS standard. This repository builds on concepts introduced in that standard but contains additional project-specific artefacts, implementation resources, and draft extensions developed for the COEL v2.0 ecosystem.

Nothing in this repository should be interpreted as an official OASIS publication unless explicitly stated.

## Main entry points

- **Root namespace:** `https://w3id.org/coel/`
- **Shiny app:** `https://w3id.org/coel/app/`
- **COEL Behavioural Atom v2.0:** `https://w3id.org/coel/atom/2.0/`
- **COEL Model v2.0:** `https://w3id.org/coel/models/coel/2.0/`
- **Mappings:** `https://w3id.org/coel/mapping/`

Additional persistent identifiers resolve through the repository folder structure and are documented within the corresponding subdirectory README files.

## Repository structure

- `app/`  
  Shiny application resources and source code.

- `atom/`  
  Versioned Atom schema resources.

- `models/`  
  Versioned behaviour classification model resources.

- `mapping/`  
  Mapping files linking external models to COEL model concepts.

- `utilities/`  
  Supporting code, example data, and JSON-LD file used in the accompanying study.

## Citation

If you use these resources, please cite the relevant versioned repository release and the associated **future COEL v2.0 publication** once available. Where appropriate, also cite the official OASIS COEL v1.0 standard.

## Licence

See `LICENSE`.

## Maintainers

- **Millen J. Theophilus** (`miltheo`)
- **Jia Ying Chua** (`ai-jyc`)

## Acknowledgements

This work builds on the COEL v1.0 standard and related community contributions. This repository is an implementation and research ecosystem resource and is not an official OASIS publication unless explicitly stated.