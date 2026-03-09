# COEL Web Application

Canonical IRI: `https://w3id.org/coel/app/`  
Live application: `https://...`

This folder contains the public resources for the COEL Web Application, a lightweight operational interface for working with COEL Behavioural Atom payloads.

The application supports routine upload, validation, integrity review, scoped inspection, summary generation, and optional JSON-LD projection of Atom payloads without requiring users to write code.

## Purpose

The COEL Web Application is a reference workflow implementation for the COEL v2.0 ecosystem. It is intended to support practical handling of COEL Behavioural Atom JSON payloads during validation, quality assurance, inspection, and semantic projection.

This application accompanies the broader COEL v2.0 repository and is being prepared as part of the published resource set for a future COEL v2.0 manuscript.

## Current functions

The current application supports:

- upload of COEL Atom payloads in `.json` or `.json.gz` format
- payload overview summaries for participants, labels, evidence types, and classification models
- structural validation against core Atom requirements
- label integrity checking against user-supplied model registries
- duplicate detection using full Atom content matching
- temporal validation for participant-level gaps and overlaps
- temporal scoping of participant-level event windows with tabular and plotted outputs
- per-day duration summary scoping with tabular and plotted outputs
- optional JSON-LD projection using a supplied or default `context.jsonld`

## Inputs

The application expects a payload containing a JSON array of COEL Behavioural Atoms.

Supported inputs include:

- Atom payloads in `.json` or `.json.gz` format
- an optional JSON-LD context file in `.json` or `.jsonld` format
- optional registry CSV sources for label integrity checking

## Typical workflow

1. Upload a COEL Atom payload.
2. Review the overview outputs to inspect coverage, participants, labels, evidence types, and models.
3. Run structural validation.
4. Run one or more integrity checks, including label integrity, duplicate detection, and temporal validation.
5. Use temporal scoping to inspect participant-level event windows.
6. Use per-day summary scoping to inspect daily label durations.
7. Optionally generate a JSON-LD projection for downstream semantic workflows.
8. Download tables, plots, or projected JSON-LD outputs as needed.

## Validation and integrity checks

Implemented checks currently include:

- **Structural validation** for required elements, required sub-elements, and expected field types
- **Label integrity** for comparison of `What.Label` values against the registry corresponding to each `How.ClassificationModel`
- **Duplicate detection** using canonical full-Atom matching
- **Temporal validation** for participant-level gaps and overlaps

These checks support routine operational review and quality assurance, but they do not replace the normative COEL specifications.

## Outputs

The application can produce and export:

- payload overview tables
- structural validation tables
- integrity review tables
- temporal scoping tables and plots
- per-day summary tables and plots
- JSON-LD payload exports

## Scope

The COEL Web Application demonstrates one practical implementation pathway for operationalising the COEL artefact ecosystem. It is intended as a reference environment for validation, inspection, and semantic projection of Atom payloads. It does not constrain the upstream evidence source or the downstream execution environment.

## Related resources

Related resources in the COEL repository include:

- COEL Behavioural Atom resources: `https://w3id.org/coel/atom/2.0/`
- COEL Model resources: `https://w3id.org/coel/models/coel/2.0/`
- mapping resources: `https://w3id.org/coel/mapping/`
- repository root: `https://w3id.org/coel/`

## Source code

The application source code is provided in this folder as part of the public COEL v2.0 implementation resource.