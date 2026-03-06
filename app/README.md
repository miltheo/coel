# COEL web application

Canonical IRI: `https://w3id.org/coel/app/`

Live application: `https://...`

This folder contains the public resources for the COEL Web Application.

## Purpose
The application provides an operational interface for upload, validation, integrity review, scoped inspection, summary export, and optional JSON-LD projection of COEL Behavioural Atom payloads.

## What the application does
The COEL web application supports routine inspection and handling of COEL Behavioural Atom payloads without requiring users to write code. It is intended as a lightweight operational environment for working with Atom JSON files during validation, quality assurance, and downstream inspection.

Current functions include:
- upload of COEL Atom payloads in `.json` or `.json.gz` format
- structural validation against core Atom requirements
- interoperability checking of labels against user-supplied model registries
- duplicate detection using full Atom content matching
- temporal integrity checks for gaps and overlaps
- temporal scoping of participant-level event windows
- per-day duration summaries and visual inspection
- optional JSON-LD projection using a supplied or default `context.jsonld`

## Inputs
The application expects a payload containing a JSON array of COEL Behavioural Atoms.

Supported inputs:
- Atom payload: `.json` or `.json.gz`
- Optional JSON-LD context: `.json` or `.jsonld`
- Optional registry files for label integrity checks: local CSV file paths or GitHub file URLs

## How to use the application
1. Upload a COEL Atom payload in `.json` or `.json.gz` format.
2. Review the **Overview** tab to inspect payload coverage, participants, labels, evidence types, and classification models.
3. Run **Structural validation** to check required elements, sub-elements, and field types.
4. If classification models are present, provide the relevant registry files and run **Label integrity**.
5. Run **Duplicate validation** and **Temporal validation** as routine quality assurance checks.
6. Use **Temporal scoping** to inspect a selected participant within a user-defined UTC time window.
7. Use **Per-day summary scoping** to generate daily summaries and stacked plots.
8. Optionally upload a `context.jsonld`, or use the default context, to create a JSON-LD projection of the payload.
9. Download any retrieved tables, plots, validation outputs, or projected JSON-LD files as needed.

## Validation and integrity checks
The application is designed for routine payload-level quality assurance.

Implemented checks currently include:
- **Structural validation**: required elements, required sub-elements, and expected field types
- **Label integrity**: comparison of `What.Label` values against the registry corresponding to each `How.ClassificationModel`
- **Duplicate detection**: full Atom duplicate identification using canonical JSON matching
- **Temporal validation**: detection of participant-level gaps and overlaps

These checks support routine review but do not replace the normative COEL specifications.

## Temporal scoping and summaries
The application includes two main inspection utilities:
- **Temporal scoping**, which retrieves and visualises Atoms overlapping a specified participant-level time window
- **Per-day summary scoping**, which aggregates Atom durations by day and label for quick inspection and export

These functions are intended for operational review, payload inspection, and lightweight downstream analysis.

## JSON-LD projection
The application can generate a JSON-LD projection of an uploaded Atom payload using:
- a user-supplied `context.jsonld`, or
- a default `context.jsonld` available in the application environment

This projection is provided to support downstream semantic tooling and linked-data workflows. The normative exchange format remains JSON.

## Outputs
The application can produce and export:
- validation tables
- integrity review tables
- temporal scoping tables
- temporal scoping plots
- per-day summary tables
- per-day summary plots
- JSON-LD payload exports

## Scope
The COEL web application is a reference implementation for operationalising the COEL artefact bundle. It is intended to demonstrate one practical workflow for validation, inspection, and semantic projection of Atom payloads. It does not constrain the upstream evidence source or the downstream execution environment.

## Related resources
Related COEL resources are available in the broader repository, including:
- COEL Behavioural Atom specification
- model registries
- mapping files
- JSON-LD context resources
- ontology serialisations
- release and citation information