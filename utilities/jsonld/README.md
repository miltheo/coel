# COEL JSON-LD Projection Resources

Canonical IRI: `https://w3id.org/coel/utilities/jsonld/`

This folder contains JSON-LD projection resources for the COEL v2.0 ecosystem, including the study `context.jsonld` used to project normative COEL Behavioural Atom JSON payloads into linked-data compatible form.

These resources are provided as part of the published companion artefact set for a future COEL v2.0 manuscript.

## Purpose

The resources in this folder support optional JSON-LD projection of normative COEL Atom v2.0 JSON payloads. The projection is intended for semantic export, RDF construction, and query-based workflows, while preserving JSON as the normative exchange format.

## Included artefacts

- `context.jsonld`  
  Minimal JSON-LD context file used in the accompanying study.

## What the app projection does

The COEL Web Application performs a lightweight JSON-LD projection rather than a bespoke field-by-field semantic transformation.

Specifically, the application:

- preserves the original Atom JSON structure
- assigns a top-level `@id` to each Atom during export
- wraps the payload in a JSON-LD document containing an `@context` and an `@graph`
- delegates semantic interpretation to the selected context file

The application therefore supports **context-pluggable projection**, but not fully context-agnostic semantic remapping.

## What a compatible context file must assume

A user-supplied context file can be used when it is structurally compatible with the normative COEL Atom JSON and with the fields present in the uploaded payload.

A compatible context file should therefore:

- use only fields defined in the normative COEL Atom JSON structure
- respect the existing nesting of Atom elements such as `Header`, `When`, `What`, `Who`, and optional `How`
- correctly specify which fields are literals, typed literals, IRIs, or sets
- preserve the optionality of blocks that may be absent in valid Atom payloads
- target only fields that are actually present in the payload when specific semantic outputs are required

Structural validation of the payload is therefore necessary, but not alone sufficient, for intended semantic output. The context file must also be compatible with the payload structure and relevant fields.

## What the study context file does

The study `context.jsonld` implements a minimal JSON-LD projection designed for RDF export. Cross-model semantic aggregation additionally requires separate SKOS mapping resources and downstream RDF/SPARQL processing code.

It uses scoped contexts for selected Atom blocks and maps fields to published vocabulary terms from:

- DCMI Metadata Terms
- PROV-O
- XML Schema Definition Language
- SKOS vocabulary prefixes for downstream mapping workflows

In the study context:

- `Header.AtomID` maps to `dcterms:identifier`
- `Header.AtomIRI` maps to `dcterms:conformsTo`
- `When.TimeUTC` maps to `dcterms:temporal`
- `When.Duration` maps to `dcterms:extent`
- `When.UTCOffset` maps to `dcterms:spatial`
- `What.LabelIRI` maps to `dcterms:subject`
- `Who.ParticipantID` and `Who.EnvironmentID` map to `dcterms:references`
- `How` maps to `prov:wasGeneratedBy` as a `prov:Activity`
- `How.ClassificationModelIRI` maps to `prov:used`
- `How.AssessmentTimeISO` maps to `prov:endedAtTime`

This context does not itself mint RDF subject identifiers. Atom-level `@id` values are assigned by the exporting application.

## Notes for users creating their own context file

Users may supply their own context file in the app, but the resulting JSON-LD will only behave as intended when the context matches the normative Atom structure and the available payload fields.

A valid context file can support RDF projection of a structurally valid payload, but downstream query tasks such as native-label summarisation or cross-model roll-up may require additional payload fields or external mapping resources.

In particular:

- a structurally valid payload may still be semantically unsuitable for a given context if the expected fields are absent
- a context designed for `LabelIRI`-based semantic joins will require those IRIs to be present in the payload
- a context expecting `How` fields for provenance will only project those triples when the `How` block exists

## Study workflow implementation

The JSON-LD projection resources in this folder were used in the accompanying study together with separate mapping resources and downstream RDF/SPARQL code.

In the study workflow:

- the COEL Web Application projected Atom payloads to JSON-LD using `context.jsonld`
- separate SKOS mapping resources linked source model concept IRIs to COEL concept IRIs
- a downstream RDF/SPARQL workflow then built the knowledge graph, executed native-label summaries, and performed CQ8-style roll-up to COEL concepts

The application therefore implements the projection step, whereas the full CQ7/CQ8 study workflow additionally depends on the mapping resources and downstream code.

Related study implementation resources include:

- mapping resources in `https://w3id.org/coel/mapping/`
- downstream RDF/SPARQL workflow code in `utilities/code/`

## Related resources

- Study context file: `https://w3id.org/coel/utilities/jsonld/context.jsonld`
- COEL Web Application: `https://w3id.org/coel/app/`
- COEL Behavioural Atom v2.0: `https://w3id.org/coel/atom/2.0/`
- COEL mapping resources: `https://w3id.org/coel/mapping/`
- Repository root: `https://w3id.org/coel/`