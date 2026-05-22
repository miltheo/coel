# COEL JSON-LD Projection Resources

IRI: [https://w3id.org/coel/utilities/jsonld/](https://w3id.org/coel/utilities/jsonld/)

This folder contains JSON-LD projection resources for the COEL v2.0 ecosystem.

## Purpose

The resources in this folder support optional JSON-LD projection of valid COEL Behavioural Atom v2.0 JSON payloads. The projection is intended for semantic export, RDF construction, and query-based workflows, while preserving JSON as the Atom exchange format.

## Included artefacts

- [context.jsonld](context.jsonld)
  JSON-LD context file for COEL Behavioural Atom v2.0 projection.

## Context assumptions

A compatible context file should:

- use fields defined in the COEL Atom JSON structure
- respect the nesting of Atom elements such as `Header`, `When`, `What`, `Who`, and optional `How`
- specify which fields are literals, typed literals, IRIs, or sets
- preserve the optionality of blocks that may be absent in valid Atom payloads
- target only fields that are present in the payload when specific semantic outputs are required

Structural validation of the payload is necessary, but not alone sufficient, for intended semantic output. The context file must also be compatible with the payload structure and relevant fields.

## Published context

The published `context.jsonld` implements a minimal JSON-LD projection designed for RDF export. Cross-model semantic aggregation additionally requires mapping resources and downstream RDF/SPARQL processing code.

It uses scoped contexts for selected Atom blocks and maps fields to published vocabulary terms from:

- DCMI Metadata Terms
- PROV-O
- XML Schema Definition Language
- SKOS vocabulary prefixes for downstream mapping workflows

The context maps selected Atom fields as follows:

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

This context does not itself mint RDF subject identifiers. Atom-level `@id` values should be assigned by the export workflow.

## Related resources

- Context file: [https://w3id.org/coel/utilities/jsonld/context.jsonld](https://w3id.org/coel/utilities/jsonld/context.jsonld)
- COEL Behavioural Atom v2.0: [https://w3id.org/coel/atom/2.0/](https://w3id.org/coel/atom/2.0/)
- COEL mapping resources: [https://w3id.org/coel/mapping/](https://w3id.org/coel/mapping/)
- Repository root: [https://w3id.org/coel/](https://w3id.org/coel/)
