# COEL Behavioural Atom v2.0

Canonical IRI: `https://w3id.org/coel/atom/2.0/`

This folder contains the public **normative resources** for COEL Behavioural Atom v2.0, a versioned JSON-based structure for representing behavioural events with temporal, contextual, provenance, and optional extension information.

COEL Behavioural Atom v2.0 is being prepared as part of the published artefact set for a future COEL v2.0 manuscript.

## Purpose

The COEL Behavioural Atom v2.0 provides a compact, self-describing representation of a behavioural event relating to a participant or environment in time. It is designed to support interoperable storage, exchange, and validation of behavioural event data.

The normative exchange format is JSON.

## Included artefacts

- `coel-atom.json`  
  JSON Schema for COEL Behavioural Atom v2.0.

- `specification.pdf`  
  Full COEL Behavioural Atom v2.0 specification document.

- `extension-registry.csv`  
  Example registry of extension fields supported by the normative Atom v2.0 structure.

## Summary of structure

A COEL Behavioural Atom v2.0 is a JSON object containing four required top-level elements:

- `Header`
- `When`
- `What`
- `Who`

It may also include the following optional elements:

- `How`
- `Where`
- `Context`
- `Consent`
- `Extension`

Together, these elements support representation of:

- Atom versioning and identifiers
- event timing and duration
- behavioural labels
- participant or environment identifiers
- provenance and evidence information
- location and contextual information
- consent-related metadata
- implementation-specific or registered extensions

## Notes

- An Atom must refer to a `ParticipantID`, an `EnvironmentID`, or both.
- The structure is designed to avoid direct personal identifiers.
- The `Extension` element supports additional fields declared through extension registries.
- JSON-LD and other semantic resources are maintained separately from the normative Atom v2.0 resources.

## Specification

The full technical specification, including schema, constraints, examples, and enumerated fields, is provided in:

- `specification.pdf`

## Related resources

- Repository root: `https://w3id.org/coel/`
- COEL Model v2.0: `https://w3id.org/coel/models/coel/2.0/`
- COEL Web Application: `https://w3id.org/coel/app/`
- JSON-LD projection resources: `https://w3id.org/coel/utilities/jsonld/`