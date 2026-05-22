# w3id Redirect Summary

This page summarises the live public w3id.org routes for the COEL v2.0 artefact bundle.

The live GitHub Pages documentation base URL is:

```text
https://miltheo.github.io/coel/
```

## Route Policy

- Namespace and collection/document paths resolve to GitHub Pages HTML pages in `docs/`.
- Concrete schema, registry, mapping, and derived TTL artefact paths resolve to raw GitHub file URLs.
- Model term fragment IRIs resolve through the base model path because URL fragments are handled by clients and are not sent to the server.
- JSON-LD routes are optional projection support, not the core COEL v2.0 identity.
- Non-public implementation routes, slug aliases, and derived JSON serialisation routes are intentionally excluded.

## Route Configuration

The live w3id redirect configuration is maintained in `w3id/coel/.htaccess`.
