# Draft w3id Redirects

This note records the intended `https://w3id.org/coel/` redirect pattern for the COEL v2.0 public release.

Use temporary `302` redirects while testing. After the GitHub Pages deployment and raw artefact targets are confirmed, decide whether each route should remain temporary or become a long-lived redirect.

## Target Pattern

- Namespace and collection paths resolve to GitHub Pages HTML pages in `docs/`.
- Concrete artefact paths resolve to raw GitHub file URLs.
- Model term fragment IRIs resolve through the base registry target because URL fragments are handled by clients and are not sent to the server.
- The local `app/` workspace is outside the public COEL v2.0 namespace.

The repository remote is `https://github.com/miltheo/coel.git` on branch `main`. If GitHub Pages is enabled from `docs/` on `main`, the expected Pages base is:

```text
https://miltheo.github.io/coel/
```

Confirm this URL in repository Pages settings before submitting final w3id changes.

## Draft `.htaccess` Snippet

```apache
Options +FollowSymLinks
RewriteEngine On

# HTML landing pages. Keep as 302 until GitHub Pages is confirmed.
RewriteRule ^$ https://miltheo.github.io/coel/ [R=302,L]
RewriteRule ^atom/2\.0/?$ https://miltheo.github.io/coel/atom/2.0/ [R=302,L]
RewriteRule ^models/?$ https://miltheo.github.io/coel/models/ [R=302,L]
RewriteRule ^models/coel/2\.0/?$ https://miltheo.github.io/coel/models/coel/2.0/ [R=302,L]
RewriteRule ^models/activinsights/?$ https://miltheo.github.io/coel/models/activinsights/ [R=302,L]
RewriteRule ^models/activinsights/behavioural_bout/1\.0/?$ https://miltheo.github.io/coel/models/activinsights/behavioural_bout/1.0/ [R=302,L]
RewriteRule ^models/activinsights/rest_activity/1\.0/?$ https://miltheo.github.io/coel/models/activinsights/rest_activity/1.0/ [R=302,L]
RewriteRule ^mapping/?$ https://miltheo.github.io/coel/mapping/ [R=302,L]
RewriteRule ^utilities/?$ https://miltheo.github.io/coel/utilities/ [R=302,L]
RewriteRule ^utilities/jsonld/?$ https://miltheo.github.io/coel/utilities/jsonld/ [R=302,L]

# Normative Atom artefacts.
RewriteRule ^atom/2\.0/coel-atom\.json$ https://raw.githubusercontent.com/miltheo/coel/main/atom/2.0/coel-atom.json [R=302,L]
RewriteRule ^atom/2\.0/extension-registry\.csv$ https://raw.githubusercontent.com/miltheo/coel/main/atom/2.0/extension-registry.csv [R=302,L]

# Canonical model registry CSV artefacts.
RewriteRule ^models/coel/2\.0/coel-model-v2\.0\.csv$ https://raw.githubusercontent.com/miltheo/coel/main/models/coel/2.0/coel-model-v2.0.csv [R=302,L]
RewriteRule ^models/activinsights/behavioural_bout/1\.0/behavioural-bout-model-v1\.0\.csv$ https://raw.githubusercontent.com/miltheo/coel/main/models/activinsights/behavioural_bout/1.0/behavioural-bout-model-v1.0.csv [R=302,L]
RewriteRule ^models/activinsights/rest_activity/1\.0/rest-activity-model-v1\.0\.csv$ https://raw.githubusercontent.com/miltheo/coel/main/models/activinsights/rest_activity/1.0/rest-activity-model-v1.0.csv [R=302,L]

# Mapping CSV artefacts.
RewriteRule ^mapping/behavioural-bout-model-v1\.0-to-coel-model-v2\.0\.csv$ https://raw.githubusercontent.com/miltheo/coel/main/mapping/behavioural-bout-model-v1.0-to-coel-model-v2.0.csv [R=302,L]
RewriteRule ^mapping/rest-activity-model-v1\.0-to-coel-model-v2\.0\.csv$ https://raw.githubusercontent.com/miltheo/coel/main/mapping/rest-activity-model-v1.0-to-coel-model-v2.0.csv [R=302,L]

# JSON-LD projection artefact.
RewriteRule ^utilities/jsonld/context\.jsonld$ https://raw.githubusercontent.com/miltheo/coel/main/utilities/jsonld/context.jsonld [R=302,L]

# Optional pre-release aliases for known slug mistakes. Leave commented unless testing requires them.
# RewriteRule ^models/activinsights/behaviour_bout/1\.0/?$ https://w3id.org/coel/models/activinsights/behavioural_bout/1.0/ [R=302,L]
# RewriteRule ^models/activinsights/behavioural-bout/1\.0/?$ https://w3id.org/coel/models/activinsights/behavioural_bout/1.0/ [R=302,L]
```

## Operational Checks

- Enable GitHub Pages from `docs/` and confirm each HTML target resolves.
- Confirm each raw target returns the expected content type and current file content.
- Test fragment IRIs against the model pages and CSV registry targets.
- Remove or explicitly approve any pre-release aliases before permanent deployment.
