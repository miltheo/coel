# Name dictionary snapshot

The two CSV files in this directory are a fixed snapshot of the public `popular-names-by-country-dataset` used by the historical CQ3 validator:

- `common-forenames-by-country.csv`
- `common-surnames-by-country.csv`

Upstream: <https://github.com/sigpwned/popular-names-by-country-dataset>, release v1.2, commit `eb62e13`.

The upstream dataset is released under CC0. Vendoring the snapshot prevents CQ3 results from changing when the upstream default branch changes and removes network access from the replication run.

The neighbouring `../cq3_charlatan_names.csv` file freezes the 50-name output generated for the historical experiment with `charlatan` seed 20251229. Keeping this fixture is necessary because locale providers can change between package versions.
