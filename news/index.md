# Changelog

## vecrep 0.1.0

CRAN release: 2026-06-18

- [`rep_altrep()`](../reference/rep_altrep.md) replicates vectors using
  R’s ALTREP framework, storing only a reference to the original data
  rather than copying full expanded replicates into memory.
- Supports double, integer, logical, complex, raw, character, and list
  vectors (list support requires R \>= 4.2).
- Preserves classes and attributes (e.g. factor, Date, POSIXct) for
  correct S3 dispatch.
