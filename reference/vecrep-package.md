# vecrep: Replication of Numeric Vectors Via ALTREP

Replicates numeric vectors using R's ALTREP framework, avoiding
unnecessary memory allocation. When a numeric vector is repeated many
times, only a reference to the original data is stored rather than
copying the full expanded sequence into memory. The expanded data is
only materialised if it is modified, making repeated vectors cheap to
create and pass around. This is particularly useful when working with
large repeated sequences, such as replicated index vectors, simulation
inputs, or repeated reference values in data pipelines.

## See also

Useful links:

- <https://pkg.mitchelloharawild.com/vecrep/>

- <https://github.com/mitchelloharawild/vecrep>

- Report bugs at <https://github.com/mitchelloharawild/vecrep/issues>

## Author

**Maintainer**: Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
([ORCID](https://orcid.org/0000-0001-6729-7695))

Authors:

- Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
  ([ORCID](https://orcid.org/0000-0001-6729-7695))
