# Changelog (typcas v1)

All notable changes to archived `typcas` v1 are documented in this file.

This changelog is historical and maintained for reference while active development continues in v2.

## [0.1.0] - Archived Baseline

### Summary

Initial archived v1 release with function-style API surface and full symbolic CAS feature set used by the legacy examples/regression suite.

### Added

1. Function-first public API exposed from `archive/v1/lib.typ`:
   - `simplify`, `simplify-meta`, `expand`
   - `diff`, `diff-n`, `implicit-diff`
   - `integrate`, `definite-integral`
   - `solve`, `solve-meta`, `factor`
   - `taylor`, `limit`
   - `eval-expr`, `substitute`
2. Assumption and domain utilities:
   - `assume`, `assume-domain`, `assume-string`, `merge-assumptions`, `apply-assumptions`
   - `parse-domain`, `domain-normalize`, `domain-intersect`, `domain-contains`, `domain-status-rel`
3. Restriction metadata pipeline:
   - structural/function restriction collectors
   - filter/classification by assumptions
   - metadata surfaces such as `diff-meta` and `simplify-meta`
4. Step-by-step subsystem:
   - `step-simplify`, `step-diff`, `step-integrate`, `step-solve`
   - `display-steps`
5. Solver and algebra tools:
   - polynomial helpers: `poly-coeffs`, `coeffs-to-expr`, `poly-div`, `poly-gcd`, `partial-fractions`
   - systems: linear and nonlinear system solvers
   - matrix operations: arithmetic, determinant, inverse, solve, eigenvalues/eigenvectors
6. Legacy example/regression suite under `archive/v1/examples`.

### Notes

1. v1 is preserved in `archive/v1` as a stable historical reference.
2. Active development, API ergonomics, and architecture unification continue in v2 (`src/` + `lib.typ`).
