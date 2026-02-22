# Changelog

All notable changes to `typcas` are documented in this file.

Historical v1 changelog: <https://github.com/sihooleebd/typCAS/blob/main/archive/v1/CHANGELOG.md>.

## [0.2.0]

### Why a full refactor was needed

The previous architecture had accumulated coupling across parser/eval/display/calculus/steps, which made correctness fixes slow and risky. A full refactor was required to:

1. Make `src/truths/*` the canonical source for function behavior and identities.
2. Remove duplicated hardcoded rule ladders across engines.
3. Unify restriction/domain propagation so simplify/diff/integrate/trace all report consistent metadata.
4. Standardize step tracing and rendering so method narration reflects actual engine decisions.
5. Improve API ergonomics while keeping structured result contracts predictable.
6. Establish contributor invariants (correctness-first, deterministic output, explicit domain semantics).

### Added

1. Task-first `cas.*` surface with structured result helpers and context wrappers.
2. Unified domain/restriction pipeline with variable-domain propagation and assumption filtering.
3. Registry-driven function metadata expansion (analysis + integration hints for internal analyzers).
4. Step style system and branch-aware trace presentation controls.
5. Comprehensive docs:
   - `docs/COMPLETE_GUIDE.md`
   - `CONTRIBUTING.md`
   - `.github/pull_request_template.md`
6. Conservative function bundle expansion:
   - `sign`/`sgn`, `floor`, `ceil`, `round`, `trunc`, `fracpart`, `min`, `max`, `clamp`
7. Conservative identity bundle expansion:
   - `log1p(expm1(u))`, `expm1(log1p(u))` (domain-sensitive)
   - `(cbrt(u))^3`, `(sqrt(u))^2` (domain-sensitive)
   - idempotence for `min/max/clamp`, plus sign/abs normalization identities

### Changed

1. Parser/evaluator/display/calculus paths were refactored to reduce truth bypasses.
2. Integration and tracing now share method-analysis decisions for better narrative fidelity.
3. Bare `C` handling was unified as the integration constant policy.
4. Restriction sign reasoning moved to truth-driven metadata where applicable.
5. Example/testing flow consolidated around `examples/test.typ`.
6. Parser now enforces known-function arity from registry metadata at parse time, including variadic minimum arity (`min/max >= 2`).

### Fixed

1. Multiple step-trace formatting and narration mismatches (chain form, u-sub narration, branch readability).
2. Inconsistent restriction reporting between operations and traces.
3. Several simplification and presentation regressions discovered during unification.

### Migration notes

1. Prefer canonical task-first calls under `cas.*`.
2. Translation helpers remain available under `translators/translation.typ` for v1-style migration.
3. Use `c` or `C_0` if a normal variable name is needed instead of bare `C`.
4. The historical codebase is preserved under `archive/v1` (with its own `archive/v1/typst.toml` and `archive/v1/lib.typ` entrypoint).
